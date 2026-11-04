# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(shiny)
library(DBI)
library(leaflet)
library(leaflet.extras)   # for addResetMapButton()
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(sf)
library(lubridate)

# Built by build_assets.R
rail_lines <- read_sf("assets/output/rail_lines.shp")

REFRESH_SECS <- 5



# ------------------------------------------------------------------------------
# Live train feed
# ------------------------------------------------------------------------------

get_trains <- function() {
  response <- GET(
    "https://developerservices.itsmarta.com/mcsmrail/itsmarta/railrealtimearrivals/developerservices/traindata",
    query = list(apiKey = Sys.getenv("MARTA_API_KEY")),
    accept_json()   # the API now defaults to XML unless JSON is requested
  )
  stop_for_status(response)
  fromJSON(content(response, as = "text", encoding = "UTF-8")) |>
    clean_names() |>
    mutate(
      delay = parse_number(delay),
      is_realtime = as.logical(is_realtime),      # must come before the title-casing below
      across(where(is.character), str_to_title),  # remaining text columns
      waiting_minutes = if_else(
        waiting_time %in% c("Arriving", "Now"), 0L,
        as.integer(str_extract(waiting_time, "\\d+"))
      ),
      delay_minutes = floor(delay / 60),
      waiting_seconds = as.numeric(waiting_seconds),   # feed sends text; without this it sorts alphabetically
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
}



# ------------------------------------------------------------------------------
# Stations
# ------------------------------------------------------------------------------

# Station dictionary: names match the real-time feed (park & ride rows are dropped by the STATION filter)
stations <- read_csv("assets/input/google_transit/MARTA_stops.csv", show_col_types = FALSE) |>
  filter(str_detect(stop_name, "STATION")) |>
  select(station = stop_name, station_lat = stop_lat, station_lon = stop_lon, is_endpoint) |>
  mutate(station = str_to_title(station))   # match the title-cased feed

trains_per_direction <- 3   # how many upcoming trains each direction shows on a station board

# Compare feed destinations to station names ("Hamilton E. Holmes" vs "Hamilton E Holmes Station")
norm_name <- function(x) x |> str_to_lower() |> str_remove(" station$") |> str_remove_all("[^a-z0-9]")



# ------------------------------------------------------------------------------
# Station boards
# ------------------------------------------------------------------------------

# One row per station, with its upcoming trains nested in `arrivals` and the board as popup HTML
build_station_boards <- function(trains) {
  trains |>
    left_join(stations, by = "station") |>
    # Drop trains terminating here: they just turn around, so they're redundant on the board
    filter(norm_name(destination) != norm_name(station)) |>
    group_by(station, direction) |>
    arrange(waiting_seconds, .by_group = TRUE) |>
    slice_head(n = trains_per_direction) |>
    ungroup() |>
    # Board order, like a platform: line, then direction (N, E, S, W), then soonest first
    arrange(station, line, factor(direction, levels = c("N", "E", "S", "W")), waiting_seconds) |>
    select(station, station_lat, station_lon, is_endpoint, direction, line, destination,
           is_realtime, waiting_minutes, delay_minutes) |>
    nest(arrivals = c(direction, line, destination, is_realtime, waiting_minutes, delay_minutes)) |>
    mutate(popup = map2_chr(station, arrivals, board_html))
}


# Note appended to a train's arrival time; empty when the train is on time or has no delay figure
waiting_note <- function(waiting_minutes, delay_minutes) {
  case_when(
    waiting_minutes == 0 ~ "▶ Train Arriving",
    is.na(delay_minutes) | delay_minutes == 0 ~ paste0(waiting_minutes, " min away"),
    delay_minutes < 0                         ~ paste0(waiting_minutes, " min away (early ", abs(delay_minutes), " min)"),
    TRUE                                      ~ paste0(waiting_minutes, " min away (delayed ", delay_minutes, " min)")
  )
}


# What the station "board" shows, as the HTML for a click popup
board_html <- function(station, arrivals) {
  wait <- waiting_note(arrivals$waiting_minutes, arrivals$delay_minutes)
  # e.g. "3 min away - delayed 0 min", or "15 min away (scheduled)"
  scheduled <- if_else(arrivals$is_realtime, "", " (scheduled)")   # not yet tracked live
  train <- paste0(wait, scheduled)
  train_class <- if_else(coalesce(arrivals$waiting_minutes == 0, FALSE), "board-train board-arriving", "board-train")
  # train <- paste0(arrivals$direction, " - ", arrivals$destination, " - ", arrivals$waiting_minutes, " min away", delay, scheduled)   # alt: "E - Indian Creek - 2 min away"

  # Layout lives in CSS (board_css), sized in em so it scales with the popup's font size
  # Each line has one destination per direction, so it's shown once in the direction heading
  blocks <- map_chr(unique(arrivals$line), \(l) {
    on_line <- which(arrivals$line == l)
    dir_blocks <- map_chr(unique(arrivals$direction[on_line]), \(d) {
      rows <- on_line[arrivals$direction[on_line] == d]
      paste0("<div class='board-dir'>", d, " - ", arrivals$destination[rows[1]], "</div>",
             paste0("<div class='", train_class[rows], "'>", train[rows], "</div>", collapse = ""))
    })
    paste0("<div class='board-group'><div class='board-line'>", l, " Line</div>",
           paste(dir_blocks, collapse = ""), "</div>")
  })
  paste0("<h3 class='board-title'>", str_to_title(station), "</h3>", paste(blocks, collapse = ""))
}


# Every station with its current board (stations with no trains get a placeholder)
station_popups <- function(boards) {
  stations |>
    left_join(select(boards, station, popup), by = "station") |>
    mutate(popup = coalesce(popup, paste0("<b>", str_to_title(station), "</b><br/>No upcoming trains")))
}



# ------------------------------------------------------------------------------
# Map assets: train arrows and the in-place popup updater
# ------------------------------------------------------------------------------

# Serve the rotated arrows to the browser, one icon per direction
addResourcePath("arrows", "assets/output/arrows")
arrow_size <- 16   # pixels; was 24
arrow_icons <- iconList(
  N = makeIcon("arrows/arrow_N.svg", iconWidth = arrow_size, iconHeight = arrow_size),
  E = makeIcon("arrows/arrow_E.svg", iconWidth = arrow_size, iconHeight = arrow_size),
  S = makeIcon("arrows/arrow_S.svg", iconWidth = arrow_size, iconHeight = arrow_size),
  W = makeIcon("arrows/arrow_W.svg", iconWidth = arrow_size, iconHeight = arrow_size)
)


# Updates the content of each station's popup in place, so an open board refreshes without closing
update_boards_js <- "
Shiny.addCustomMessageHandler('update_boards', function(msg) {
  var widget = HTMLWidgets.find('#map');
  if (!widget) return;
  var map = widget.getMap();
  Object.keys(msg).forEach(function(id) {
    var layer = map.layerManager.getLayer('marker', id);
    if (layer) layer.setPopupContent(msg[id]);
  });
});
"



# ------------------------------------------------------------------------------
# Analytics: summaries of the polls the collector stores in the Neon database
# ------------------------------------------------------------------------------

# Connection to the Neon database the collector writes to (RPostgres doesn't take a URL, so split it)
db_connect <- function() {
  u <- httr::parse_url(Sys.getenv("DATABASE_URL"))
  dbConnect(
    RPostgres::Postgres(),
    host = u$hostname, port = u$port %||% 5432, dbname = u$path,
    user = u$username, password = u$password, sslmode = "require"
  )
}


# Time filter choices: label shown in the app = value used ("all" = no filter, otherwise a Postgres interval)
gap_windows <- c("All time" = "all", "Last 30 days" = "30 days", "Last 7 days" = "7 days", "Last 24 hours" = "24 hours")


# SQL condition limiting `col` to one of gap_windows ("true" when unfiltered)
time_condition <- function(window, col) {
  window <- match.arg(window, gap_windows)   # only these intervals are ever pasted into the SQL
  if (window == "all") "true" else sprintf("%s >= now() - interval '%s'", col, window)
}


# Average time between consecutive trains, grouped by "line" or "station", over one of gap_windows.
# Within each poll, a station's upcoming trains (per line and direction) are sorted by wait time and
# the gap to the previous train is taken; those gaps are then averaged.
avg_gap_by <- function(group_by = c("line", "station"), window = "all") {
  group_by <- match.arg(group_by)        # only these names are ever pasted into the SQL
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sprintf("
    with gaps as (
      select p.line, a.station,
             a.waiting_seconds - lag(a.waiting_seconds) over (
               partition by a.collected_at, a.station, p.line, p.direction
               order by a.waiting_seconds) as gap_seconds
      from train_arrivals a
      join train_polls p using (collected_at, train_id)
      where %s
    )
    select %s as grp, (avg(gap_seconds) / 60.0)::float8 as avg_gap_minutes, count(gap_seconds) as n
    from gaps
    where gap_seconds is not null
    group by 1
    order by 2", time_condition(window, "a.collected_at"), group_by))
}


# Average delay in minutes (negative = early), grouped by "line" or "station", over one of gap_windows.
# By line, each train counts once per poll; by station, each train counts once for every station it is
# heading to, so a station's figure is the delay of the trains arriving there.
avg_delay_by <- function(group_by = c("line", "station"), window = "all") {
  group_by <- match.arg(group_by)
  con <- db_connect()
  on.exit(dbDisconnect(con))
  sql <- if (group_by == "line") {
    sprintf("
      select p.line as grp, (avg(p.delay_seconds) / 60.0)::float8 as avg_delay_minutes, count(p.delay_seconds) as n
      from train_polls p
      where p.delay_seconds is not null and %s
      group by 1
      order by 2", time_condition(window, "p.collected_at"))
  } else {
    sprintf("
      select a.station as grp, (avg(p.delay_seconds) / 60.0)::float8 as avg_delay_minutes, count(p.delay_seconds) as n
      from train_arrivals a
      join train_polls p using (collected_at, train_id)
      where p.delay_seconds is not null and %s
      group by 1
      order by 2", time_condition(window, "a.collected_at"))
  }
  dbGetQuery(con, sql)
}


# Standard line colours for the bar chart
line_colors <- c(Red = "#d32f2f", Gold = "#c9a227", Green = "#2e7d32", Blue = "#1565c0")


# Bar chart of `value` (a column name) by `grp`, shared by every analytics chart.
# By line: vertical bars in the line colours. By station: horizontal bars in `station_fill`.
summary_bar_chart <- function(df, value, group_by, y_label, station_fill) {
  df <- df |> mutate(grp = str_remove(grp, " Station$"), grp = fct_reorder(grp, .data[[value]]))
  base <- labs(x = NULL, y = y_label)
  if (group_by == "line") {
    ggplot(df, aes(grp, .data[[value]], fill = grp)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = line_colors) +
      base + theme_minimal(base_size = 14)
  } else {
    # Many stations, so horizontal bars (and a taller plot, see chart_height)
    ggplot(df, aes(grp, .data[[value]])) +
      geom_col(fill = station_fill) +
      coord_flip() +
      base + theme_minimal(base_size = 14)
  }
}


# Plot height in pixels: stations need much more room than the four lines
chart_height <- function(group_by) if (identical(group_by, "station")) 900 else 350



# ------------------------------------------------------------------------------
# Styling
# ------------------------------------------------------------------------------

# Station board styling; em units keep every size relative to the popup's own text size
board_css <- "
.board-title { font-size: 1.4em; margin: 0 0 0.5em 0; }
.board-group { margin-bottom: 0.75em; }
.board-line  { font-weight: bold; }
.board-dir   { font-style: italic; padding-left: 0.75em; }
.board-train { padding-left: 1.75em; }
.board-arriving { font-weight: bold; color: #2e7d32; }
"


# Page layout styling: the map sits in a bordered card, with the analytics section below it
page_css <- "
.map-card { border: 1px solid #d0d0d0; border-radius: 0.5em; overflow: hidden; }
.analytics-card { border: 1px solid #d0d0d0; border-radius: 0.5em; padding: 1.5em; margin: 1em 0; }
"



# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$script(HTML(update_boards_js)),
    tags$style(HTML(board_css)),
    tags$style(HTML(page_css))
  ),

  titlePanel(
    title = "MARTA Live Rail Map",
    windowTitle = "MARTA Live Rail Map"
  ),
  p("Live train positions and station arrival boards, refreshed every few seconds. Click a station for its board."),

  div(class = "map-card", leafletOutput("map", height = "70vh")),

  div(
    class = "analytics-card",
    h4("Train performance"),
    # Shared by both charts
    fluidRow(
      column(3, selectInput("chart_group", "Group by", choices = c("Line" = "line", "Station" = "station"), width = "100%")),
      column(3, selectInput("chart_window", "Time period", choices = gap_windows, selected = "all", width = "100%"))
    ),
    fluidRow(
      column(6, h5("Average time between trains"), plotOutput("gap_plot", height = "auto")),
      column(6, h5("Average delay"), plotOutput("delay_plot", height = "auto"))
    )
  )
)



# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Live data: refetched every REFRESH_SECS ---
  trains <- reactive({
    invalidateLater(REFRESH_SECS * 1000)
    get_trains()
  })
  boards <- reactive(build_station_boards(trains()))


  # --- Map: base layers and stations, drawn once (stations never get redrawn) ---
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.1, wheelPxPerZoomLevel = 60)) |>   # allow fractional zoom so fitBounds can fit tightly
      addProviderTiles(providers$Stadia.AlidadeSmooth) |>
      addPolylines(data = rail_lines, color = ~color, weight = 4, opacity = 0.5) |>
      addCircleMarkers(
        data = station_popups(isolate(boards())), lng = ~station_lon, lat = ~station_lat,
        layerId = ~station, group = "stations",
        radius = ~if_else(is_endpoint, 9, 6),   # termini are 50% larger
        weight = 2, color = "#333333", fillColor = "white", fillOpacity = 1,
        popup = ~popup
      ) |>
      # Start zoomed to fit every station, with light padding (in pixels)
      fitBounds(
        lng1 = min(stations$station_lon), lat1 = min(stations$station_lat),
        lng2 = max(stations$station_lon), lat2 = max(stations$station_lat),
        options = list(padding = c(40, 40))
      ) |>
      # Button under +/- that refits the map to all stations (same bounds and padding as above)
      # addEasyButton(easyButton(
      #   icon = "<strong>&#8634;</strong>", title = "Reset view",
      #   onClick = JS(sprintf(
      #     "function(btn, map) { map.fitBounds([[%f, %f], [%f, %f]], {padding: [40, 40]}); }",
      #     min(stations$station_lat), min(stations$station_lon),
      #     max(stations$station_lat), max(stations$station_lon)
      #   ))
      # )) |>
      addResetMapButton()   # button under +/- that returns to the initial view
  })


  # --- Map: train arrows, replaced each refresh ---
  observe({
    # Update Console message
    time <- round_date(Sys.time(), unit = "second")
    cat(paste0("\rLast Refereshed: ", time))
    flush.console()

    # Feed has one row per upcoming station per train; keep the soonest row per train
    df <- trains() |>
      filter(is_realtime, !is.na(latitude), !is.na(longitude), direction %in% names(arrow_icons)) |>
      arrange(waiting_seconds) |>
      distinct(train_id, .keep_all = TRUE)
    leafletProxy("map", data = df) |>
      clearGroup("trains") |>
      addMarkers(
        lng = ~longitude, lat = ~latitude, group = "trains",
        icon = ~arrow_icons[direction],
        label = ~lapply(
          paste0(
            line, " to ", destination, "<br/>",
            "Next stop: ", station, "<br/>",
            # Replace with function to handle "arriving" as well?
            waiting_note(waiting_minutes, delay_minutes)
          ),
          htmltools::HTML
        )
      )
  })


  # --- Map: station boards, pushed into the popups each refresh ---
  observe({
    popups <- station_popups(boards())
    session$sendCustomMessage("update_boards", as.list(setNames(popups$popup, popups$station)))
  })


  # --- Analytics: both charts follow the shared "Group by" and "Time period" menus ---
  gap_data <- reactive(avg_gap_by(input$chart_group, input$chart_window))
  delay_data <- reactive(avg_delay_by(input$chart_group, input$chart_window))

  output$gap_plot <- renderPlot(
    {
      df <- gap_data()
      shiny::validate(need(nrow(df) > 0, "No stored train data in this time period."))   # jsonlite also has a validate()
      summary_bar_chart(df, "avg_gap_minutes", input$chart_group, "Average minutes between trains", "#4a6fa5")
    },
    height = function() chart_height(input$chart_group)
  )

  output$delay_plot <- renderPlot(
    {
      df <- delay_data()
      shiny::validate(need(nrow(df) > 0, "No stored train data in this time period."))
      summary_bar_chart(df, "avg_delay_minutes", input$chart_group, "Average delay (minutes; negative = early)", "#b5651d")
    },
    height = function() chart_height(input$chart_group)
  )
}



# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------

shinyApp(ui, server)
