# REFERENCE ONLY: example of a live train map. Not your app; use it as a guide.
# Assumes the feed has latitude/longitude columns (unverified; check names(trains)).

library(shiny)
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

# Station dictionary: names match the real-time feed (park & ride rows are dropped by the STATION filter)
stations <- read_csv("assets/input/google_transit/MARTA_stops.csv", show_col_types = FALSE) |>
  filter(str_detect(stop_name, "STATION")) |>
  select(station = stop_name, station_lat = stop_lat, station_lon = stop_lon, is_endpoint) |>
  mutate(station = str_to_title(station))   # match the title-cased feed

trains_per_direction <- 3   # how many upcoming trains each direction shows on a station board

# Compare feed destinations to station names ("Hamilton E. Holmes" vs "Hamilton E Holmes Station")
norm_name <- function(x) x |> str_to_lower() |> str_remove(" station$") |> str_remove_all("[^a-z0-9]")

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

# Station board styling; em units keep every size relative to the popup's own text size
board_css <- "
.board-title { font-size: 1.4em; margin: 0 0 0.5em 0; }
.board-group { margin-bottom: 0.75em; }
.board-line  { font-weight: bold; }
.board-dir   { font-style: italic; padding-left: 0.75em; }
.board-train { padding-left: 1.75em; }
.board-arriving { font-weight: bold; color: #2e7d32; }
"

ui <- fluidPage(
  tags$head(tags$script(HTML(update_boards_js)), tags$style(HTML(board_css))),
  leafletOutput("map", height = "100vh")
)

server <- function(input, output, session) {
  trains <- reactive({
    invalidateLater(REFRESH_SECS * 1000)
    get_trains()
  })
  boards <- reactive(build_station_boards(trains()))

  # Draw the base map and the stations once; the stations never get redrawn
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

  # Each refresh: replace only the train arrows
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

  # Each refresh: push the new board text into the station popups
  observe({
    popups <- station_popups(boards())
    session$sendCustomMessage("update_boards", as.list(setNames(popups$popup, popups$station)))
  })
}

shinyApp(ui, server)
