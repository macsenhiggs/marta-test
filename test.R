# Static (non-Shiny) equivalent of the data steps in reference_app.R: one request, run top to bottom.
# The functions below are copies of the ones in reference_app.R; keep them in sync if either changes.

library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)

# --- Data request + mutations (get_trains) ---
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

# --- Station dictionary ---
# Names match the real-time feed (park & ride rows are dropped by the STATION filter)
stations <- read_csv("assets/input/google_transit/MARTA_stops.csv", show_col_types = FALSE) |>
  filter(str_detect(stop_name, "STATION")) |>
  select(station = stop_name, station_lat = stop_lat, station_lon = stop_lon, is_endpoint) |>
  mutate(station = str_to_title(station))   # match the title-cased feed

trains_per_direction <- 3   # how many upcoming trains each direction shows on a station board

# Compare feed destinations to station names ("Hamilton E. Holmes" vs "Hamilton E Holmes Station")
norm_name <- function(x) x |> str_to_lower() |> str_remove(" station$") |> str_remove_all("[^a-z0-9]")

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
  scheduled <- if_else(arrivals$is_realtime, "", " (scheduled)")   # not yet tracked live
  train <- paste0(wait, scheduled)
  train_class <- if_else(coalesce(arrivals$waiting_minutes == 0, FALSE), "board-train board-arriving", "board-train")

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

# Every station with its current board (stations with no trains get a placeholder)
station_popups <- function(boards) {
  stations |>
    left_join(select(boards, station, popup), by = "station") |>
    mutate(popup = coalesce(popup, paste0("<b>", str_to_title(station), "</b><br/>No upcoming trains")))
}

# --- Run once ---
trains <- get_trains()
head(trains)

# Station boards: what the map's station markers show on click
boards <- build_station_boards(trains)
popups <- station_popups(boards)
boards
boards$arrivals[[1]]            # one station's board as a data frame
cat(boards$popup[[1]], "\n")    # the same board as popup HTML

# Train markers: the soonest row per live train (position, arrow direction, hover label)
train_markers <- trains |>
  filter(is_realtime, !is.na(latitude), !is.na(longitude), direction %in% c("N", "E", "S", "W")) |>
  arrange(waiting_seconds) |>
  distinct(train_id, .keep_all = TRUE) |>
  mutate(label = paste0(
    line, " to ", destination, "<br/>",
    "Next stop: ", station, "<br/>",
    waiting_note(waiting_minutes, delay_minutes)
  ))
train_markers |> select(train_id, line, direction, latitude, longitude, label)
