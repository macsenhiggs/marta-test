# Run once (or when the GTFS files or arrow SVG change) to build everything in assets/output.
# The app then just loads those files instead of rebuilding on every run.

library(tidyverse)
library(sf)

gtfs <- "assets/input/google_transit"

routes <- read_csv(file.path(gtfs, "routes.txt"), show_col_types = FALSE) |>
  filter(route_type == 1) |>   # 1 = subway/metro (MARTA rail)
  select(route_id, line = route_short_name, color = route_color)

shape_ids <- read_csv(file.path(gtfs, "trips.txt"), show_col_types = FALSE) |>
  inner_join(routes, by = "route_id") |>
  distinct(route_id, line, color, shape_id)

shapes <- read_csv(file.path(gtfs, "shapes.txt"), show_col_types = FALSE) |>
  semi_join(shape_ids, by = "shape_id") |>
  arrange(shape_id, shape_pt_sequence)

rail_lines <- shapes |>
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
  group_by(shape_id) |>
  summarise(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING") |>
  left_join(shape_ids, by = "shape_id") |>
  mutate(color = paste0("#", color))

dir.create("assets", showWarnings = FALSE)
saveRDS(rail_lines, "assets/output/rail_lines.rds")

# Shapefile copy (for QGIS/ArcGIS); keep the .shp/.shx/.dbf/.prj files together
st_write(rail_lines, "assets/output/rail_lines.shp", delete_layer = TRUE)

# --- Direction arrows ---
# Pick which source arrow to use: change the number to try assets/input/arrows/arrow<N>.svg
arrow_num <- 4
# Source arrows must point north (up)

arrow_src <- readLines(file.path("assets/input/arrows", paste0("arrow", arrow_num, ".svg")), warn = FALSE) |>
  paste(collapse = "")
view_box <- str_match(arrow_src, 'viewBox="([^"]+)"')[, 2]
vb <- as.numeric(str_split_1(view_box, " "))
cx <- vb[1] + vb[3] / 2
cy <- vb[2] + vb[4] / 2
arrow_inner <- arrow_src |>
  str_remove("^.*?<svg[^>]*>") |>
  str_remove("</svg>\\s*$")
# Attributes on the root <svg> (fill, stroke, etc.) are inherited by its children, so keep them on the new root
root_attrs <- str_match(arrow_src, "<svg([^>]*)>")[, 2] |>
  str_remove_all('\\s(xmlns|width|height|viewBox|class)="[^"]*"')

dir.create("assets/output/arrows", showWarnings = FALSE)
walk2(c(N = 0, E = 90, S = 180, W = 270), c("N", "E", "S", "W"), \(deg, dir) {
  svg <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="%s"%s><g transform="rotate(%s %s %s)">%s</g></svg>',
    view_box, root_attrs, deg, cx, cy, arrow_inner
  )
  writeLines(svg, file.path("assets/output/arrows", paste0("arrow_", dir, ".svg")))
})

# --- Footnote (optional, not used by the app): split-circle station icons ---
# Tried as station markers and dropped as too cluttered; kept in case they're wanted later.
# Station lines: which rail lines serve each station, then one icon per combination of lines.
# Rail trips -> the stops they call at -> each stop's parent station -> the station names the app uses
rail_trips <- read_csv(file.path(gtfs, "trips.txt"), show_col_types = FALSE) |>
  inner_join(routes, by = "route_id") |>
  distinct(trip_id, line, color)

stop_parents <- read_csv(file.path(gtfs, "stops.txt"), col_select = c(stop_id, parent_station), show_col_types = FALSE) |>
  mutate(parent_station = coalesce(parent_station, stop_id))   # a stop with no parent is its own station

station_names <- read_csv(file.path(gtfs, "MARTA_stops.csv"), show_col_types = FALSE) |>
  filter(str_detect(stop_name, "STATION")) |>
  transmute(parent_station, station = str_to_title(stop_name))   # same casing as the app

station_lines <- read_csv(file.path(gtfs, "stop_times.txt"), col_select = c(trip_id, stop_id), show_col_types = FALSE) |>
  inner_join(rail_trips, by = "trip_id") |>
  distinct(stop_id, line, color) |>
  left_join(stop_parents, by = "stop_id") |>
  inner_join(station_names, by = "parent_station") |>
  distinct(station, line, color) |>
  arrange(station, line) |>
  mutate(color = paste0("#", color))


# One row per station: the lines serving it and which marker icon to use.
# Stations with the same set of lines share an icon (7 combinations).
station_icons <- station_lines |>
  group_by(station) |>
  summarise(lines = paste(line, collapse = "/"), colors = list(color), .groups = "drop") |>
  mutate(icon = paste0("station_", str_to_lower(str_replace_all(lines, "/", "-")), ".svg"))

# A circle split into equal pie slices, one per line color, starting at 12 o'clock and going clockwise
station_svg <- function(colors) {
  n <- length(colors)
  r <- 11   # radius inside the 24x24 viewBox; the outline stroke fills out to the edge
  point <- function(angle) c(12 + r * sin(angle), 12 - r * cos(angle))
  slices <- if (n == 1) {
    sprintf('<circle cx="12" cy="12" r="%s" fill="%s"/>', r, colors)
  } else {
    map_chr(seq_len(n), \(i) {
      from <- point((i - 1) * 2 * pi / n)
      to   <- point(i * 2 * pi / n)
      sprintf('<path d="M12 12 L%.3f %.3f A%s %s 0 0 1 %.3f %.3f Z" fill="%s" stroke="#fff" stroke-width="1"/>',
              from[1], from[2], r, r, to[1], to[2], colors[i])
    })
  }
  paste0('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24">',
         paste(slices, collapse = ""),
         '<circle cx="12" cy="12" r="11" fill="none" stroke="#333333" stroke-width="2"/></svg>')
}

dir.create("assets/output/station_icons", showWarnings = FALSE)
station_icons |>
  distinct(icon, .keep_all = TRUE) |>
  select(icon, colors) |>
  pwalk(\(icon, colors) writeLines(station_svg(colors), file.path("assets/output/station_icons", icon)))

station_icons |>
  select(station, lines, icon) |>
  write_csv("assets/output/station_icons.csv")

rm(list = ls())   # clean up the environment once everything is built
