# Collector: one poll of the MARTA feed, trimmed and appended to the Neon table `train_snapshots`.
# Run on a schedule (see .github/workflows/collect.yml); the Shiny app never writes to the database.
# Needs env vars MARTA_API_KEY and DATABASE_URL.

library(DBI)
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)

# Copy of get_trains() in reference_app.R; keep in sync if either changes
get_trains <- function() {
  response <- GET(
    "https://developerservices.itsmarta.com/mcsmrail/itsmarta/railrealtimearrivals/developerservices/traindata",
    query = list(apiKey = Sys.getenv("MARTA_API_KEY")),
    accept_json()
  )
  stop_for_status(response)
  fromJSON(content(response, as = "text", encoding = "UTF-8")) |>
    clean_names() |>
    mutate(
      delay = parse_number(delay),
      is_realtime = as.logical(is_realtime),
      across(where(is.character), str_to_title),
      waiting_seconds = as.numeric(waiting_seconds)
    )
}

# Keep what the summaries need (delay, trains run, wait time by line / station / time).
# Scheduled rows have no train_id or delay, so only live rows are stored.
snapshot <- get_trains() |>
  filter(is_realtime) |>
  transmute(
    collected_at = Sys.time(),
    line, station, direction, destination, train_id,
    waiting_seconds,
    delay_seconds = delay
  )

# RPostgres doesn't take a URL, so split DATABASE_URL into its parts
u <- httr::parse_url(Sys.getenv("DATABASE_URL"))
con <- dbConnect(
  RPostgres::Postgres(),
  host = u$hostname, port = u$port %||% 5432, dbname = u$path,
  user = u$username, password = u$password,
  sslmode = "require"
)
on.exit(dbDisconnect(con))

if (!dbExistsTable(con, "train_snapshots")) {
  dbExecute(con, "
    create table train_snapshots (
      collected_at    timestamptz not null,
      line            text,
      station         text,
      direction       text,
      destination     text,
      train_id        text,
      waiting_seconds integer,
      delay_seconds   integer
    )")
  dbExecute(con, "create index on train_snapshots (collected_at)")
  dbExecute(con, "create index on train_snapshots (line, station, collected_at)")
}

dbAppendTable(con, "train_snapshots", snapshot)
cat("Stored", nrow(snapshot), "rows at", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")
