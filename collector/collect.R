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

# The feed has one row per upcoming station per train, so a train's line, direction, destination
# and delay repeat on every one of its rows. Store them once (train_polls) and keep only what
# differs per station in train_arrivals. Scheduled rows have no train_id or delay, so only live rows are stored.
live <- get_trains() |>
  filter(is_realtime, !is.na(train_id), train_id != "") |>
  mutate(collected_at = Sys.time())

train_polls <- live |>
  distinct(collected_at, train_id, .keep_all = TRUE) |>
  transmute(collected_at, train_id, line, direction, destination,
            delay_seconds = as.integer(delay))

train_arrivals <- live |>
  distinct(collected_at, train_id, station, .keep_all = TRUE) |>
  transmute(collected_at, train_id, station, waiting_seconds = as.integer(waiting_seconds))

# RPostgres doesn't take a URL, so split DATABASE_URL into its parts
u <- httr::parse_url(Sys.getenv("DATABASE_URL"))
con <- dbConnect(
  RPostgres::Postgres(),
  host = u$hostname, port = u$port %||% 5432, dbname = u$path,
  user = u$username, password = u$password,
  sslmode = "require"
)
on.exit(dbDisconnect(con))

if (!dbExistsTable(con, "train_polls")) {
  dbExecute(con, "
    create table train_polls (
      collected_at  timestamptz not null,
      train_id      text        not null,
      line          text,
      direction     text,
      destination   text,
      delay_seconds integer,
      primary key (collected_at, train_id)
    )")
  dbExecute(con, "create index on train_polls (line, collected_at)")
}
if (!dbExistsTable(con, "train_arrivals")) {
  dbExecute(con, "
    create table train_arrivals (
      collected_at    timestamptz not null,
      train_id        text        not null,
      station         text        not null,
      waiting_seconds integer,
      primary key (collected_at, train_id, station),
      foreign key (collected_at, train_id) references train_polls (collected_at, train_id)
    )")
  dbExecute(con, "create index on train_arrivals (station, collected_at)")
}

# Both inserts or neither, so the tables can't drift out of sync
dbWithTransaction(con, {
  dbAppendTable(con, "train_polls", train_polls)
  dbAppendTable(con, "train_arrivals", train_arrivals)
})
cat("Stored", nrow(train_polls), "trains and", nrow(train_arrivals), "arrivals at",
    format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")
