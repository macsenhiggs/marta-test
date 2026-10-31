# Scratch file for poking at the MARTA real-time API and figuring out what changed.
# Run section by section. Nothing here is used by the app.

library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)

# old
url <- "https://developerservices.itsmarta.com:18096/itsmarta/railrealtimearrivals/developerservices/traindata"
# new
url <- "https://developerservices.itsmarta.com/mcsmrail/itsmarta/railrealtimearrivals/developerservices/traindata"

key <- Sys.getenv("MARTA_API_KEY")
nzchar(key)   # should be TRUE (does not print the key)

# What the app expects (columns we saw on 2026-10-27) vs what's there now
expected <- c("destination", "direction", "event_time", "is_realtime", "line", "next_arr",
              "station", "train_id", "waiting_seconds", "waiting_time",
              "delay", "latitude", "longitude")

# --- 1. Default request: status, format, headers ---
r_default <- GET(url, query = list(apiKey = key))
status_code(r_default)
headers(r_default)            # look for a version or format header
raw_default <- content(r_default, as = "text", encoding = "UTF-8")
substr(raw_default, 1, 600)   # start of the body

# --- 2. Asking for JSON ---
r_json <- GET(url, query = list(apiKey = key), accept_json())
status_code(r_json)
headers(r_json)$`content-type`
raw_json <- content(r_json, as = "text", encoding = "UTF-8")
substr(raw_json, 1, 600)

api_json <- fromJSON(raw_json) |> clean_names()
dim(api_json)
names(api_json)
glimpse(api_json)

# --- 3. Which expected fields are missing / which are new? ---
setdiff(expected, names(api_json))   # missing now
setdiff(names(api_json), expected)   # new

# Does any record carry extra keys? (a field could exist on only some records)
records <- fromJSON(raw_json, simplifyVector = FALSE)
table(map_chr(records, ~ paste(sort(names(.x)), collapse = ",")))

# --- 4. Search the raw text for the missing fields, in either format ---
grepl("DELAY|LATITUDE|LONGITUDE", raw_json, ignore.case = TRUE)
grepl("DELAY|LATITUDE|LONGITUDE", raw_default, ignore.case = TRUE)

# --- 5. Does the response change with the request? Try other things the endpoint might accept ---
# (edit these to try ideas; each prints the columns it gets back)
try_request <- function(label, ...) {
  r <- GET(url, query = list(apiKey = key), ...)
  b <- content(r, as = "text", encoding = "UTF-8")
  cols <- tryCatch(names(clean_names(fromJSON(b))), error = function(e) "(not JSON)")
  cat(label, "| HTTP", status_code(r), "|", headers(r)$`content-type`, "\n  columns:", paste(cols, collapse = ", "), "\n")
}
try_request("accept json      ", accept_json())
try_request("accept xml       ", accept_xml())
try_request("accept */*       ", add_headers(Accept = "*/*"))
try_request("no header at all ")

# --- 6. Data sanity: is the feed live and what do the rows look like? ---
api_json |> count(is_realtime)
api_json |> summarise(latest_event = max(event_time), n_trains = n_distinct(train_id[train_id != ""]))
api_json |> filter(is_realtime == "true") |> slice_head(n = 5)
