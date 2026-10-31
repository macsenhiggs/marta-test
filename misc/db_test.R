# One-off check that DATABASE_URL (in .Renviron) connects to Neon
library(DBI)

url <- Sys.getenv("DATABASE_URL")
stopifnot("DATABASE_URL is empty; restart R after editing .Renviron" = nzchar(url))

# RPostgres doesn't take a URL, so split it into its parts
u <- httr::parse_url(url)
con <- dbConnect(
  RPostgres::Postgres(),
  host = u$hostname, port = u$port %||% 5432, dbname = u$path,
  user = u$username, password = u$password,
  sslmode = "require"
)
on.exit(dbDisconnect(con))
dbGetQuery(con, "select 1 as ok, current_database() as db, version() as server")
