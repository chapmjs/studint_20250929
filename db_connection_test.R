library(DBI)
library(RMySQL)

conn <- dbConnect(
  MySQL(),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port = as.integer(Sys.getenv("DB_PORT"))
)

dbGetQuery(conn, "SELECT 1")
dbDisconnect(conn)
