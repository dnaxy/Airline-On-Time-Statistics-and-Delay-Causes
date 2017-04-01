# load required library
library(DBI)
library(RPostgreSQL)
library(dplyr)
# DB connection
flights.db <- src_postgres(
  dbname = "joe_db",
  host = "localhost",
  port = 5432,
  user = "joe",
  password = "test"
)
# after test with the data come from 1 month, 3 month, the whole 12 month,
# choose the 3 month data, from Oct, 2015 to Dec, 2015 for demo due to
# the performance issue
flights <- tbl(flights.db, "month10to12")
# create a subset database to increase the performance
jetblue <-
  flights %>% select(
    Carrier,
    OriginCityName,
    DestCityName,
    DepDelayMinutes,
    ArrDelayMinutes,
    CarrierDelay,
    WeatherDelay,
    NASDelay,
    SecurityDelay,
    LateAircraftDelay
  ) %>% filter(Carrier == 'B6')
# get the unique OriginCityName and DestCityName combination
airport.list <-
  jetblue %>% distinct(OriginCityName, DestCityName) %>%
  collect() %>% arrange(OriginCityName, DestCityName)
