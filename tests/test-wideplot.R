library(dplyr)
library(brinton)

data(ca2006, package = "pscl")

flights_dt <- nycflights13::flights %>%
  dplyr::transmute(departure = lubridate::make_datetime(
    year,
    month,
    day,
    hour,
    minute)
  )

flights_dt50 <- as.data.frame(flights_dt[1:50,])

wideplot(ca2006, label = TRUE)
wideplot(as.data.frame(ChickWeight), label = TRUE)
wideplot(flights_dt50, label = TRUE)
