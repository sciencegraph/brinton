library(dplyr)

data(rockArt, package = "DAAG")
data(ca2006, package = "pscl")

data(monica, package = "DAAG")
monica_mod <- monica[1:100,]

flights_dt <- nycflights13::flights %>%
  dplyr::transmute(
    departure = lubridate::make_datetime(
    year,
    month,
    day,
    hour,
    minute),
    arrival = departure + lubridate::minutes(sched_dep_time),
    delay = sched_dep_time + 1
  )

data(midwest, package = "ggplot2")
midwest <- as.data.frame(midwest)

flights_dt50 <- as.data.frame(flights_dt[1:50,])

# 1vars
brinton::longplot(ca2006, "open")
brinton::longplot(esoph, "agegp")
brinton::longplot(monica_mod, "smstat")
brinton::longplot(rockArt, "District")
brinton::longplot(flights_dt50, "departure")
brinton::longplot(midwest, "area")

# 2vars
brinton::longplot(midwest, c("area", "poptotal"))
brinton::longplot(flights_dt50, c("departure", "arrival"))
brinton::longplot(flights_dt50, c("departure", "delay"))
brinton::longplot(iris, c("Species", "Sepal.Length"))
