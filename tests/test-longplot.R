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
brinton::longplot(midwest, c("area", "poptotal"))                           # 2 numeric
brinton::longplot(flights_dt50, c("departure", "arrival"))                  # 1 datetime
brinton::longplot(flights_dt50, c("departure", "delay"))                    # 1 datetime and 1 numeric
brinton::longplot(iris, c("Species", "Sepal.Length"))                       # 1 factor and 1 numeric
brinton::longplot(mediation::jobs, c("occp", "marital"))                    # 2 factor
brinton::longplot(as.data.frame(ggplot2::diamonds), c("clarity", "cut"))    # 2 ordered
brinton::longplot(Ecdat::HI, c("education", "region"))                      # 1 ordered and 1 factor
