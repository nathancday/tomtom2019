#' -----
#' title: Data input
#' description: Read in raw JSON and tidy it
#' author: nathancday@@gmail.com
#' date: 2019-03-23
#' -----

lapply(
  c("magrittr",
    "tidyverse"),
  require,
  c = T
)

# In ----------------------------------------------------------------------

dat <- data.table::fread("~/Documents/transit.txt")
format(object.size(dat), units = "auto")

names(dat) <- c("id", "stop_name", "route", "date_time", "count1", "count2",
                "fare_category", "lat", "lng", "payment")

# check routes
table(dat$route) # looks good

# check stops
table(dat$stop_name)
table(dat$stop_name) %>% length()

# split out stop ref'd by lat-lon
bad_stops <- filter(dat, grepl("Please refer", stop_name))
dat %<>% anti_join(bad_stops)

bad_stop_counts <- with(bad_stops, table(lat, lng)) %>%
  as.data.frame() %>% 
  filter(Freq > 0)

# of remaining "good" stops split `stop_number` and `stop_name`
dat %<>% separate(stop_name, c("stop_number", "stop_name"), sep = " - ")

# look for matches to names stops
good_stops <- select(dat, lat, lng, stop_number, stop_name) %>% unique()

# lots of stops are referenced by slightly different lat/lng values
# just picking one to be the "true" geo-location
good_stops %<>% group_by(stop_name, stop_number) %>% slice(1)

# merge "fixed" back into `dat`
dat %<>% select(-lat, -lng) %>% inner_join(good_stops)




# separate stop id into number and name


