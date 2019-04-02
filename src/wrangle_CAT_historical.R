#' -----
#' title: Wrangling CAT ridership data
#' description: Read in raw JSON and tidy it
#' author: nathancday@@gmail.com
#' date: 2019-03-23
#' -----

lapply(
  c("gtfsr",
    "sf",
    "magrittr",
    "tidyverse",
    "leaflet"),
  require,
  c = T
)

# some old GTFS heler functions
source("https://gist.githubusercontent.com/nathancday/d50afccb762174d402f54486d3d328b6/raw/7e62f39efceb208b1a62c85c1cc72522a9ccb035/gtfs_sf.R")

scale_colour_discrete <- function(...) {
  ggsci::scale_color_d3("category20", ...)
}

# In ----------------------------------------------------------------------

dat <- data.table::fread("~/Documents/transit.txt")
format(object.size(dat), units = "auto")

names(dat) <- c("id", "stop_name", "route", "date_time", "count1", "count2",
                "fare_category", "lat", "lng", "payment")


# Clean -------------------------------------------------------------------

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


# Aggregate ---------------------------------------------------------------

stop_counts <- count(dat, stop_number, lng, lat) %>%
  rename(stop_code = stop_number) # for joings later

ggplot(stop_counts, aes(lng, lat, size = n)) +
  geom_point(alpha = .5)

route_counts <- dat %>%
  # matching names for joins later
  mutate(route_short_name = str_replace(route, " .*", "") %>% 
           str_replace("T.*", "T") %>% 
           str_replace("^0", "") %>% 
           paste("route", .)) %>%
  count(route_short_name)

# GTFS --------------------------------------------------------------------

cat <- gtfsr::import_gtfs("https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/blob/b6c714ec190f8843d6aa154fc74ed7be3bd5307f/data/2017_08_CharlottesvilleAreaTransit.zip?raw=true")
cat %>% gtfsr::map_gtfs(., route_ids = .$routes_df$route_id,
                 route_colors = paste0("#", .$routes_df$route_color))

# extract routes and stops
cat_routes <- gtfs_routes_sf(cat) %>%
  group_by(route_short_name, route_color) %>%
  summarise(geometry = st_combine(geometry))

cat_stops <- gtfs_stops_sf(cat) %>% 
  st_set_crs(st_crs(cat_routes))

# join-in counts
cat_routes %<>% inner_join(route_counts)
cat_stops %<>% inner_join(select(stop_counts, -lng, -lat))

ggplot(cat_stops) +
  geom_sf(aes(color = route_id, size = n), shape = 1)

ggplot(cat_routes) +
  geom_sf(aes(color = route_short_name, size = n))

ggplot(cat_stops) +
  geom_sf(data = cat_routes, aes(color = alpha(route_id, .2), size = n)) +
  geom_sf(aes(color = route_id, size = n), shape = 1)

ggplot(cat_stops) +
  geom_sf(data = cvl_geo, show.legend = F) +
  geom_sf(aes(color = route_id, size = n), alpha = .5)

ggplot(cat_stops) +
  geom_sf(data = cvl_geo, show.legend = F) +
  geom_sf(data = cat_routes, aes(color = alpha(route_id, .2), size = n))

plot(cat_routes)

cat_routes2 <- get_routes_sldf(cat, cat$routes_df$route_id, NULL, NULL, .5,
                              unique(cat_routes$route_color))

leaflet(cat_stops) %>% 
  addCircleMarkers()

# names(cat_routes$geometry) <- NULL

cat_routes$n <- cat_routes$n /max(cat_routes$n) * 10

leaflet(cat_routes) %>% 
  addTiles() %>% 
  addPolylines(color = ~paste0("#", route_color),
               weight = 10)
