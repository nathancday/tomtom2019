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
    "tidyverse"),
  require,
  c = T
)

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

gtfs_routes_sf <- function(gtfs) {
  
  ## gather key-values first
  
  # trips_df has route_id:shape_id
  shape_key <- gtfs$trips_df %>%
    select(route_id, shape_id) %>%
    unique()
  
  # routes_df has route_id:route_name
  route_key <- gtfs$routes_df %>%
    select(route_id, route_short_name) %>%
    mutate(route_short_name = paste("route", route_short_name)) %>%
    inner_join(shape_key)
  
  # check for colors :)
  if ( !is.null(gtfs$routes_df$route_color) ) { # extract if they exist
    route_key %<>% inner_join(select(gtfs$routes_df, route_color, route_id) )
  }
  else { # planB: build a pal from my favorite pallette 'd3'
    route_key %<>% mutate(route_color = rep(ggsci::pal_d3()(10),
                                            length.out = nrow(route_key)))
  }
  
  ## build the sf object
  
  # exctract lon/lat values as matrix to build linestrings for each "shape_id"
  sfc <- gtfs$shapes_df %>% # long data frame
    split(.$shape_id) %>% # list of shorted data framee, one per shape
    map(~ select(., shape_pt_lon, shape_pt_lat) %>% # order maters, lon-1st lat-2nd
          as.matrix %>% # coherce for st_linestrings happiness
          st_linestring) %>%
    st_sfc(crs = 4326) # bundle all shapes into a collection w/ projection
  
  # add collection on and convert to sf
  unique(gtfs$shapes_df$shape_id) %>%
    sort() %>% # sort to match with names(sfc); split()'s factor-cohercion alpha sorts
    st_sf(shape_id = ., geometry = sfc) %>%
    inner_join(route_key)

}

cat_routes <- gtfs_routes_sf(cat)

# join-in counts

cat_routes %>% inner_join(route_counts)

gtfs_stops_sf <- function(gtfs) {
  shape_key <- gtfs$trips_df %>%
    select(trip_id, route_id, shape_id) %>%
    unique()
  
  # stop_times_df also has stop sequence and arrive/depart time for specific stops
  stop_key <- gtfs$stop_times_df %>%
    select(trip_id, stop_id) %>%
    unique() %>%
    inner_join(shape_key) %>% # one stop is on multiple routes
    # need to pair down
    arrange(route_id) %>% # use route_id as tiebreaker (for now)
    group_by(stop_id) %>% # group_by() to stop_id 
    slice(1) # to slice() out the first row
  
  if ( !is.null(gtfs$routes_df$route_color) ) {
    stop_key %<>% inner_join(select(gtfs$routes_df, route_color, route_id)) }
  else {stop_key %<>% mutate(route_color = rep(ggsci::pal_d3()(10), length.out = nrow(route_key))) }
  
  stops_sfc <- gtfs$stops_df %>%
    split(.$stop_id) %>%
    map(~select(., stop_lon, stop_lat) %>%
          unlist() %>%
          st_point() ) %>% # point instead of linestring
    st_sfc()
  
  st_sf(stop_key, geometry = stops_sfc) %>%
    inner_join(gtfs$stops_df)
}
cat_stops <- gtfs_stops_sf(cat)

# join stops to counts
cat_stops %<>% inner_join(select(stop_counts, -lng, -lat))

ggplot(cat_stops) +
  geom_sf(aes(color = route_id, size = n), shape = 1) +
  # geom_sf(data = cat_routes)
  ggsci::scale_color_d3("category20")
