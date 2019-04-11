lapply(
  c("cowplot",
    "gtfsr",
    "leaflet",
    "sf",
    "tidycensus",
    "magrittr",
    "lubridate",
    "tidyverse"),
  require,
  c = T
)

theme_set(theme_bw())

source("https://gist.githubusercontent.com/nathancday/d50afccb762174d402f54486d3d328b6/raw/7e62f39efceb208b1a62c85c1cc72522a9ccb035/gtfs_sf.R")

# map data
cvl_geo <- get_acs("tract", "B00001_001",
                   county = "Charlottesville", state = "VA",
                   geometry = T) %>%
  select(GEOID, geometry)

vars <- c("rent" = "B25064_001")

years <- c(2010:2017) %>% set_names(., .)

cvl_dat <- map_df(years, 
                  ~ get_acs("tract", vars, year = ., survey = "acs5",
                            county = "Charlottesville", state = "VA"),
                  .id = "last_year") %>%
  mutate(variable = "rent") %>%
  select(-moe)

cvl <- inner_join(cvl_geo, cvl_dat)

d3_pal <- colorFactor(ggsci::pal_d3("category20")(12),
                      unique(cvl$GEOID)
)

cvl_centers <- cvl %>% 
  mutate(geometry = st_centroid(geometry))

leaflet(cvl, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addPolygons(color = ~d3_pal(GEOID), fillOpacity = .1, label = ~GEOID)

ggplot(cvl) +
  geom_sf(aes(fill = GEOID), show.legend = F) +
  geom_sf_label(aes(label = GEOID)) +
  ggsci::scale_fill_d3("category20") +
  coord_sf(datum = NA)

# Slide 1 -----------------------------------------------------------------

ggplot(cvl) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c() +
  coord_sf(datum = NA) + 
  facet_wrap(~ last_year, nrow = 2) +
  theme_minimal() +
  labs(fill = "Median Rent") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


# Slide 2 -----------------------------------------------------------------

# helper function to get overall delta
prct_changer <- . %>% 
  spread(last_year, estimate) %>% 
  mutate(change =  (`2017` - `2010`) / `2010`,
         change_alpha = abs(change) / max(abs(change)),
         change_lbl = round(100 * change)) %>% 
  gather("last_year", "estimate", `2010`:`2017`)

cvl %<>% prct_changer()

d3_pal <- ggsci::pal_d3("category20")(12) %>%
  set_names(cvl_geo$GEOID)

key_map <- ggplot(cvl_geo) +
  geom_sf(aes(fill = GEOID), show.legend = F) +
  ggsci::scale_fill_d3("category20") +
  coord_sf(datum = NA)

trend_plot <- ggplot(cvl, aes(last_year, estimate, color = GEOID, group = GEOID)) +
  geom_path(size = 2, show.legend = F) +
  ggsci::scale_color_d3("category20") +
  scale_y_continuous(labels = scales::dollar) +
  scale_alpha_continuous(range= c(.25, 1)) +
  labs(y = "Median Rent",
       x = NULL)

cowplot::plot_grid(key_map, trend_plot, rel_widths = c(.3, .6))


# slide2.3 ----------------------------------------------------------------
# alpha'd to show show red

cvl_geo %<>%
  mutate(alpha = case_when(grepl("401$", GEOID) ~ 1, 
                           TRUE ~ .8))

key_map <- ggplot(cvl_geo) +
  geom_sf(aes(fill = GEOID, alpha = alpha), show.legend = F) +
  ggsci::scale_fill_d3("category20") +
  scale_alpha(range = c(.25, 1)) +
  coord_sf(datum = NA)

cvl %<>%
  mutate(alpha = case_when(grepl("401$", GEOID) ~ 1, 
                           TRUE ~ .8))


trend_plot <- ggplot(cvl, aes(last_year, estimate, color = GEOID, group = GEOID)) +
  geom_path(aes(alpha = alpha), size = 2, show.legend = F) +
  ggsci::scale_color_d3("category20") +
  scale_y_continuous(labels = scales::dollar) +
  scale_alpha(range = c(.25, 1)) +
  labs(y = "Median Rent",
       x = NULL)

cowplot::plot_grid(key_map, trend_plot, rel_widths = c(.3, .6))

# slide2.6 ----------------------------------------------------------------
# alpha'd to show show bigs

cvl_geo %<>%
  mutate(color = case_when(grepl("(900)|(502)|(201)|(402)", GEOID) ~ GEOID, 
                           TRUE ~ "GREY"))

key_map <- ggplot(cvl_geo) +
  geom_sf(aes(fill = color, stroke = alpha), show.legend = F) +
  scale_fill_manual(values = d3_pal2) +
  scale_alpha(range = c(.25, 1)) +
  coord_sf(datum = NA)

key_map

cvl %<>%
  mutate(color = case_when(grepl("(900)|(502)|(201)|(402)", GEOID) ~ GEOID, 
                           TRUE ~ "GREY"))

d3_pal2 <- ggsci::pal_d3("category20")(12) %>%
  set_names(cvl_geo$GEOID)

trend_plot <- ggplot(cvl, aes(last_year, estimate, color = GEOID, group = GEOID)) +
  geom_path(aes(color = color), size = 2, show.legend = F) +
  scale_color_manual(values = d3_pal2) +
  scale_y_continuous(labels = scales::dollar) +
  scale_alpha(range = c(.25, 1)) +
  labs(y = "Median Rent",
       x = NULL)

trend_plot

cowplot::plot_grid(key_map, trend_plot, rel_widths = c(.3, .6))

# Slide 3 -----------------------------------------------------------------

vars <- c("population" = "B01003_001", "income" = "B21004_001")
decode_vars <- names(vars) %>% set_names(vars)

years <- c(2010:2017) %>% set_names(., .)

cvl_dat2 <- map_df(years, 
                  ~ get_acs("tract", vars, year = ., survey = "acs5",
                            county = "Charlottesville", state = "VA"),
                  .id = "last_year") %>%
  select(-moe)

trend_plot2 <- ggplot(cvl_dat2, aes(last_year, estimate, color = GEOID, group = GEOID)) +
  geom_path(size = 2, show.legend = F) +
  ggsci::scale_color_d3("category20") +
  facet_grid(variable ~ ., scales = "free") +
  scale_alpha_continuous(range= c(.25, 1)) +
  theme(strip.background = element_blank()) +
  labs(y = NULL,
       x = NULL)



cowplot::plot_grid(key_map, trend_plot2, rel_widths = c(.3, .6))


# Slide 3.5 ----------------------------------------------------------------
cvl_dat2 <- inner_join(cvl_dat2, cvl_geo)

plot_func <- function(data, title) {
  ggplot(data) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c() +
  coord_sf(datum = NA) + 
  facet_wrap(~ last_year, nrow = 1) +
  theme_minimal() +
  labs(fill =  title) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(1, "in"))
}
cvl_dat2 %>%
  split(.$variable) %>% 
  map2(c("Median Income", "Population"), ~ plot_func(.x, .y))
# saving these two plots by hand

li <- cvl_dat2 %>% split(.$variable) 

ggplot(li[[1]]) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(option = "B", labels = scales::dollar) +
  coord_sf(datum = NA) + 
  facet_wrap(~ last_year, nrow = 1) +
  theme_minimal()  +
  labs(fill = "Income") +
  theme(legend.title = element_text(size = rel(1.3)),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(1, "in"))

ggplot(li[[2]]) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(option = "E") +
  coord_sf(datum = NA) + 
  facet_wrap(~ last_year, nrow = 1) +
  theme_minimal()  +
  labs(fill = "Population") +
  theme(legend.title = element_text(size = rel(1.3)),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(1, "in"))

# Slide 4 -----------------------------------------------------------------
# * CAT routes raw -----

cat <- gtfsr::import_gtfs("https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/blob/b6c714ec190f8843d6aa154fc74ed7be3bd5307f/data/2017_08_CharlottesvilleAreaTransit.zip?raw=true")
cat %>% gtfsr::map_gtfs(., route_ids = .$routes_df$route_id,
                        route_colors = paste0("#", .$routes_df$route_color))

# extract routes and stops
cat_routes <- gtfs_routes_sf(cat) %>%
  group_by(route_short_name, route_color) %>%
  summarise(geometry = st_combine(geometry))

cat_stops <- gtfs_stops_sf(cat) %>%
  st_set_crs(st_crs(cat_routes)) %>%
  select(-stop_desc, -stop_url, -location_type, -parent_station,
         -stop_timezone, -wheelchair_boarding, -trip_id, -stop_id)

# * Ridership raw -----
dat <- data.table::fread("~/Documents/transit.txt")

names(dat) <- c("id", "stop_name", "route", "date_time", "count1", "count2",
                "fare_category", "lat", "lng", "payment")

# clean up route names
# matching names for joins later
dat %<>% mutate(route_short_name = str_replace(route, " .*", "") %>% 
         str_replace("T.*", "T") %>% 
         str_replace("^0", "") %>% 
         paste("route", .))

# clean up stops
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

stop_counts <- count(dat, stop_number, lng, lat) %>%
  rename(stop_code = stop_number) # for joings later

ggplot(stop_counts, aes(lng, lat, size = n)) +
  geom_point(alpha = .5)

route_counts <- dat %>%
  count(route_short_name)

# * join shapes + counts ------
cat_routes1 <- inner_join(cat_routes, route_counts)
cat_stops1 %<>% inner_join(cat_stops, select(stop_counts, -lng, -lat))

# weight routes ?
cat_routes1 %<>%
  ungroup() %>% 
  mutate(wtd = 20 * n / max(n),
         wtd = ifelse(wtd < 3, 3, wtd))

# Slide 5 -----------------------------------------------------------------
# leaflet(cat_routes1) %>% 
#   addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
#   addPolylines(color = ~paste0("#", route_color),
#                weight = ~wtd)
# 
# pal <- paste0("#", cat_routes1$route_color) %>% 
#   set_names(cat_routes1$route_short_name)
# 
# cat_routes1 %>%
#   mutate(route_short_name = reorder(route_short_name, n)) %>%
#   ggplot() +
#   geom_col(aes(y = n, x = route_short_name, fill = route_short_name)) +
#   coord_flip() +
#   scale_fill_manual(values = pal) +
#   scale_y_continuous(labels = c(0, .25, .5, .75, 1)) +
#   theme(legend.position = "none") +
#   labs(y = "Million Riders",
#        x = NULL,
#        caption = "")
# 
# cat_routes1 %<>%
#   ungroup() %>% 
#   mutate(wtd = 20 * n / max(n),
#          wtd = ifelse(wtd < 3, 3, wtd))
# 
# leaflet(cat_routes1) %>% 
#   addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
#   addPolylines(color = ~paste0("#", route_color),
#                weight = ~wtd)
# 
# pal <- paste0("#", cat_routes1$route_color) %>% 
#   set_names(cat_routes1$route_short_name)
# 
# cat_routes1 %>%
#   mutate(route_short_name = reorder(route_short_name, n)) %>%
#   ggplot() +
#   geom_col(aes(y = n, x = route_short_name, fill = route_short_name)) +
#   coord_flip() +
#   scale_fill_manual(values = pal) +
#   scale_y_continuous(labels = function(breaks) breaks / 1000) +
#   theme(legend.position = "none") +
#   labs(y = "Thousand Riders",
#        x = NULL,
#        caption = "From 2017-09-01 to 2019-01-31")






# Slide 5.5 ----------------------------------------------------------------
# CAT routes for work day travel
# * look at work day stops ------
commute_dat <- dat %>%
  mutate(date_time = as_datetime(date_time, tz = "US/Eastern"))

ggplot(data = commute_dat, aes(date_time)) + geom_histogram()
range(commute_dat$date_time)

commute_dat <- mutate(commute_dat,
                      wday = wday(date_time),
                      hour = hour(date_time)) %>%
  filter(hour %in% 7:9,
         wday %in% 2:6)

commute_route_counts <- commute_dat %>%
  count(route_short_name)

commute_routes <- inner_join(cat_routes, commute_route_counts)

commute_routes %<>%
  ungroup() %>% 
  mutate(wtd = 20 * n / max(n),
         wtd = ifelse(wtd < 3, 3, wtd))

leaflet(commute_routes) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addPolylines(color = ~paste0("#", route_color),
               weight = ~wtd,
               popup = ~route_short_name)

pal <- paste0("#", commute_routes$route_color) %>% 
  set_names(commute_routes$route_short_name)

commute_routes %>%
  mutate(route_short_name = reorder(route_short_name, n)) %>%
  ggplot() +
  geom_col(aes(y = n, x = route_short_name, fill = route_short_name)) +
  coord_flip() +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = function(breaks) breaks / 1000) +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(1.2))) +
  labs(y = "Thousand riders",
       x = NULL,
       caption = "M-F 7a-9a")


# Slide 5.75 ---------------------------------------------------------------
# commmute stops

commute_stop_counts <- commute_dat %>%
  count(stop_number) %>% 
  inner_join(cat_stops, ., by = c("stop_code" = "stop_number")) %>%
  select(-stop_lat, -stop_lon)


leaflet(commute_stop_counts) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addCircleMarkers(color = ~paste0("#", route_color),
                   radius = ~ sqrt(n) / 10,
                   popup = ~n)

pal <- colorNumeric("viridis", domain = sqrt(commute_stop_counts$n))

buffer_stops <- commute_stop_counts %>% 
  mutate(count_n = as.character(n),
         geometry = st_buffer(geometry, .001))# ~ 100m

leaflet(buffer_stops) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addPolygons(color = ~pal(sqrt(n)),
              popup = ~count_n)



# Slide 6 -----------------------------------------------------------------

shapes <- read_sf("https://opendata.arcgis.com/datasets/0e9946c2a77d4fc6ad16d9968509c588_72.geojson")

shapes %<>%
  select(Assessment) %>% 
  mutate(Assessment = case_when(between(Assessment, 1e4, 1e6) ~ Assessment,
                                TRUE ~ NA_integer_)) %>% 
  filter(as.numeric(st_area(.)) < 4100) # ~ 1 acre

c_pal <-  colorNumeric("viridis", domain = shapes$Assessment)

leaflet(shapes) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(weight = .2,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal(Assessment))

library(ggridges)
ggplot(shapes, aes(x = Assessment,fill = ..x.., y = 1)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c() +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  scale_x_continuous(labels = scales::dollar) +
  labs(y = NULL,
       x = NULL,
       caption = "12,388 parcels <= ~ 1 acre")

# Slide 7 ----------------------------------------------------------------
# stop shape buffering to join with parcels

weighted_stops <- buffer_stops %>%
  mutate(
    # weight = sqrt(n),
    weight = log(n),
         weight = weight / max(weight),
         weight = weight + 1)

c_pal <-  colorNumeric("inferno", domain = weighted_stops$weight)

leaflet(weighted_stops, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(color = ~c_pal(weight), fillOpacity = .7)

# Slide 8 ----------------------------------------------
# masking lots

shapes_by_stops <- shapes %>%
  mutate(on_wt = st_intersects(geometry, weighted_stops),
         matched_wt = map(on_wt, ~ weighted_stops[unlist(.),]),
         total_wt = map_dbl(matched_wt, ~ min(sum(.$weight), 5)), # max out at 5x multiplier
         weighted = Assessment * total_wt,
         weighted = ifelse(weighted == 0, NA_real_, weighted))

c_pal2 <-  colorNumeric("viridis", domain = shapes_by_stops$Assessment)

leaflet(weighted_stops, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(data = shapes_by_stops,
              weight = .1,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal2(Assessment)) %>% 
addPolygons(color = ~c_pal(weight), fillOpacity = .1)


# Slide 9 -----------------------------------------------------------------

c_pal <-  colorNumeric("viridis", domain = shapes_by_stops$total_wt)
c_pal2 <-  colorNumeric("viridis", domain = ideal_lots$total_wt)

leaflet(shapes_by_stops) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal(total_wt),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "transit weight: ", total_wt, "<br>",
                              "weighted:", weighted))

shapes_by_stops %>%
  ggplot(aes(x = Assessment, y = total_wt, color = total_wt)) +
  geom_point(size = 3, alpha = .3) +
  geom_rect(xmin = 0, xmax = 1e6, ymin = -.5, ymax = 1,
            alpha = .01, aes(color = NULL), fill = "lightgrey") +
  geom_rect(xmin = 4e5, xmax = 1.02e6, ymin = -.5, ymax = 5.5,
            alpha = .01, aes(color = NULL), fill = "lightgrey") +
  scale_x_continuous(labels = scales::dollar) +
  scale_color_viridis_c() +
  labs(x = NULL,
       y = "Transit score") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(1.1)))

ideal_lots <- shapes_by_stops %>% 
  filter(Assessment < 4.5e5, total_wt > 1)

c_pal <-  colorNumeric("viridis", domain = shapes_by_stops$total_wt)
c_pal2 <-  colorNumeric("viridis", domain = ideal_lots$total_wt)

leaflet(ideal_lots, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .7,
              fillColor = ~c_pal2(total_wt),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "transit weight: ", total_wt, "<br>",
                              "weighted:", weighted))


# Slide 11 ----------------------------------------------------------------


c_pal <-  colorNumeric("viridis", domain = shapes_by_stops$total_wt)
c_pal2 <-  colorNumeric("viridis", domain = ideal_lots$total_wt)

leaflet(ideal_lots, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .7,
              fillColor = ~c_pal2(total_wt),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "transit weight: ", total_wt, "<br>",
                              "weighted:", weighted))


