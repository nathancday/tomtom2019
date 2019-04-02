lapply(
  c("cowplot",
    "gtfsr",
    "leaflet",
    "sf",
    "tidycensus",
    "magrittr",
    "tidyverse"),
  require,
  c = T
)

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

cvl <- inner_join(cvl_dat, cvl_geo)

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

key_map <- ggplot(cvl_geo) +
  geom_sf(aes(fill = GEOID), show.legend = F) +
  ggsci::scale_fill_d3("category20") +
  coord_sf(datum = NA)

trend_plot <- ggplot(cvl, aes(last_year, estimate, color = GEOID, group = GEOID)) +
  geom_path(size = 2, show.legend = F) +
  ggsci::scale_color_d3("category20") +
  scale_alpha_continuous(range= c(.25, 1)) +
  labs(y = "Median Rent",
       x = NULL)

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

# Slide 4 -----------------------------------------------------------------

cat <- gtfsr::import_gtfs("https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/blob/b6c714ec190f8843d6aa154fc74ed7be3bd5307f/data/2017_08_CharlottesvilleAreaTransit.zip?raw=true")
cat %>% gtfsr::map_gtfs(., route_ids = .$routes_df$route_id,
                        route_colors = paste0("#", .$routes_df$route_color))


dat <- data.table::fread("~/Documents/transit.txt")

names(dat) <- c("id", "stop_name", "route", "date_time", "count1", "count2",
                "fare_category", "lat", "lng", "payment")

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
  # matching names for joins later
  mutate(route_short_name = str_replace(route, " .*", "") %>% 
           str_replace("T.*", "T") %>% 
           str_replace("^0", "") %>% 
           paste("route", .)) %>%
  count(route_short_name)

# extract routes and stops
cat_routes <- gtfs_routes_sf(cat) %>%
  group_by(route_short_name, route_color) %>%
  summarise(geometry = st_combine(geometry))

cat_stops <- gtfs_stops_sf(cat) %>% 
  st_set_crs(st_crs(cat_routes))

# join-in counts
cat_routes %<>% inner_join(route_counts)
cat_stops %<>% inner_join(select(stop_counts, -lng, -lat))

cat_routes %<>%
  ungroup() %>% 
  mutate(wtd = 20 * n / max(n),
         wtd = ifelse(wtd < 3, 3, wtd))

# Slide 5 -----------------------------------------------------------------
leaflet(cat_routes) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addPolylines(color = ~paste0("#", route_color),
               weight = ~wtd)

pal <- paste0("#", cat_routes$route_color) %>% 
  set_names(cat_routes$route_short_name)


cat_routes %>%
  mutate(route_short_name = reorder(route_short_name, n)) %>%
  ggplot() +
  geom_col(aes(y = n, x = route_short_name, fill = route_short_name)) +
  coord_flip() +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = c(0, .25, .5, .75, 1)) +
  theme(legend.position = "none") +
  labs(y = "Million Riders",
       x = NULL)


# Slide 6 -----------------------------------------------------------------

shapes <- read_sf("https://opendata.arcgis.com/datasets/0e9946c2a77d4fc6ad16d9968509c588_72.geojson")

shapes %<>%
  select(Assessment) %>% 
  mutate(Assessment = case_when(between(Assessment, 1e4, 1e6) ~ Assessment,
                                TRUE ~ NA_integer_))

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
  scale_fill_viridis_c(name = "$", option = "D") +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  scale_x_continuous(labels = scales::comma) +
  labs(y = NULL,
       x = "$$$")

# Slide 7 ----------------------------------------------------------------

cat_routes2 <- cat_routes %>%
  mutate(color2 = case_when(n > 4e5 ~ "1",
                            n > 1e5 ~ "2",
                            TRUE ~ "3"))

pal2 <- colorFactor("plasma", levels(cat_routes2$color2))

leaflet(cat_routes2) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addPolylines(color = ~pal2(color2),
               weight = 5) %>% 
  addLegend("topright", pal = pal2, values = ~color2,
            title = "Route tier",
            opacity = 1
  )

tmp <- select(cat_routes2, route_short_name, color2, route_color) %>%
  st_set_geometry(NULL)
cat_stops2 <- cat_stops %>%
  inner_join(tmp) %>%
  mutate(geometry = st_buffer(geometry, .001))

leaflet(cat_stops2) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
  addPolygons(color = ~pal2(color2),
                   weight = 5) %>% 
  addLegend("topright", pal = pal2, values = ~color2,
            title = "Route tier",
            opacity = 1
  )




         
         
