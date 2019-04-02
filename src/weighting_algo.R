#' -----
#' title: parcel values
#' descripton: Unweight and transit weighted property values
#' author: nathancday@@gmail.com
#' date: 2019/03/20
#' -----

library(sf)
library(lwgeom)
library(units)

shapes <- read_sf("https://opendata.arcgis.com/datasets/0e9946c2a77d4fc6ad16d9968509c588_72.geojson")

shapes %<>% select(Assessment) %>% 
  filter(between(Assessment, 1e5, 1e6))

shapes %<>% st_transform_proj("+proj=longlat +datum=WGS84 +units=m +no_defs")

# map template
c_pal <-  colorNumeric("viridis", domain = shapes$Assessment)

leaflet(shapes) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal(Assessment),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "weighted: ")) %>%
  addPolylines(data = cat_routes,
               color = ~paste0("#", route_color),
               opacity = 1)


# Weighting ---------------------------------------------------------------

# route weighting
shapes_buffer <- mutate(shapes, 
                        geometry = st_buffer(geometry, .0001))

shapes_buffer %<>%
  mutate(on_routes = st_intersects(geometry, cat_routes) %>%
           map(~ cat_routes$route_short_name[.]),
         on_route = map_dbl(on_routes, length),
         weighted = Assessment * on_route)

shapes %<>%
  mutate(on_route_wt = shapes_buffer$on_route +1,
         weighted = Assessment * on_route_wt)

c_pal <-  colorNumeric("viridis", domain = shapes$weighted)

leaflet(shapes) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal(weighted),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "weighted:", weighted)) %>%
  addPolylines(data = cat_routes,
               color = ~paste0("#", route_color),
               opacity = 1)

# stop weighting

stop_buffers <- st_transform_proj(cat_stops, "+proj=longlat +datum=WGS84 +units=m +no_defs") %>% 
  mutate(geometry = st_buffer(geometry, .001))

shapes_buffer %<>%
  mutate(at_stops = st_intersects(geometry, stop_buffers) %>%
                          map(~ cat_routes$route_short_name[.]),
                        at_stop = map_dbl(on_routes, length))

shapes %<>%
  mutate(at_stop_wt = shapes_buffer$at_stop,
         all_weights = sqrt(on_route_wt + at_stop_wt),
         all_weights = ifelse(all_weights < 1, 1, all_weights),
         weighted = Assessment * all_weights)

c_pal <-  colorNumeric("viridis", domain = log10(shapes$weighted))

leaflet(shapes) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data = stop_buffers) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal(log10(weighted)),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "weighted:", weighted, "<br>",
                              "all_weights:", all_weights))


shapes$at_stop_bool <- shapes$at_stop_wt >= 1
c_pal <-  colorFactor("viridis", domain = shapes$at_stop_bool)

leaflet(shapes) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data = stop_buffers) %>% 
  addPolygons(weight = .1,
              color = "black",
              fillOpacity = .5,
              fillColor = ~c_pal(at_stop_bool),
              popup = ~paste0("raw: ", Assessment, "<br>",
                              "weighted:", at_stop_wt, "<br>",
                              "all_weights:", at_stop_bool))



