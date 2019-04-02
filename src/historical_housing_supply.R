#' -----
#' title: Historical housing supply
#' description: Use tidycensus to get timecourse of housing/population as maps
#' author: nathancday@@gmail.com
#' date: 2019-03-29
#' -----

lapply(
  c("cowplot",
    "tidycensus",
    "magrittr",
    "tidyverse"),
  require,
  c = T
)

# In ----------------------------------------------------------------------

# map data
cvl_geo <- get_acs("tract", "B00001_001",
                   county = "Charlottesville", state = "VA",
                   geometry = T) %>%
  select(GEOID, geometry)

# vs <- load_variables(2017, "acs5", cache = TRUE)
# View(vs)

vars <- c("B00001_001", "B25064_001", "B08301_010")
decode_vars <- c("population", "housing_units", "public_transit") %>%
  set_names(vars)

years <- c(2010:2017) %>% set_names(., .)

cvl_dat <- map_df(years, 
              ~ get_acs("tract", vars, year = ., survey = "acs5",
                        county = "Charlottesville", state = "VA"),
              .id = "last_year") %>%
  mutate(variable = decode_vars[variable])

# build new ratio-variabls
cvl_dat %<>% 
  select(-moe) %>% 
  spread(variable, estimate) %>% 
  mutate(housing_to_pop = housing_units / population) %>% 
  gather("variable", "estimate", housing_units:housing_to_pop)

cvl <- full_join(cvl_geo, cvl_dat)


# Plots --------------------------------------------------------------------

plot_map_fill <- function(data, option = "D") {
  ggplot(data) +
    geom_sf(aes(fill = estimate)) +
    scale_fill_viridis_c(option = option) +
    coord_sf(datum = NA) + 
    facet_wrap(~ last_year, nrow = 2) +
    labs(title = unique(data$variable))
}

cvl %>% 
  split(.$variable) %>% 
  map2(LETTERS[1:4], ~plot_map_fill(.x, .y)) %>%
  cowplot::plot_grid(plotlist = .)

# helper function to get overall delta
prct_changer <- . %>% 
  spread(last_year, estimate) %>% 
  mutate(change =  (`2017` - `2010`) / `2010`,
         change_alpha = abs(change) / max(abs(change)),
         change_lbl = round(100 * change)) %>% 
  gather("last_year", "estimate", `2010`:`2017`)

plot_trend <- function(data) {
  
  data <- prct_changer(data)
  
  ggplot(data, aes(last_year, estimate, color = GEOID, group = GEOID)) +
    geom_path(size = 2, show.legend = F, aes(alpha = change_alpha)) +
    ggsci::scale_color_d3("category20") +
    scale_size_continuous(range= c(.1, 2)) +
    labs(title = unique(data$variable))
}


key_map <- ggplot(cvl_geo) +
  geom_sf(aes(fill = GEOID), show.legend = F) +
  ggsci::scale_fill_d3("category20") +
  coord_sf(datum = NA)
  

cvl %>% 
  split(.$variable) %>% 
  map(plot_trend) %>% 
  cowplot::plot_grid(plotlist = .) %>%
  cowplot::plot_grid(key_map, ., rel_widths = c(1, 3))



