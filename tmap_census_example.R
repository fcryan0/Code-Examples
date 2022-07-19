# tmap examples


library(tidyverse)
library(sf)
library(tmap)
library(tidycensus)
library(basemaps)


# Download Data -----------------------------------------------------------

# Use tidycensus to download some county-level data
# Note that you may need to set a census API key
# See: https://walker-data.com/tidycensus/articles/basic-usage.html 
countyPop <- 
  get_acs(
    geography = "county",
    state = "WI",
    variables = "B01001_001",
    geometry = TRUE,
    year = 2020
    ) %>% 
  rename(Population = estimate) %>% 
  mutate(
    label = str_replace(NAME, " County, Wisconsin", ""),
    .before = geometry
    )

# Download some point data
seaplaneBases <- 
  read_sf("https://geo.dot.gov/mapping/rest/services/NTAD/Airports/MapServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  filter(Fac_Type == "SEAPLANE BASE") %>% st_transform(4269) %>% st_filter(countyPop)

# Make a basic map
tm_shape(countyPop) + tm_polygons()


# Display Types -----------------------------------------------------------

# Different display types (Can be stacked)
tm_shape(countyPop) + tm_borders()
tm_shape(countyPop) + tm_dots()
tm_shape(countyPop) + tm_text("GEOID")
tm_shape(countyPop) + tm_fill()
tm_shape(countyPop) + tm_borders() + tm_dots()

# Symbology
tm_shape(countyPop) + tm_polygons(col = "Population")
tm_shape(countyPop) + tm_polygons(col = "Population", style = "quantile")
tm_shape(countyPop) + tm_polygons(col = "Population", style = "jenks")
tm_shape(countyPop) + tm_polygons(col = "Population", style = "log10_pretty")

tm_shape(countyPop) + 
  tm_borders() +
  tm_dots(size = "Population", col = "Population", scale = 2)

# Custom legend breaks 
tm_shape(countyPop) + 
  tm_polygons(
    col = "Population", 
    breaks = c(0, 10000, 50000, 333333, Inf)
    )

# Be mindful of data ranges when setting custom breaks
tm_shape(countyPop) + 
  tm_polygons(
    col = "Population", 
    breaks = c(10000, 50000, 333333)
  )



# Colors ------------------------------------------------------------------

# Changing Colors
tm_shape(countyPop) + 
  tm_polygons(
    col = "Population", style = "jenks",
    palette = "Blues"
  )

# Inverse Colors
tm_shape(countyPop) + 
  tm_polygons(
    col = "Population", style = "jenks",
    palette = "-Reds"
  )

# Custom Colors
tm_shape(countyPop) + 
  tm_polygons(
    col = "Population", style = "jenks",
    palette = c("red", "green", "yellow", "pink", "gray")
  )


# Multiple Map Objects ----------------------------------------------------

# Multiple Objects
tm_shape(countyPop) + 
  tm_polygons(col = "Population", style = "jenks") +
tm_shape(seaplaneBases) + 
  tm_symbols(col = "Elevation", size = 1, palette = "Blues")


# Legend Adjustments
map <- 
  tm_shape(countyPop) + 
    tm_polygons(col = "Population", style = "jenks") +
  tm_shape(seaplaneBases) + 
    tm_symbols(col = "Elevation", size = 1, palette = "Blues")

# Legend Adjustments ------------------------------------------------------

# bottom left corner
map +
  tm_layout(legend.position = c("left", "bottom"))

# Outside of the map bounding box
map +
  tm_layout(legend.outside = TRUE)

# Bottom of map, horizontally stacked
map =
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.stack = "horizontal",
    frame = FALSE
  )


# basemaps ----------------------------------------------------------------

# Download basemaps for the map background

# ESRI World Streets
base <- basemap(
  ext = countyPop,
  map_service = "esri",
  map_type = "world_street_map"
  )
tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_borders(alpha = 0.5)

# ESRI Aerials
base <- basemap(
  ext = countyPop,
  map_service = "esri",
  map_type = "world_imagery"
)
tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_borders(col = "gray80")

# CartoDBLight
base <- basemap(
  ext = countyPop,
  map_service = "carto",
  map_type = "light"
)
tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_borders(col = "gray80")


tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_polygons(col = "Population", alpha = 0.5) +
  tm_layout(legend.position = c("left", "bottom"))

# Separate Labels
base <- basemap(
  ext = countyPop,
  map_service = "carto",
  map_type = "light_no_labels"
)
labels <- basemap(
  ext = countyPop,
  map_service = "carto",
  map_type = "light_only_labels"
)

tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_polygons(col = "Population", alpha = 0.5) +
  tm_layout(legend.position = c("left", "bottom")) +
  tm_shape(labels) + tm_rgb()


# Expanding the View Extent
base <- basemap(
  ext = countyPop %>% st_transform(8162) %>% st_buffer(5280*10),
  map_service = "carto",
  map_type = "light",
  force = TRUE #force to download new
)
tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_borders(col = "gray50")

tm_shape(base) + tm_rgb() +
  tm_shape(countyPop) + tm_polygons(col = "Population", alpha = 0.5) +
  tm_layout(legend.position = c("left", "bottom"))

# ESRI Aerials Detail

# First seaplane base (poor quality)
base <- basemap(
  ext = seaplaneBases[1,] %>% st_transform(8162) %>% st_buffer(500),
  map_service = "esri",
  map_type = "world_imagery"
)
tm_shape(base) + tm_rgb()

# Random point in downtown Minneapolis (high quality)
pointMinneapolis <- st_point(c(-93.26816303007287, 44.97513872600363)) %>% st_sfc(crs = 4326) %>% st_as_sf() 
base <- basemap(
  ext = pointMinneapolis %>% st_transform(8162) %>% st_buffer(500),
  map_service = "esri",
  map_type = "world_imagery"
)
tm_shape(base) + tm_rgb()


# Putting it all Together -------------------------------------------------

# Adding Legends Manually
# Saving output file
base <- basemap(
  ext = countyPop %>% st_transform(8162) %>% st_buffer(5280*10),
  map_service = "carto",
  map_type = "light_no_labels",
  map_res = 1
)

labels <- basemap(
  ext = countyPop %>% st_transform(8162) %>% st_buffer(5280*10),
  map_service = "carto",
  map_type = "light_only_labels",
  map_res = 1
)

map <- 
  tm_shape(base, unit = "imperial") + tm_rgb() + 
  tm_add_legend(type = "symbol", labels = "Seaplane Bases", col = "blue", size = 0.5) +
  tm_shape(countyPop) + 
    tm_polygons(col = "Population", alpha = 0.5, style = "jenks", palette = "PuRd") +
  tm_shape(seaplaneBases) + 
    tm_symbols(col = "blue", size = 0.5) +
  tm_shape(labels) + tm_rgb() +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.frame = TRUE, 
    outer.margins = 0
    ) +
  tm_scale_bar(breaks = c(0, 50, 100, 200), position = c("right", "top")) +
  tm_compass(position = c("left", "top"))
  
tmap_save(map, filename = "WisconsinSeaplaneBases.jpg", dpi = 400)



# View Mode ---------------------------------------------------------------

tmap_mode("view")
tm_shape(seaplaneBases) + tm_dots()

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(countyPop) + 
    tm_polygons(col = "Population", alpha = 0.5, palette = "Blues", style = "log10_pretty") +
  tm_shape(seaplaneBases) + tm_dots(col = "red") +
  tm_add_legend(type = "fill", labels = "Seaplane Bases", col = "red")

  
  