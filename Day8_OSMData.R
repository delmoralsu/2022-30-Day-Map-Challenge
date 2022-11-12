library(osmdata)
library(rgdal)
library(tidyverse)
library(sf)
install.packages("cowplot")
library(cowplot)

setwd("D:/Dropbox/Proyectos/Proyectos/30daymap")
df <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")

df <- st_as_sf(df)

cuauhtemoc_poly <- df %>%
  filter(nomgeo == "CUAUHTÉMOC")

cuauhtemoc <- getbb("Delegación Cuauhtémoc")
x <- c(-99.1901, -99.1212)
y <- c(19.3925, 19.4677)
cuauhtemoc <- rbind(x, y)
colnames(cuauhtemoc) <- c("min", "max")
cuauhtemoc

cuauhtemoc_mainstreets <- opq(cuauhtemoc) %>%
  add_osm_feature(key="highway", value=c("motorway",
                                         "primary")) %>%
  osmdata_sf()

cuauhtemoc_medium <- opq(bbox=cuauhtemoc) %>%
  add_osm_feature(key="highway", value=c("secondary")) %>%
  osmdata_sf()

cuauhtemoc_small <- opq(bbox=cuauhtemoc) %>%
  add_osm_feature(key="highway", value=c("tertiary", "residential")) %>%
  osmdata_sf()

cuauhtemoc_smallest <- opq(bbox=cuauhtemoc) %>%
  add_osm_feature(key="highway", value="unclassified") %>%
  osmdata_sf()

bigstreets <- trim_osmdata(cuauhtemoc_mainstreets, cuauhtemoc_poly, exclude = TRUE)
mediumstreets <- trim_osmdata(cuauhtemoc_medium, cuauhtemoc_poly, exclude = TRUE)
small <- trim_osmdata(cuauhtemoc_small, cuauhtemoc_poly, exclude = TRUE)
smallest <- trim_osmdata(cuauhtemoc_smallest, cuauhtemoc_poly, exclude = TRUE)

cuauhtemoc_bars <- opq(bbox=cuauhtemoc) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf()

cuauhtemoc_restos <- opq(bbox=cuauhtemoc) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()

cuauhtemoc_parks <- opq(bbox=cuauhtemoc) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

bars <- trim_osmdata(cuauhtemoc_bars, cuauhtemoc_poly, exclude = TRUE)
restaurants <- trim_osmdata(cuauhtemoc_restos, cuauhtemoc_poly, exclude = TRUE)
parks <- trim_osmdata(cuauhtemoc_parks, cuauhtemoc_poly, exclude = TRUE)

library(ggplot2)
bares <- ggplot() +
  geom_sf(data = cuauhtemoc_poly) +
  geom_sf(data = parks$osm_polygons,
          inherit.aes = FALSE,
          fill = "#22BC22") +
  geom_sf(data = bigstreets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = 1) +
  geom_sf(data = mediumstreets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = smallest$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = bars$osm_points,
          inherit.aes = FALSE,
          color = "red",
          size = .8,
          alpha = 1) +
  theme_void()
bares
  

restos <- ggplot() +
  geom_sf(data = cuauhtemoc_poly) +
  geom_sf(data = parks$osm_polygons,
          inherit.aes = FALSE,
          fill = "#22BC22") +
  geom_sf(data = bigstreets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = 1) +
  geom_sf(data = mediumstreets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = smallest$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = restaurants$osm_points,
          inherit.aes = FALSE,
          color = "blue",
          size = .8,
          alpha = 1) +
  theme_void() +
  labs(caption = "Fuente: Open Street Map") +
  theme(plot.caption = element_text(hjust = 0.5))
  
restos
  


mapa <- plot_grid(bares, restos, labels = c("Bares", "Restaurantes"))


mapa
title <- ggdraw() + 
  draw_label(
    "Miles per gallon decline with displacement and horsepower",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, mapa,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

title
ggsave("bares.jpg", width = 7.5, height = 10)
