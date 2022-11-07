library(osmdata)
library(sf)
library(tidyverse)
library(dplyr)
library(rgdal)



cdmx_poly <- getbb("Mexico City", format_out = "polygon")

parks <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="leisure", value=c("garden",
                                         "golf_course",
                                         "nature_reserve",
                                         "park", "pitch")) %>% 
  osmdata_sf()

park <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="leisure", value="park") %>% 
  osmdata_sf()

forest <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="landuse", value=c("forest")) %>% 
  osmdata_sf()

trees <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="landcover", value=c("trees")) %>% 
  osmdata_sf()


reserva <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="leisure", value="nature_reserve") %>% 
                    osmdata_sf()

landuse <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="landuse", value=c("cemetery",
                                         "farmland",
                                         "forest",
                                         "greenfield", "meadow",
                                         "orchard", "recreation_ground",
                                         "village_green", "vineyard")) %>% 
  osmdata_sf()

allotment <- opq(bbox = "Mexico City") %>%
  add_osm_feature(key="landuse", value= "allotments") %>%
  osmdata_sf()


natural <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="natural", value=c("wood",
                                         "scrub",
                                         "health",
                                         "grassland", "wetland")) %>% 
  osmdata_sf()

cemetery <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="landuse", value="cemetery") %>%
  osmdata_sf()

graveyard <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="amenity", value=c("graveyard")) %>% 
  osmdata_sf()


setwd("D:/Dropbox/Proyectos/Proyectos/30daymap")
alcaldias <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")

cemetery, natural, reserva, parks, wood

cemetery <- trim_osmdata(cemetery, cdmx_poly, exclude = TRUE)
natural <- trim_osmdata(natural, cdmx_poly, exclude = TRUE)
reserva <- trim_osmdata(reserva, cdmx_poly, exclude = TRUE)
parks <- trim_osmdata(parks, cdmx_poly, exclude = TRUE)
wood <- trim_osmdata(wood, cdmx_poly, exclude = TRUE)
landuse <- trim_osmdata(landuse, cdmx_poly, exclude = TRUE)


ggplot() +
  geom_polygon(data=suelo, aes(x = long, y = lat, group = group, fill = "#B1D8B7"), colour = "#B1D8B7") + 
  geom_sf(data = landuse$osm_multipolygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = landuse$osm_polygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = parks$osm_multipolygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = parks$osm_polygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = reserva$osm_multipolygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = reserva$osm_polygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = natural$osm_polygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = natural$osm_multipolygons,
          inherit.aes = FALSE, aes(
            fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = cemetery$osm_multipolygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_sf(data = cemetery$osm_polygons,
          inherit.aes = FALSE, aes(
          fill = "darkgreen"),
          color = "darkgreen",
          size = 1,
          alpha = 1) +
  geom_polygon(data = alcaldias, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, alpha = 1, fill = NA) +
  theme_void() +
  scale_fill_manual(name = "",
                       values = c("#B1D8B7", "darkgreen"),
                       labels = c("Suelo de conservación", "Areas verdes"),
                       guide = "legend") +
  theme(plot.title = element_text(hjust = 0.6, size = 28, face = "bold", vjust = -.40),
    plot.caption = element_text(size = 11, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size=20)) +
  labs(title = "Áreas verdes y suelo de \n conservación en CDMX", caption = "Fuentes: OpenStreetMap & GobCDMX.")


  geom_sf(data = graveyard$osm_polygons,
          inherit.aes = FALSE,
          color = "blue",
          size = .3,
          alpha = .5) +
  geom_sf(data = allotments$osm_polygons,
          inherit.aes = FALSE,
          color = "blue",
          size = .3,
          alpha = .5) +
  geom_sf(data = trees$osm_polygons,
          inherit.aes = FALSE,
          color = "green",
          size = .3,
          alpha = .5) +
    geom_sf(data = wood$osm_multipolygons,
            inherit.aes = FALSE,
            
            fill = "darkgreen",
            size = .3,
            alpha = .5) +
    geom_sf(data = wood$osm_polygons,
            inherit.aes = FALSE,
            color = "darkgreen",
            fill = "darkgreen",
            size = .3,
            alpha = .5)
  
  geom_polygon(data=suelo, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  
  
ggsave("verdes.jpg", width = 10, height = 15, unit = "in")
