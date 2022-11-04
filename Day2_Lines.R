library(osmdata)
library(sf)
library(tidyverse)
library(dplyr)
library(rgdal)

## Definir la bounding box en forma de polígono para los límites de CDMX
cdmx_poly <- getbb("Mexico City", format_out = "polygon")

## Extraer datos de calles desde Open Street Maps, según jerarquía de calles
bigstreets <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="highway", value=c("motorway",
  "primary")) %>%
  osmdata_sf()

mediumstreets <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="highway", value=c("secondary")) %>%
  osmdata_sf()

smallstreets <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="highway", value=c("tertiary", "residential")) %>%
  osmdata_sf()

smallest <- opq(bbox="Mexico City") %>%
  add_osm_feature(key="highway", value="unclassified") %>%
  osmdata_sf()


## Recortar la información de las calles para que no sobresalgan de los límites
## administrativos de la CDMX
bigstreets <- trim_osmdata(bigstreets, cdmx_poly, exclude = TRUE)
mediumstreets <- trim_osmdata(mediumstreets, cdmx_poly, exclude = TRUE)
smallstreets <- trim_osmdata(smallstreets, cdmx_poly, exclude = TRUE)
smallest <- trim_osmdata(smallest, cdmx_poly, exclude = TRUE)

## Leer shapefile de los límites administrativos de CDMX

alcaldias <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")

## Hacer mapa, capa por capa

ggplot() +
  geom_polygon(data = alcaldias, aes(x = long, y = lat, group = group), colour = "yellow", size = 0.1, alpha = 0.5, fill = NA) +
  geom_sf(data = mediumstreets$osm_lines,
          inherit.aes = FALSE,
          color = "#DAA520",
          size = .3,
          alpha = .5) +
  geom_sf(data = smallstreets$osm_lines,
          inherit.aes = FALSE,
          color = "#DAA520",
          size = .2,
          alpha = 0.3) +
  geom_sf(data = smallest$osm_lines,
          inherit.aes = FALSE,
          color = "#DAA520",
          size = .2,
          alpha = 0.3) +
  geom_sf(data = bigstreets$osm_lines,
          inherit.aes = FALSE,
          color = "#DAA520",
          size = .33,
          alpha = .8) +
  theme_void()+
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.caption = element_text(size = 30, color = "white")) +
  labs(caption = "Fuentes: Open Street Maps & GobCDMX.")

## Guardar mapa

ggsave("calles_cdmx.png", width = 15, height = 15)
  
