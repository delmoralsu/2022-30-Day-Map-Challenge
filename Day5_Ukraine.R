library(openrouteservice)
library(mapview)
library(tidyverse)
library(osmdata)
library(sf)
library(paletteer)
library(ggfx)
install.packages("ggfx")
library()

library(osmdata)

install.packages("paletteer")
library(paletteer)
install.packages("openrouteservice")
install.packages("remotes")
library(remotes)
remotes::install_github("GIScience/openrouteservice-r")


ors_api_key("")
coordinates <- data.frame(lon = c(-99.21304240002354), lat = c(19.4165392746137))

cj_iso <- ors_isochrones(locations = coordinates, profile = "foot-walking", range = 6000, interval = 600, output = "sf")

mapviewOptions(fgb = FALSE)

intervals <- levels(factor(cj_iso$value))
cj_iso_list <- split(cj_iso, intervals)
cj_iso_list <- cj_iso_list[rev(intervals)]

names(cj_iso_list) <- sprintf("%s_min", as.numeric(names(cj_iso_list))/60)

mapview(cj_iso_list, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)

Mexico <- getbb("Mexico City")
Mexico
rm(dath)


streets <- Mexico %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary",
                            "trunk", "secondary_link", "tertiary_link",
                            "residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

cdmx_poly <- getbb("Mexico City", format_out = "polygon")

streets <- trim_osmdata(streets, cdmx_poly, exclude = TRUE)
sf_use_s2(FALSE)
rep.x <- function(i, na.rm=FALSE) {
  
  if(i == length(cj_iso_list)) {streets$osm_lines %>% st_intersection(cj_iso_list[[i]])}
  
  else if(i < length(cj_iso_list)) {streets$osm_lines %>% st_intersection(st_difference(cj_iso_list[[i]], cj_iso_list[[i+1]]))}
  
}

list_df <- lapply(1:length(cj_iso_list), rep.x)

iso_df <- dplyr::bind_rows(list_df)

colpal <- rev(paletteer_c("pals::ocean.tempo", 10))
Mexico
ggplot() +
  geom_sf(data = streets$osm_lines,
          color = "#151515",
          size = .2) +
  geom_sf(data = iso_df,
          aes(colour = as.factor(value),
              geometry = geometry),
          fill = "#060606",
          size = .2,
          alpha = .8) +
  scale_colour_manual(values = rev(colpal),
                      labels = seq(10,100,10),
                      guide = guide_legend(override.aes = list(fill = rev(colpal), alpha = 1),
                                           nrow = 1,
                                           keywidth = 3.5,
                                           keyheight = 1,
                                           title.position = "top",
                                           label.position = "bottom", 
                                           label.hjust = 0.5)) +
  coord_sf(xlim = Mexico[1,], 
           ylim = Mexico[2,],
           expand = FALSE)  +
  with_outer_glow(annotate(geom = "text", label = "Caminar alrededor de la embajada\n de Ucrania en CDMX",
                           x = -99.12251540189509, y = 19.56, size = 10, colour = colpal[10], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 10) +
  with_outer_glow(annotate(geom = "text", label = "¿Qué tan lejos se llega en 100 min?",
                           x = -99.13466741022717, y = 19.52, size = 9, colour = colpal[10], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 7) +
  with_outer_glow(annotate(geom = "text", label = "Fuente: openrouteservice.org por HeiGIT | Mapa: OpenStreetMap y contribuidores \n Tutorial: Jamie Hudson (@Jamie_Bio) ",
                           x = -99.13800379307892, y = 19.1, size = 4.5, hjust = 0.5, colour = colpal[10], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 4) +
  geom_point(aes(x = -99.2130424, y = 19.41740947634951), colour = "red", size = 0.45) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#060606"),
        panel.background = element_rect(fill = "#060606"),
        legend.text = with_outer_glow(element_text(colour = colpal[10],
                                                   family = "mono", size = 15),
                                      colour = colpal[4], sigma = 2, expand = 3),
        legend.title = element_blank(),
        legend.position=c(0.5, 0.8),
        legend.justification = "bottom",
        legend.direction = "horizontal")
ggsave("embajada.png", width = 10, height = 13, unit = "in")


embajadax <- c(-99.2367, -99.12)
embajaday <- c(19.35, 19.48)

embajadacoords <- rbind(embajadax,embajaday) 
colnames(embajadacoords) <- c("min", "max")



ggplot() +
  geom_sf(data = streets$osm_lines,
          color = "#151515",
          size = .2) +
  geom_sf(data = iso_df,
          aes(colour = as.factor(value),
              geometry = geometry),
          fill = "#060606",
          size = .2,
          alpha = .8) +
  scale_colour_manual(values = rev(colpal),
                      labels = seq(10,100,10),
                      guide = guide_legend(override.aes = list(fill = rev(colpal), alpha = 1),
                                           nrow = 1,
                                           keywidth = 3.5,
                                           keyheight = 1,
                                           title.position = "top",
                                           label.position = "bottom", 
                                           label.hjust = 0.5)) +
  coord_sf(xlim = embajadacoords[1,], 
           ylim = embajadacoords[2,],
           expand = FALSE)  +
  with_outer_glow(annotate(geom = "text", label = "Caminar alrededor de la embajada\n de Ucrania en CDMX",
                           x = -99.1737, y = 19.475, size = 9, hjust = 0.5, colour = colpal[10], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 10) +
  with_outer_glow(annotate(geom = "text", label = "¿Qué tan lejos se llega en 100 min?",
                           x = -99.1737, y =  19.4657, size = 7.5, colour = colpal[10], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 7) +
  with_outer_glow(annotate(geom = "text", label = "Fuente: openrouteservice.org por HeiGIT | Mapa: OpenStreetMap y contribuidores \n Tutorial: Jamie Hudson (@Jamie_Bio)",
                           x = -99.1723 , y = 19.3594,, size = 4, colour = colpal[10], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 4) +
  geom_point(aes(x = -99.2130424, y = 19.41740947634951), colour = "red", size = 2) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#060606"),
        panel.background = element_rect(fill = "#060606"),
        legend.text = with_outer_glow(element_text(colour = colpal[10],
                                                   family = "mono", size = 15),
                                      colour = colpal[4], sigma = 2, expand = 3),
        legend.title = element_blank(),
        legend.position=c(0.5, 0.8),
        legend.justification = "bottom",
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black", fill = "black"))

ggsave("embajada.png", width = 10, height = 13, unit = "in")
