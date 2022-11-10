setwd("D:/Dropbox/Proyectos/Proyectos/30daymap")
library(rgdal)
library(rgeos)
library(sf)
library(ggplot2)
library(rasterVis)
library(grid)

library(viridis)  # better colors for everyone
install.packages("ggthemes")
library(ggthemes)

map <- readOGR("Data/densidadpoblacion2010/densidadpoblacion2010.shp")


map <- st_as_sf(map)
ggplot() +
  geom_tile(data = map, aes(x = long, y = lat))

fr<-rasterize(map, cr), cr)
data <- as(data, "SpatialPixelsDataFrame")
data_df <- as.data.frame(data)
test_spdf <- as(test, "SpatialPixelsDataFrame")
colnames(data_df) <- c("ele", "x", "y")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
df <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")

range(data_df$ele)
terrain <- terrain.colors(4)
ggplot() +  
  geom_tile(data=data_df, aes(x=x, y=y, fill=ele), alpha=0.9) +
  geom_polygon(data = df, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_viridis() +
  coord_equal() +
  labs(title = "Elevación en CDMX", fill = "Elevación en\n metros",
       caption = "Fuentes: GobCDMX e INEGI (CEM 3.0)") +
  theme_void() +
  theme(legend.position="bottom",
        legend.text=element_text(size=15, face = "bold"),
        plot.title = element_text(size=20, face = "bold", hjust = 0.5),
        plot.caption = element_text(size=10, hjust = 0.9),
        legend.key.width = unit(0.50, "in"))

  ?guide_colourbar
        
?scale_fill_viridis_b

geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
             fill=NA, color="grey50", size=0.25) +

  scale_fill_viridis_b(option = "magma")
  
ggsave("elevacion.jpg", width = 10, height = 13, units = c("in"), dpi = 600)  


                                                         library(raster)
data <- raster("Data/rastercem/CiudadMexico_r15m.tif")
plot(data)

ggplot() +
  geom_tile(data = data, aes(fill = factor))