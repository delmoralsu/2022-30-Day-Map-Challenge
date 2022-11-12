setwd("D:/Dropbox/Proyectos/Proyectos/30daymap")
library(rgdal)
library(rgeos)
library(sf)
library(ggplot2)

map <- readOGR("Data/concesionado_ruta_shp/Concesionado_Ruta.shp")
df <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")
metro <- readOGR("Data/metro/stcmetro_shp/STC_Metro_lineas_utm14n.shp")
rtp <- readOGR("Data/rtp/RTP_Rutas.shp")
pob <- readOGR("Data/pob_trans/pob_trans_conce.shp")
library(ggplot2)

pal <- brewer.pal(4, "RdYlGn")


library(RColorBrewer)
?RColorBrewer
rutasdf <- gIntersection(map, df)
rutasdf <- st_as_sf(rutasdf)
pob <- st_as_sf(pob)
pob

ggplot() +
  geom_sf(data = sc, fill = "darkgrey") +
  geom_sf(data = pob, aes(fill = porc)) +
  geom_sf(data = rutasdf) +
  scale_fill_manual(values = pal,
                    labels = c("0-25%", "25.1-50%", "50.1-75%", "75.1-100%", "ND"),
                    guide = guide_legend(keywidth = 7.5,
                    keyheight = 3,
                    title.position = "top",
                    label.position = "bottom", 
                    label.hjust = 0.5),
                    name = "Porcentaje de población atendida por colonia") +
  labs(title = "Población atendida por el transporte\n concesionado en CDMX - 2022",
       caption = "Fuentes: GobCDMX, SEMOVI e IPDP.") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text=element_text(size=30, face = "bold"),
        legend.title=element_text(size=38, face = "bold"),
        plot.title = element_text(size=45, face = "bold", hjust = 0.5),
        plot.caption = element_text(size=30, hjust = 0.9))
  
  
ggplot() +
  geom_sf(data = map, alpha = 0.5) +
  geom_polygon(data = df, aes(x = long, y = lat, group = group), colour = "black", size = 0.6, alpha = 1, fill = NA) +
  geom_sf(data = metro,
          aes(color = as.factor(metro$LINEA))) +
  geom_sf(data = rtp, color = "orange")

unique(pob$P_PCOBTC)
pob$porc <- cut(pob$P_PCOBTC,
                       breaks=c(0, 25, 50, 75, 100),
                       labels=c('0 - 25', '25.1 - 50', '50.1 - 75', '75.1 - 100'))

rtp$RUTA

ggsave("transporte1.jpg", width = 22.5, height = 30, unit = "in", dpi = 600)

max(pob$SUP_COL_M2)
which(pob$SUP_COL_M2 == 11857600)
library(tidyverse)
colonias <- pob %>%
  filter(P_PCOBTC == 0)

sc <- pob[1815,]
pob <- pob[-1815,]

plot(df, add = TRUE)


