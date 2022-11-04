library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf)
library(broom)

setwd("D:/Dropbox/Proyectos/Proyectos/30daymap/")

## Importar datos del DENUE del INEGI, filtrar datos pertenecientes a CDMX
## y extraer información de negocios que tengan el código de comercio al por menor de libros,
## o que tengan "librería" o "libros" en el nombre.
##Por añadir: código para descargar, descomprimir y añadir
## DENUE de comercio al por menor
denue1cdmx <- read.csv("Data/DENUE/denue1.csv")
denue1cdmx <- filter(denue1cdmx, cve_ent == 9)

tacos1cdmx <- filter(denue1cdmx, grepl("TACOS", ignore.case = T, nom_estab))
tacos1cdmx <- rbind(tacos1cdmx, filter(denue1cdmx, grepl("TAQUERIA", ignore.case = T, nom_estab)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denue1cdmx, grepl("TAQUERÍA", ignore.case = T, nom_estab)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denue1cdmx, grepl("TACOS", ignore.case = T, nombre_act)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denue1cdmx, grepl("TAQUERÍA", ignore.case = T, nombre_act)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denue1cdmx, grepl("TAQUERIA", ignore.case = T, nombre_act)))

rm(denue1cdmx)

denue2cdmx <- read.csv("Data/DENUE/denue2.csv")
denue2cdmx <- filter(denue2cdmx, cve_ent == 9)

tacos2cdmx <- filter(denue2cdmx, grepl("TACOS", ignore.case = T, nom_estab))
tacos2cdmx <- rbind(tacos2cdmx, filter(denue2cdmx, grepl("TAQUERIA", ignore.case = T, nom_estab)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denue2cdmx, grepl("TAQUERÍA", ignore.case = T, nom_estab)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denue2cdmx, grepl("TACOS", ignore.case = T, nombre_act)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denue2cdmx, grepl("TAQUERÍA", ignore.case = T, nombre_act)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denue2cdmx, grepl("TAQUERIA", ignore.case = T, nombre_act)))

rm(denue2cdmx)

denue3cdmx <- read.csv("Data/DENUE/denue3.csv")
denue3cdmx <- filter(denue3cdmx, cve_ent == 9)

tacos3cdmx <- filter(denue3cdmx, grepl("TACOS", ignore.case = T, nom_estab))
tacos3cdmx <- rbind(tacos3cdmx, filter(denue3cdmx, grepl("TAQUERIA", ignore.case = T, nom_estab)))
tacos3cdmx <- rbind(tacos3cdmx, filter(denue3cdmx, grepl("TAQUERÍA", ignore.case = T, nom_estab)))
tacos3cdmx <- rbind(tacos3cdmx, filter(denue3cdmx, grepl("TACOS", ignore.case = T, nombre_act)))
tacos3cdmx <- rbind(tacos3cdmx, filter(denue3cdmx, grepl("TAQUERÍA", ignore.case = T, nombre_act)))
tacos3cdmx <- rbind(tacos3cdmx, filter(denue3cdmx, grepl("TAQUERIA", ignore.case = T, nombre_act)))

rm(denue3cdmx)

denue4cdmx <- read.csv("Data/DENUE/denue4.csv")
denue4cdmx <- filter(denue4cdmx, cve_ent == 9)

tacos4cdmx <- filter(denue4cdmx, grepl("TACOS", ignore.case = T, nom_estab))
tacos4cdmx <- rbind(tacos4cdmx, filter(denue4cdmx, grepl("TAQUERIA", ignore.case = T, nom_estab)))
tacos4cdmx <- rbind(tacos4cdmx, filter(denue4cdmx, grepl("TAQUERÍA", ignore.case = T, nom_estab)))
tacos4cdmx <- rbind(tacos4cdmx, filter(denue4cdmx, grepl("TACOS", ignore.case = T, nombre_act)))
tacos4cdmx <- rbind(tacos4cdmx, filter(denue4cdmx, grepl("TAQUERÍA", ignore.case = T, nombre_act)))
tacos4cdmx <- rbind(tacos4cdmx, filter(denue4cdmx, grepl("TAQUERIA", ignore.case = T, nombre_act)))

rm(denue4cdmx)

tacoscdmx <- rbind(tacos1cdmx, tacos2cdmx, tacos3cdmx, tacos4cdmx)
rm(tacos1cdmx, tacos2cdmx, tacos3cdmx, tacos4cdmx)

## DENUE de preparación de alimentos
denuealimentos1 <- read.csv("Data/DENUE/denue_alimentos1.csv")
denuealimentos1 <- filter(denuealimentos1, cve_ent == 9)

tacos1cdmx <- filter(denuealimentos1 , grepl("TACOS", ignore.case = T, nom_estab))
tacos1cdmx <- rbind(tacos1cdmx, filter(denuealimentos1 , grepl("TAQUERIA", ignore.case = T, nom_estab)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denuealimentos1 , grepl("TAQUERÍA", ignore.case = T, nom_estab)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denuealimentos1 , grepl("TACOS", ignore.case = T, nombre_act)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denuealimentos1 , grepl("TAQUERÍA", ignore.case = T, nombre_act)))
tacos1cdmx <- rbind(tacos1cdmx, filter(denuealimentos1 , grepl("TAQUERIA", ignore.case = T, nombre_act)))

rm(denuealimentos1)

denuealimentos2 <- read.csv("Data/DENUE/denue_alimentos2.csv")
denuealimentos2 <- filter(denuealimentos2, cve_ent == 9)

tacos2cdmx <- filter(denuealimentos2 , grepl("TACOS", ignore.case = T, nom_estab))
tacos2cdmx <- rbind(tacos2cdmx, filter(denuealimentos2, grepl("TAQUERIA", ignore.case = T, nom_estab)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denuealimentos2, grepl("TAQUERÍA", ignore.case = T, nom_estab)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denuealimentos2, grepl("TACOS", ignore.case = T, nombre_act)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denuealimentos2, grepl("TAQUERÍA", ignore.case = T, nombre_act)))
tacos2cdmx <- rbind(tacos2cdmx, filter(denuealimentos2, grepl("TAQUERIA", ignore.case = T, nombre_act)))
rm(denuealimentos2)
rm(tacos2cdmx)

## Crear una base con todas las taquerías
tacoscdmxfull <- rbind(tacoscdmx, tacos1cdmx)
rm(tacos1cdmx, tacoscdmx)

## Eliminar entradas duplicadas
tacoscdmxfull <- tacoscdmxfull[!duplicated(tacoscdmxfull[, 1]),]

## Seleccionar variables que nos interesan
tacoscdmxfull <- tacoscdmxfull %>%
  select(id, latitud, longitud)

## Cargar shapefiles de las colonias de CDMX
colonias <- read_sf("Data/Map/mgpc_2019.shp")
colonias2 <- readOGR("Data/georef_colonias/georef-mexico-colonia-millesime.shp")

## Reordenar variables para tener longitud y luego latitud
tacoscdmxubi <- tacoscdmxfull[, 2:3]
tacoscdmxubi <- tacoscdmxubi[, c(2,1)]

## Determinar intersección entre ubicaciones de tacos y polígonos de colonias
## para saber cuántos tacos por colonia

res <- over(tacoscdmxubi, colonias)

## Transformar datos a base espacial
tacoscdmxubi <- SpatialPointsDataFrame(tacoscdmxubi, data.frame(id=1:11197))

## Averiguar tipo de proyección de segundo SHP.
## readOGR conserva proyección
proj4string(colonias2)

## Asignar proyección a base espacial
proj4string(tacoscdmxubi) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
rm(tacoscdmxubi)

## Determinar intersección de tacos con polígonos de colonias
res <- over(tacoscdmxubi, colonias2)

## Transformar datos a data.frame
taquerias <- data.frame(res)

## Transformar a taquerias totales por colonia
taqueriaspop <- taquerias %>%
  count(col_code)

## Crear dataframe con población por colonia
pop <- data.frame(colonias)
pop <- pop %>%
  dplyr::select(CVEUT, POB2010)

## Renombrar variable de identificación de colonia en base de
## taquerías
colnames(taqueriaspop) <- c("CVEUT", "Total")

## Unir bases de población y taquerías
taqueriaspop <- merge(taqueriaspop, pop, by = "CVEUT")

## Calcular taquerías per capita
taqueriaspop <- taqueriaspop %>%
  mutate(Percapita = ((Total/as.numeric(POB2010))))

## Revisar nombres y que no haya NAs en Percapita
colnames(taqueriaspop) <- c("col_code", "Total", "POB2010", "Percapita")
taqueriaspop[is.na$Percapita] <- 0

## Crear base final, con información espacial, de taquerías y de
## población
colonias_fortified <- tidy(colonias2, region = "col_code")
colonias_fortified <- colonias_fortified %>%
  left_join(., taqueriaspop, by = c("id" ="col_code"))

## Agregar SHAPEFILE de alcaldías, para dibujar límites de CDMX
alcaldias <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")

## Primer mapa: total de taquerías
ggplot() + 
  geom_polygon(data = alcaldias, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_polygon(data = colonias_fortified, aes(fill = Total, x = long, y = lat, group = group), colour = "black") +
  theme_void() +
  theme(plot.title = element_text(size = 38, face="bold", hjust=.5),
        plot.subtitle = element_text(size = 32, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 15, vjust=10),
        legend.position = "right",
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        legend.key.size = unit(2, 'cm')) +
  labs(title = "Taquerías en CDMX", subtitle = "Total por colonia", caption = "Fuentes: INEGI DENUE (05/2022) & GobCDMX.")

ggsave("tacos_total.jpg", width = 15, height = 15) 

## Segundo mapa: taquerías per capita
ggplot() + 
  geom_polygon(data = alcaldias, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_polygon(data = colonias_fortified, aes(fill = Percapita, x = long, y = lat, group = group), colour = "black") +
  theme_void() +
  theme(plot.title = element_text(size = 38, face="bold", hjust=.5),
        plot.subtitle = element_text(size = 32, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 15, vjust=10),
        legend.position = "right",
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        legend.key.size = unit(2, 'cm')) +
  labs(title = "Taquerías en CDMX", subtitle = "Per capita por colonia (población 2010)", caption = "Fuentes: INEGI DENUE (05/2022) & GobCDMX.")

ggsave("tacos_percapita.jpg", width = 15, height = 15)

