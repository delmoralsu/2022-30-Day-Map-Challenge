library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf)

setwd("D:/Dropbox/Proyectos/Proyectos/30daymap/")

## Importar datos del DENUE del INEGI, filtrar datos pertenecientes a CDMX
## y extraer información de negocios que tengan el código de comercio al por menor de libros,
## o que tengan "librería" o "libros" en el nombre.
##Por añadir: código para descargar, descomprimir y añadir
rm(ageb)
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

tacoscdmxfull <- rbind(tacoscdmx, tacos1cdmx)

tacoscdmxfull <- tacoscdmxfull[!duplicated(tacoscdmxfull[, 1]),]

tacoscdmxfull <- tacoscdmxfull %>%
  select(id, latitud, longitud)

colonias <- read_sf("Data/Map/mgpc_2019.shp")

tacoscdmxubi <- tacoscdmxfull[, 2:3]
tacoscdmxubi <- tacoscdmxubi[, c(2,1)]

res <- over(tacoscdmxubi, colonias)

colonias <- read_sf("Data/Map/mgpc_2019.shp")
colonias2 <- readOGR("Data/georef_colonias/georef-mexico-colonia-millesime.shp")

proj4string(colonias2)
rm(tacoscdmxubi)

tacoscdmxubi <- tacoscdmxfull[, 2:3]
tacoscdmxubi <- tacoscdmxubi[, c(2,1)]

tacoscdmxubi <- SpatialPointsDataFrame(tacoscdmxubi, data.frame(id=1:11197))

proj4string(tacoscdmxubi) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
intersection <- st_intersection(colonias2, tacoscdmxubi)
proj4string(colonias2)
plot(tacoscdmxubi, col = "red", add = TRUE)


proj4string(colonias)
library(move)
res <- over(tacoscdmxubi, colonias2)

ggplot() +
  geom_sf(data = colonias, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
table(res$col_name)

taquerias <- data.frame(res)

map
gageb <- read_sf("Data/ageb/u_territorial_agebs_scince_inegi_2010.shp")
gageb <- filter(gageb, gageb$cvegeoedo == "09")



colonias$DTTOLOC
  


coloniastodo <- colonias$NOMUT
num <- seq(0, 1814)

coloniastodo <- colonias$NOMUT
coloniasnomdt <- colonias$NOMDT
coloniasnun <- colonias$CVEUT
acorregir <- cbind(coloniastodo, coloniasnomdt, coloniasnun)

library(xlsx)
write.xlsx(acorregir, "base.xlsx")
write.xlsx(tacoscdmx, "acorregir.xlsx")

coloniastacos <- unique(tacoscdmx$nomb_asent)

coloniastodo

i <- 0
x <- 0
for (i in 0:length(tacoscdmx$nomb_asent)){
  x <- sum(colonias$NOMUT %in% tacoscdmx$nomb_asent[i])
  if ( x == 0){
    print(tacoscdmx$nomb_asent[i])
  }
}

arrange(tacoscdmx$nomb_asent)

alcaldias <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")


