library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf)
library(remotes)
library(osmdata)
library(OpenStreetMap)

## Importar datos del DENUE del INEGI, filtrar datos pertenecientes a CDMX
## y extraer información de negocios que tengan el código de comercio al por menor de libros,
## o que tengan "librería" o "libros" en el nombre.
##Por añadir: código para descargar, descomprimir y añadir

##Cargar DENUE1 
denue1cdmx <- read.csv("Data/DENUE/denue1.csv")
denue1cdmx <- filter(denue1cdmx, cve_ent == 9)
## Extraer datos de DENUE1
libros1cdmx <- filter(denue1cdmx, grepl("LIBROS", ignore.case = T, nom_estab))
libros1cdmx <- rbind(libros1cdmx, filter(denue1cdmx, grepl("LIBRERIA", ignore.case = T, nom_estab)))
libros1cdmx <- rbind(libros1cdmx, filter(denue1cdmx, grepl("LIBRERÍA", ignore.case = T, nom_estab)))
libros1cdmx <- rbind(libros1cdmx, filter(denue1cdmx, grepl("LIBROS", ignore.case = T, nombre_act)))
## Eliminar DENUE1
rm(denue1cdmx)

## Cargar DENUE2
denue2cdmx <- read.csv("Data/DENUE/denue2.csv")
denue2cdmx <- filter(denue2cdmx, cve_ent == 9)
## Extraer datos de DENUE2
libros2cdmx <- filter(denue2cdmx, grepl("LIBROS", ignore.case = T, nom_estab))
libros2cdmx <- rbind(libros2cdmx, filter(denue2cdmx, grepl("LIBRERIA", ignore.case = T, nom_estab)))
libros2cdmx <- rbind(libros2cdmx, filter(denue2cdmx, grepl("LIBRERÍA", ignore.case = T, nom_estab)))
libros2cdmx <- rbind(libros2cdmx, filter(denue2cdmx, grepl("LIBROS", ignore.case = T, nombre_act)))
## Eliminar DENUE2
rm(denue2cdmx)

## Cargar DENUE3
denue3cdmx <- read.csv("Data/DENUE/denue3.csv")
denue3cdmx <- filter(denue3cdmx, cve_ent == 9)
## Extraer datos
libros3cdmx <- filter(denue3cdmx, grepl("LIBROS", ignore.case = T, nom_estab))
libros3cdmx <- rbind(libros3cdmx, filter(denue3cdmx, grepl("LIBRERIA", ignore.case = T, nom_estab)))
libros3cdmx <- rbind(libros3cdmx, filter(denue3cdmx, grepl("LIBRERÍA", ignore.case = T, nom_estab)))
libros3cdmx <- rbind(libros3cdmx, filter(denue3cdmx, grepl("LIBROS", ignore.case = T, nombre_act)))
## Eliminar DENUE3
rm(denue3cdmx)

## Cargar DENUE4
denue4cdmx <- read.csv("Data/DENUE/denue4.csv")
denue4cdmx <- filter(denue4cdmx, cve_ent == 9)
## Extraer datos
libros4cdmx <- filter(denue4cdmx, grepl("LIBROS", ignore.case = T, nom_estab))
libros4cdmx <- rbind(libros4cdmx, filter(denue4cdmx, grepl("LIBRERIA", ignore.case = T, nom_estab)))
libros4cdmx <- rbind(libros4cdmx, filter(denue4cdmx, grepl("LIBRERÍA", ignore.case = T, nom_estab)))
libros4cdmx <- rbind(libros4cdmx, filter(denue4cdmx, grepl("LIBROS", ignore.case = T, nombre_act)))
## Eliminar DENUE4
rm(denue4cdmx)

## Crear una sola base que contenga toda la información extraída.
## Solamente en DENUE3 y DENUE4 había información sobre negocios que venden libros en CDMX.
libroscdmx <- rbind(libros3cdmx, libros4cdmx)
## Eliminar versiones previas
rm(libros1cdmx)
rm(libros2cdmx)
rm(libros3cdmx)
rm(libros4cdmx)

## Eliminar entradas duplicadas (porque se extrajo por nombre y por actividad)
libroscdmx <- libroscdmx[!duplicated(libroscdmx[, 1]),]

## Cargar shapefile CDMX
alcaldias <- readOGR("Data/alcaldias/alcaldias_cdmx.shp")

## Crear base con los datos que nos interesan
ubi_libros <- select(libroscdmx, id, nom_estab, raz_social, nom_vial, latitud, longitud)
unique(libroscdmx$raz_social)
colnames(libroscdmx3) <- c("lat", "long")
class(libroscdmx3$lat)

## Extraer librerías de Porrua
libporrua <- ubi_libros %>%
  filter(grepl("PORRUA", ignore.case = T, raz_social))
libporrua$raz_social <- "PORRUA"
ubi_libros <- ubi_libros %>%
  filter(!(id %in% libporrua$id))

## Extraer librerías de Fondo de Cultura
libFCE <- ubi_libros %>%
  filter(grepl("FONDO DE CULTURA ECONOMICA", ignore.case = T, raz_social))
libFCE$raz_social <- "FCE"
ubi_libros <- ubi_libros %>%
  filter(!(id %in% libFCE$id))

## Extraer librerías de Gandhi
libGan <- ubi_libros %>%
  filter(grepl("GANDHI", ignore.case = T, raz_social))
libGan$raz_social <- "GANDHI"
ubi_libros <- ubi_libros %>%
  filter(!(id %in% libGan$id))

## Extraer librerías en la calle de Donceles
libDon <- ubi_libros %>%
  filter(grepl("DONCELES", ignore.case = T, nom_vial))
ubi_libros <- ubi_libros %>%
  filter(!(id %in% libDon$id))
libDon$raz_social <- "DONCELES"

## El restante no perteneciente a ninguna categoría anterior
## Marcar como "Resto"
librerias <- ubi_libros
librerias$raz_social <- "Otro"

## Crear una base ya con la variable de tipo de librería, bajo el nombre razsoc
librerias <- rbind(librerias, libDon, libFCE, libporrua, libGan)
## Seleccionar columnas que nos interesan
librerias <- librerias %>%
  select(id, nom_estab, raz_social, longitud, latitud)
## Renombrarlas
colnames(librerias) <- c("id", "nom", "razsoc", "long", "lat")

## Mapa de CDMX con librerías
map <- ggplot() + 
  geom_polygon(data = alcaldias, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

librerias_cdmx <- map +
  geom_point(data = librerias, aes(x = long, y = lat), size = .65, alpha = .50,
             color = "blue") +
  theme_void() +
  theme(plot.title = element_text(size = 15, face="bold", hjust=.5),
        plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 6, vjust=10)) +
  labs(title = "¿Dónde comprar libros en CDMX?", subtitle = "Incluye librerías, puestos y otras tiendas", caption = "Fuentes: INEGI (05/2022), GobCDMX.") +
  coord_fixed(ratio=1)
## Guardar mapa 1
ggsave("librerias_cdmx.jpg", width = 15, height = 12, units = c("cm"), dpi = 300)

## Quitar Estacion de lectura FCE en Zócalo
librerias <- librerias %>%
  filter(id != 888653)


## Para realizar el acercamiento al centro de CDMX
## Coordenandas área Centro
lat1 <- 19.4280
lat2 <- 19.440
lon1 <- -99.15
lon2 <- -99.1285

## Hacer bounding box del Centro de CDMX
centro_bb <- matrix(data = c(-99.15, -99.1285, 19.40, 19.440),
                    nrow = 2,
                    byrow = TRUE)
colnames(centro_bb) <- c("min", "max")
rownames(centro_bb) <- c("x", "y")


## Descargar información de calles desde OSM

## Vias primarias
cdmx_big <- opq(centro_bb) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

## Vias secundarias
cdmx_medi <- opq(centro_bb) %>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()
## Vias terciarias
cdmx_small <- opq(centro_bb) %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                          "unclassified", "service", 
                          "footway")) %>%
            osmdata_sf()


## Crear mapa con calles
cdmx_centro <- ggplot() +
  geom_sf(data = cdmx_small$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .70,
          alpha = .5) +
geom_sf(data = cdmx_medi$osm_lines,
        inherit.aes = FALSE,
        color = "darkgrey",
        size = .85,
        alpha = .80) +
  geom_sf(data = cdmx_big$osm_lines,
          inherit.aes = FALSE,
          color = "darkgrey",
          size = 1)
  
## Terminar el mapa, con leyenda, colores y títulos
librerias_centro <- cdmx_centro + 
  geom_point(data = librerias, aes(x = long, y = lat, color = razsoc), size = 2) + 
  coord_sf(xlim=c(lon1, lon2), ylim=c(lat1, lat2)) +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
  theme_void() +
  theme(plot.title = element_text(size = 12, hjust=.5),
        plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.caption = element_text(size = 6, vjust=3),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        legend.position = "bottom") +
  labs(title = "¿Dónde comprar libros en CDMX?", subtitle = "Alrededores del Centro Histórico, incluye librerías, puestos y otras tiendas", caption = "Fuentes: INEGI (05/2022), GobCDMX, OpenStreetMap.",
       color = "")
  
## Guardar mapa 2
ggsave("librerias_centro.jpg", width = 15, height = 12, units = c("cm"), dpi = 300)
  
