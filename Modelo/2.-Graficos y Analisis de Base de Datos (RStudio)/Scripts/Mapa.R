library(rgdal)
library(RColorBrewer)
library(classInt)
library(RColorBrewer)

Data_1DPA <- read_excel("D:/Mateo Heras/Escritorio/Proyectos/Tasa retorno educacion Ecuador/Datos/Data_1DPA.xlsx")

dirmapas <- "D:/Mateo Heras/Escritorio/DATOS/Econometria/Mapas/2012_nacionalporprovincias"
setwd(dirmapas)

Poligonos <- readOGR("nxprovincias.shp" , layer="nxprovincias")
View(Poligonos@data)

name_prov <- c ("Azuay", "Bolivar" , "Canar" ,"Carchi" ,"Cotopaxi", "Chimborazo", "El Oro" ,"Esmeraldas" ,"Guayas", "Imbabura" ,"Loja" ,"Los Rios" ,"Manabi" ,"Morona Santiago", "Napo", "Pastaza" ,"Pichincha" ,"Tungurahua" ,"Zamora Chinchipe","Galapagos" ,"Sucumbios" ,"Orellana" ,"Santo Domingo de los Tsachilas" ,"Santa Elena" ,"Zona no delimitada")
Poligonos@data$DPA_DESPRO <- name_prov

Poligonos <- Poligonos[Poligonos$DPA_PROVIN != 20,]
Poligonos <- Poligonos[Poligonos$DPA_DESPRO != "Zona no delimitada",]
Data_1DPA <- Data_1DPA[Data_1DPA$DPA!= 20,]
Data_1DPA <- Data_1DPA[Data_1DPA$DPA!= 90,]
#Emparejar datos
Data_1DPA$DPA <- Poligonos$DPA_PROVIN

#Sacar la variable a graficar
dat_map <-round(Data_1DPA[,2], digits = 2)
View(dat_map)

#Tratamiento de datos para la grafica
dat_map <- as.data.frame(dat_map)
names(dat_map)<- "dat_map" 
row.names(dat_map)<- row.names(Poligonos)

Poligonos.data <- SpatialPolygonsDataFrame(Poligonos,dat_map)

#graficamos 
my.palette <- brewer.pal(n = 9, name = "OrRd")
spplot(Poligonos.data, main = "Promedio de años de educacion", sub = "Pais:Ecuador", 
       col.regions = my.palette, cuts = 7)

#display.brewer.all() #colores
