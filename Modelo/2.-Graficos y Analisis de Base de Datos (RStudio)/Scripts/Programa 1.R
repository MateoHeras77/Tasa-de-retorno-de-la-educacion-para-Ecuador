library(ggridges)
library(dplyr)
library(ggplot2)
library(readxl)
library(haven)
library(rgl)
library(PerformanceAnalytics)
library(corrplot)

#Los datos se encuentran en la carpeta
Data <- read_excel("Datos/Datos.xls")
names(Data)


# Tratamiento de la base de datos -----------------------------------------

#Cambiar nombre de columnas

colnames(Data) <- c("Area","Ciudad","Sexo","Edad","Estado_Civil",
                    "Educacion","Año_Aprobado","Experiencia_Laboral","Ingresos", 
                    "Años_de_Educacion","Experiencia_Laboral_2")
summary(Data)

#Tratamiento de NA values
Data<-na.omit(Data) #elimicaion de valores faltantes
summary(Data)

#Convertir datos de letras a numeros
Data$Ingresos<- as.numeric(as.character(Data$Ingresos))
Data$Experiencia_Laboral <- as.numeric(as.character(Data$Experiencia_Laboral))
Data$Edad <- as.numeric(as.character(Data$Edad))
summary(Data)



# Grafico de variables ----------------------------------------------------

#Sexo x Edad

ggplot(Data,aes(Sexo,Edad,fill=Sexo)) + geom_violin() +
geom_boxplot(width=0.1)+  scale_fill_brewer(palette="Blues")+
  labs(title="Relación entre Sexo  y Edad") + theme_classic()



#Nivel de estudios INSTRUCCION x EDAD


Data$Educacion <-factor(Data$Educacion,levels= c("Centro de alfabetizacion","Primaria","Secundaria","Educacion Basica",
                                                                       "Educacion  Media","Superior no universitario","Superior Universitario","Post-grado"))

levels(Data$Educacion) <- c("Centro de alfabetizacion","Primaria","Secundaria","Educacion Basica",
                                       "Educacion  Media","Superior no universitario","Superior Universitario","Post-grado")

ggplot(Data, aes(x = Edad, y = Educacion, fill = Educacion)) +
  geom_density_ridges() +
  labs(title = "Nivel de Educacion", 
       subtitle = "¿Cual es la distribución del nivel de Educacion con relación a la edad?",
       x = "Edad", 
       y = "Nivel de Educacion" ) +
  scale_color_manual(values=c('#25AAE2','#F2B53A', '#8BC540', '#DC5D42', '#666666',
                              '9FAFBE'))

#Nivel de estudios INSTRUCCION x INGRESOS


Data$Educacion <-factor(Data2$Educacion,levels= c("Centro de alfabetizacion","Primaria","Secundaria","Educacion Basica",
                                                                       "Educacion  Media","Superior no universitario","Superior Universitario","Post-grado"))

levels(Data2$Educacion) <- c("Centro de alfabetizacion","Primaria","Secundaria","Educacion Basica",
                                       "Educacion  Media","Superior no universitario","Superior Universitario","Post-grado")

ggplot(Data2, aes(x = Ingresos, y = Educacion, fill = Educacion)) +
  geom_density_ridges() +
  labs(title = "Nivel de Ingresos", 
       subtitle = "¿Cual es la distribución del nivel de educacion con relación a las ingresos?",
       x = "Ingresos", 
       y = "Nivel de Educacion" ) +
  scale_color_manual(values=c('#25AAE2','#F2B53A', '#8BC540', '#DC5D42', '#666666',
                              '9FAFBE'))



#EXPERIENCIA X EDAD

ggplot(Data, aes(x=Edad, y=Experiencia_Laboral_2 , color=Experiencia_Laboral_2)) + 
  geom_point(alpha=0.4) +
  scale_size_continuous( trans="exp") +
  scale_colour_continuous(guide = FALSE) +
  labs(title="Relación entre Experiencia Laboral y Edad",
       y="Experiencia Laboral", 
       x="Edad")


#INGRESOS X EXPERIENCIA


Data2<-filter(select(Data,Ingresos,Experiencia_Laboral,Edad,Educacion,Ciudad), Ingresos <=10000)

ggplot(Data2, aes(x=Edad, y= Ingresos , color=Ingresos)) + 
  geom_point(alpha=0.4) +
  scale_size_continuous( trans="exp") +
  scale_colour_continuous(guide = FALSE) +
  labs(title="Relación entre Ingresos y Edad")










# Grafica en 3D -----------------------------------------------------------
Data2<-filter(select(Data,Ingresos,Experiencia_Laboral,Edad,Educacion,Ciudad,Experiencia_Laboral_2), Ingresos <=10000)
names(Data2)

# Function to interleave the elements of two vectors
interleave <- function(v1, v2) as.vector(rbind(v1,v2))

# Plot the points
plot3d(Data2$Experiencia_Laboral_2, Data2$Edad, Data2$Ingresos,
       xlab="Experiencia Laboral", ylab="Edad", zlab="Ingresos", col = "#F2CB05",
       size=0.5, type="s", lit=FALSE)


# Draw the box.
rgl.bbox(color="#6960A6", # grey60 surface and black text
         emission="#6960A6", # emission color is grey50
         xlen=0, ylen=0, zlen=0) # Don't add tick marks

# Set default color of future objects to black
rgl.material(color="black")

# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges=c("x--", "y+-", "z--"),
       ntick=6, # Attempt 6 tick marks on each side
       cex=.75) # Smaller font

# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Experiencia Laboral", edge="x--", line=2)
mtext3d("Edad", edge="y+-", line=3)
mtext3d("Ingresos", edge="z--", line=3)


# Correlaciones -----------------------------------------------------------
Data3<-filter(select(Data,Ingresos,Experiencia_Laboral,Edad,Años_de_Educacion))

Data_Cor<-round(cor(Data3, method = "pearson"), digits = 2)

corrplot(Data_Cor, addCoef.col = "black")

#method = "shade, "circle", "ellipse", "pie"
# order="AOE", "FPC", "hclust"

corrplot(Data_Cor, method = "pie",
         shade.col=NA, tl.col="Black",
         tl.str=45,
         addCoef.col = "black", addcolorlabel="no",
         order="AOE",
         type = "upper",
         diag=F)


# Exportacion de Datos ----------------------------------------------------

summary(Data3)

which(is.na(Data))#Verificar NA values
write.csv(Data, "Data_2.csv")

