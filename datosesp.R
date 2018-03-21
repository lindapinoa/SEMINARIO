install.packages("maps", lib="D:/R")
install.packages("maptools", lib="D:/R")
install.packages("sp", lib="D:/R")
install.packages("spdep", lib="D:/R")
install.packages("gstat", lib="D:/R")
install.packages("splancs", lib="D:/R")
install.packages("spatstat", lib="D:/R")
install.packages("spatstat", lib="D:/R")
install.packages("pgirmess", lib="D:/R")
install.packages("RColorBrewer", lib="D:/R")
install.packages("classInt", lib="D:/R")
install.packages("spData", lib="D:/R")
install.packages("spatstat.data", lib="D:/R") 


library("maps", lib.loc="D:/R")
library("maptools", lib.loc="D:/R")
library("sp", lib.loc="D:/R")
library("spdep", lib.loc="D:/R")
library("gstat", lib.loc="D:/R")
library("splancs", lib.loc="D:/R")
library("spatstat", lib.loc="D:/R")
library("pgirmess", lib.loc="D:/R")
library("RColorBrewer", lib.loc="D:/R")
library("classInt", lib.loc="D:/R")
library("spatstat.data", lib.loc="D:/R")

##############################################################
library("maps")         ## Projections
library("maptools")     ## Data management
library("sp")           ## Data management
library("spdep")        ## Spatial autocorrelation
library("gstat")        ## Geostatistics
library("splancs")      ## Kernel Density
library("spatstat")     ## Geostatistics
library("pgirmess")     ## Spatial autocorrelation
library("RColorBrewer") ## Visualization
library("classInt")     ## Class intervals

## Importemos los shape:
bga<-readShapePoly("G:/TRABAJO DATOS ESPACIALES/DATOS ESPACIALES Marzo 07/Bucaramanga/Barrios_Bmanga")
head(bga@data)
plot(bga)
summary(bga)

datos<-read.delim("clipboard",header=T,dec=".");str(datos)
datos<-datos[,c(6:9,12:14,20)];str(datos)
summary(datos)
str(datos)

library(ggplot2)
library(ggrepel)
library(tidyr)
library(dplyr)
library(scales)


install.packages("ggplot2", lib="D:/R")
install.packages("ggrepel", lib="D:/R")
install.packages("tidyr", lib="D:/R")
install.packages("dplyr", lib="D:/R")
install.packages("scales", lib="D:/R")
install.packages("labeling", lib="D:/R")

library("ggplot2", lib.loc="D:/R")
library("ggrepel", lib.loc="D:/R")
library("tidyr", lib.loc="D:/R")
library("dplyr", lib.loc="D:/R")
library("scales", lib.loc="D:/R")
library("labeling", lib.loc="D:/R")

#############################################################
tabla0<-as.data.frame(with(datos,prop.table(table(SEXO))))
ggplot(tabla0, aes(x = SEXO, y = Freq)) +
geom_bar(stat="identity", fill="aquamarine1", colour="darkgreen")+
geom_text(aes(label=round(100*Freq,2),
hjust=ifelse(sign(Freq)>0, 1, 0)), 
position = position_dodge(width = 1),size=4,vjust=-0.5,hjust=0.5)+
theme_light()+
scale_y_continuous(labels = percent_format()) +
labs(x="SEXO",y="")+
ggtitle("Homicidios por Género (Víctimas) ")

#############################################################
tabla1<-as.data.frame(with(datos,prop.table(table(SEXO,ESTADO.CIVIL))))

ggplot(tabla1, aes(x = ESTADO.CIVIL, y = Freq)) +
geom_bar(stat="identity", fill="cyan3", colour="darkgreen") + 
facet_wrap( ~ SEXO)+
geom_text(aes(label=round(100*Freq,2),
hjust=ifelse(sign(Freq)>0, 1, 0)), 
position = position_dodge(width = 1),size=3,vjust=-0.5,hjust=0.5)+
theme_light()+
scale_y_continuous(labels = percent_format()) +
labs(x="Estado Civil",y="")+
ggtitle("Homicidios por género y Estado Civil (Víctimas)")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

#############################################################
ggplot(datos,aes(x=SEXO, y = EDAD)) + geom_boxplot() + theme_light()+
ggtitle("Homicidios - Comparación de Edades")
with(datos, t.test(EDAD~SEXO))
with(datos, round(t.test(EDAD~SEXO)$p.value,2))
#############################################################
tabla2<-as.data.frame(with(datos,prop.table(table(SEXO,ARMA.EMPLEADA))))

ggplot(tabla2, aes(x = ARMA.EMPLEADA, y = Freq)) +
geom_bar(stat="identity", fill="cadetblue1", colour="darkgreen") + 
facet_wrap( ~ SEXO)+
geom_text(aes(label=round(100*Freq,1),
hjust=ifelse(sign(Freq)>0, 1, 0)), 
position = position_dodge(width = 1),size=3,vjust=-0.5,hjust=0.5)+
theme_light()+
scale_y_continuous(labels = percent_format()) +
labs(x="Arma Empleada",y="")+
ggtitle("Homicidios Tipo de Arma Empleada")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))


################################################################

head(data.frame(bga@data,coordinates(bga))) # ¿Qué es X1 y X2?
dtabarrios<-data.frame(bga@data[,c(6,8)],coordinates(bga))
names(dtabarrios)
colnames(dtabarrios) <- c("CODIGO","BARRIO","LONG","LAT")

v.barrios<-aggregate(CANTIDAD~BARRIO,FUN=sum,data=datos)

barriosf<-merge(v.barrios,dtabarrios,by="BARRIO")

sp_point <- cbind(barriosf$LONG, barriosf$LAT)
colnames(sp_point) <- c("LONG","LAT")
head(sp_point)

proj <- CRS("+proj=utm +zone=18 +datum=WGS84")
data.sp <- SpatialPointsDataFrame(coords=sp_point,barriosf,proj4string=proj)
bbox(data.sp)

par(mar=c(2,2,0.2,0.2))
plot(data.sp,pch=3, cex=1, axes=T)

dev.off()

win.graph()
par(mar=rep(0.5,4))
plot(bga,xlim=bbox(data.sp)[1,],ylim=bbox(data.sp)[2,],col="aliceblue")
plot(data.sp,pch=3, cex=1,add=T, col="blue")

##### Ahora con comunas:
bgac<-readShapePoly("G:/TRABAJO DATOS ESPACIALES/DATOS ESPACIALES Marzo 07/Bucaramanga/Comunas_Bmanga")
head(bgac@data)
plot(bgac)
summary(bgac)

win.graph()
par(mar=rep(0.5,4))
plot(bgac,xlim=bbox(data.sp)[1,],ylim=bbox(data.sp)[2,],col="aliceblue")
plot(data.sp,pch=3, cex=1,add=T, col="green")

barrioslef<-barriosf
bga@data$Barrio<-bga@data$NOMBRE###NO CORRER CUANDO SE HACE EL CAMBIO
head(left_join(bga@data, barrioslef))####YA SIRVEE PERO ANTES HAY QUE CAMBIAR BARRIO POR NOMBRE

#########LO QUE HIZO EL PROFE PERO NO SIRVIÓ
bga@data <- left_join(bga@data, barrioslef)
barrioslef<-edit(barrioslef)###PARA CAMBIAR BARRIO POR NOMBRE
names(bga@data)
#############

library(tmap)
library("tmap", lib.loc="D:/R")###llamarla de R

proj4string(bga)<-proj

###si quiero etiquetas le agrefo esto: text = "NOMNAMEBRE", text.size=0.75,   al primer renglón

qtm(bga, fill="CANTIDAD", 
format="Europe", style="white", 
text.root=5, fill.title="Casos de Homicidio",
fill.textNA="Registro Perdido") 

tm_shape(bga) +
    tm_fill("CANTIDAD", legend.show = TRUE) +
    tm_facets("NOMBRE") +
    tmap_options(tmap.limits=c(facets.plot=217, facets.view=4))

