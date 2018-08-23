###################
### TESIS MECAP ###
###################

#Paquetes para econometria espacial.
install.packages("maptools", dependencies = TRUE)
install.packages("spdep", dependencies = TRUE)
install.packages("leaflet", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)

library(spdep)
library(maptools)


#Directorio al que busca la informacion.
setwd("C:/Users/PC/Desktop/Tesis MECAP/3. Calculos y bases/Analisis espacial")


#Se llama a los archivos.
chi.poly <- readShapePoly('pxpciadatosok.shp')
class(chi.poly)
str(slot(chi.poly,"data"))
plot(chi.poly)


#Para hacer el mapa más lindo.
library(leaflet)
leaflet(chi.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5) %>%
  addTiles() #adds a map tile, the default is OpenStreetMap


#Para agregar color al mapa.
require(RColorBrewer)


#Mapas para las distintas variables.
###
qpal<- colorQuantile("Blues", chi.poly$URB)  ##Urbanizacion.

leaflet(chi.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(URB)) %>%
  addTiles()
###
qpal2<- colorQuantile("Blues", chi.poly$IDH)  ##Indice de desarrollo humano.

leaflet(chi.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal2(IDH)) %>%
  addTiles()
###
qpal3<- colorQuantile("Blues", chi.poly$IDG)  ##Indice de desarrollo relativo al genero. 

leaflet(chi.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal3(IDG)) %>%
  addTiles()
###
qpal4<- colorQuantile("Blues", chi.poly$GINI)  ##Indice de Gini. 

leaflet(chi.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal4(GINI)) %>%
  addTiles()

?RColorBrewer  ##Para ver otras paletas


###
#Econometria espacial.

#Regresiones.

chi.mco2<-lm(IDH~URB+GINI, data=chi.poly@data)
summary(chi.mco2)

chi.mco3<-lm(IDG~URB+GINI, data=chi.poly@data)
summary(chi.mco3)

chi.mco4<-lm(IDH~URB+GINI+IDG, data=chi.poly@data)
summary(chi.mco4)



#Modelizacion de la dependencia espacial. 

list.queen<-poly2nb(chi.poly, queen=TRUE)  #Criterio queen.
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
print(W, zero.policy=TRUE) 
plot(W,coordinates(chi.poly))

coords<-coordinates(chi.poly)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)


#Autocorrelacion espacial.

moran.lm2<-lm.morantest(chi.mco2, W, alternative="two.sided", zero.policy = TRUE)  ##Test de Moran.
print(moran.lm2)
LM2<-lm.LMtests(chi.mco2, W, test="all", zero.policy = TRUE)  ##Test de multiplicadores de lagrange.
print(LM2)

moran.lm3<-lm.morantest(chi.mco3, W, alternative="two.sided", zero.policy = TRUE)  ##Test de Moran.
print(moran.lm3)
LM3<-lm.LMtests(chi.mco3, W, test="all", zero.policy = TRUE)  ##Test de multiplicadores de lagrange.
print(LM3)


#Regresiones espaciales.
sar.chi<-lagsarlm(IDH~URB+GINI, data=chi.poly@data, W, zero.policy = TRUE)  ##Modelo de autorregresión espacial.
summary(sar.chi)

sar.chi2<-lagsarlm(IDG~URB+GINI, data=chi.poly@data, W, zero.policy = TRUE)  ##Modelo de autorregresión espacial.
summary(sar.chi2)


#Comparacion de residuos entre modelos.
chi.poly@data$chi.mco2.res<-resid(chi.mco2) #Residuos MCO.
chi.poly@data$chi.sar.res<-resid(sar.chi) #Residuos SAR.

chi.poly@data$chi.mco3.res<-resid(chi.mco3) #Residuos MCO.
chi.poly@data$chi.sar2.res<-resid(sar.chi2) #Residuos SAR.

spplot(chi.poly,"chi.mco2.res", at=seq(min(chi.poly@data$chi.mco2.res,na.rm=TRUE),max(chi.poly@data$chi.mco2.res,na.rm=TRUE),length=12),col.regions=rev(brewer.pal(11,"Blues")))
spplot(chi.poly,"chi.sar.res",at=seq(min(chi.poly@data$chi.sar.res,na.rm=TRUE),max(chi.poly@data$chi.sar.res,na.rm=TRUE), length=12), col.regions=rev(brewer.pal(11,"Greens")))

spplot(chi.poly,"chi.mco3.res", at=seq(min(chi.poly@data$chi.mco3.res,na.rm=TRUE),max(chi.poly@data$chi.mco3.res,na.rm=TRUE),length=12),col.regions=rev(brewer.pal(11,"Blues")))
spplot(chi.poly,"chi.sar2.res",at=seq(min(chi.poly@data$chi.sar2.res,na.rm=TRUE),max(chi.poly@data$chi.sar2.res,na.rm=TRUE), length=12), col.regions=rev(brewer.pal(11,"Greens")))


#Efectos marginales.
impacts(sar.chi, listw=W)
impacts(sar.chi2, listw=W)


#Modelo de errores espaciales.
errorsalm.chi<-errorsarlm(IDH~URB+GINI, data=chi.poly@data, W, zero.policy = TRUE)
summary(errorsalm.chi)

errorsalm.chi2<-errorsarlm(IDG~URB+GINI, data=chi.poly@data, W, zero.policy = TRUE)
summary(errorsalm.chi2)


###
#Datos de panel.

#Base de datos.
library(readxl)
Datosprovanio <- read_excel("C:/Users/PC/Desktop/Tesis MECAP/3. Calculos y bases/Analisis espacial/Datosprovanio.xlsx")
View(Datosprovanio)
Datosprovanio


#Regresiones por MCO.
reg1<-lm(IDH~Urb+Gini, data=Datosprovanio)
summary(reg1)

reg2<-lm(IDG~Urb+Gini, data=Datosprovanio)
summary(reg2)


#Regresiones por efectos fijos.
reg.fijos.ind = lm(IDH~Urb+Gini+factor(Provincia), data=Datosprovanio)
summary(reg.fijos.ind)

reg.fijos.ind2 = lm(IDG~Urb+Gini+factor(Provincia), data=Datosprovanio)
summary(reg.fijos.ind2)


reg.fijos.peri = lm(IDH~Urb+Gini+factor(Anio), data=Datosprovanio)
summary(reg.fijos.peri)

reg.fijos.peri2 = lm(IDG~Urb+Gini+factor(Anio), data=Datosprovanio)
summary(reg.fijos.peri2)


#Regresiones por efectos aleatorios.

install.packages("plm")
library(plm)

reg.aleatorios = plm(IDH~Urb+Gini, index=c("Provincia", "Anio"), model="random", data=Datosprovanio)
summary(reg.aleatorios)

reg.aleatorios2 = plm(IDG~Urb+Gini, index=c("Provincia", "Anio"), model="random", data=Datosprovanio)
summary(reg.aleatorios2)


#Prueba de Hausman.

reg.fijos.within = plm(IDH~Urb+Gini, index=c("Provincia", "Anio"), model="within", data=Datosprovanio)
summary(reg.fijos.within)

reg.fijos.within2 = plm(IDG~Urb+Gini, index=c("Provincia", "Anio"), model="within", data=Datosprovanio)
summary(reg.fijos.within2)

phtest(reg.fijos.within, reg.aleatorios) 

phtest(reg.fijos.within2, reg.aleatorios2) 

