#Modelització de dades Geoespacials

#Imports


library(geoR)
library(sm)
library(sp)
library(gstat)
library(npsp)
library(geohashTools) 
library(rgdal)
library(ggmap)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(gifski)

#=======================================================================#

##TIPUS 1: Variació d'una variable continua en l'espai. 
#          Geoestadística (Variogrames & Kriging)

dd <- read_csv("C:/Users/alexc/OneDrive/Escriptori/PMAAD/Dataset_clean_clean_id.csv")
dd <- dd[,c(15,16,20)]   #Ens quedem únicament amb les necessàries
#En deixem tres numèriques per poder predir 
#diferents coses després.

coordinates(dd) <- c("ln", "lt")

proj4string(dd) <- CRS("+proj=longlat +ellps=WGS84
                      +datum=WGS84 +no_defs")
dd <- spTransform(dd, CRS("+proj=utm +zone=31 +ellps=WGS84
                      +datum=WGS84 +units=m +no_defs"))
dd$UTMx <- coordinates(dd)[, 1]   #Afegim les dues noves columnes en els nous eixos.
dd$UTMy <- coordinates(dd)[, 2]
dd$logprice <- log10(dd$price)
hist(dd$logprice,bins=16)
sm.variogram(coordinates(dd), dd$logprice, model = "independent") 

plot(variog(coords = coordinates(dd),data = dd$logprice,max.dist=9000))
vario.b <- variog(coords = coordinates(dd),data = dd$logprice,max.dist=9000)  #Variograma empíric

#Cómo escoger el ajuste para el variograma empirico?
#Ajuste visual
#Range o rango: separación o distancia entre pares de puntos en la cual ya no hay dependencia espacial,
#aprox 1600m
#Nugget o pepita: semivarianza a la separación de 0m. aprox 0.035
#Total-sill o meseta: semivarianza a la distancia del rango. aprox 0.1
#Partial-sill o meseta parcial: total sill - nugget. aprox 0.07

#Variograma teòric
plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(0.07,1600), nugget = 0.035, max.dist = 9000, lwd = 3)
lines.variomodel(cov.model = "mat", cov.pars = c(0.07,1600), nug = 0.035,kappa=1, max.dist = 9000, lwd = 3)
lines.variomodel(cov.model = "sph", cov.pars = c(0.07,1600), nug = 0.035, max.dist = 9000, lwd = 3)

vario.ols <- variofit(vario.b, c(0.07,1600), weights = "equal")  #ordinarios
vario.wls <- variofit(vario.b, ini = c(0.07,1600), weights = "cressie")  #ponderados
vario.wls


vario.ml <- likfit(coords = coordinates(dd),data = dd$logprice, cov.model = "sph",ini.cov.pars = c(0.07,1600),kappa=1,nugget = 0.035) #Modelo exponencial con par ini umbral y escala (1/3 rango)
vario.ml <- likfit(coords = coordinates(dd),data = dd$logprice, ini = c(0.07,1600)) #Modelo exponencial con par ini umbral y escala (1/3 rango)

plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.ml, max.dist = 9000)
lines(vario.ols, lty = 2, max.dist = 9000)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 9000)
legend(0.3, 0.3, legend = c("ML", "OLS", "WLS"), lty = c(1, 2, 2), lwd = c(0.5,0.5,0.5)) 

#Predicción espacial (kriging)

#   Kriging ordinario (KO): se supone que la media es constante y desconocida.

xx <- seq(min(dd$UTMx), max(dd$UTMx), l = 51)
yy <- seq(min(dd$UTMy), max(dd$UTMy), l = 51)
pred.grid <- expand.grid(x = xx, y = yy)
plot(dd$UTMx,dd$UTMy, pch = 20)
points(pred.grid, pch = 3, cex = 0.2)

#Kriging ordinario
ko.wls <- krige.conv(coords = coordinates(dd),data = dd$logprice, loc = pred.grid, krige = krige.control(obj.m = vario.ml))  #Amb maximum likelihood
names(ko.wls)
image(ko.wls,add=T) #superficie de predicción
title("Predicciones")
points(coordinates(dd), pch=20) #añadir posiciones datos
contour(ko.wls,add=T) #añadir gráfico de contorno

image(ko.wls, val = ko.wls$krige.var,add=T) #superficie de varianzas
title("Superficie de varianzas")
points(coordinates(dd), pch=20)
contour(ko.wls,val=sqrt(ko.wls$krige.var),add=T)

contour(ko.wls,filled = TRUE)
fcol <- topo.colors(10)[cut(matrix(ko.wls$pred,nrow=51,ncol=51)[-1,-1],10,include.lowest=TRUE)]
persp(ko.wls, theta=-60, phi=40, col=fcol)

persp3D(xx, yy, matrix(ko.wls$predict, nrow = length(xx)), theta=-60, phi=40)
spersp(xx, yy, ko.wls$predict, theta=-60, phi=40)


#===========================================#



#=======================================================================#

##TIPUS 2: Events puntuals / Específics

#Fem el merge entre la base de dades de les reviews amb data+id amb
#la de id+lat-long

d1 <- read_csv("C:/Users/alexc/OneDrive/Escriptori/PMAAD/Dataset_clean_id.csv")
d2 <- read_csv("C:/Users/alexc/OneDrive/Escriptori/PMAAD/reviews (1).csv")  #Id + date
d1 <- d1[,c(1,15,16)]  #Id+lat+long
colnames(d2)[1] <- "id"

#Per tal de tenir un database transaccional, és a dir, on les files són
#reviews i les columnes indiquen un punt en l'espai (review SI) hem
#d'afegir per cada aparició de l'id d'un apartament a d2 (reviews)
#els valors de lat i long associats que tenim a d1.

#Primer els ids com a factors:

d1$id <- as.factor(d1$id)
d2$id <- as.factor(d2$id)

#Juntem datasets per tenir-ho com una transacció

d <- merge(d1, d2, by = "id", all = FALSE)  #Id + lat + long + date_review

#Estadística descriptiva.

ggplot(d, aes(x = ln, y = lt)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = d) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

ggplot(d, aes(x = ln, y = lt)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 h = .02, n = 300,
                 geom = "polygon", data = d) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

ggplot(crime, aes(x = lon, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  coord_cartesian(xlim = c(-95.1, -95.7),   #Si volguéssim posar límits cartesians
                  ylim = c(29.5, 30.1))


scatterplot_rev <- qmplot(x=ln,y=lt,data=filter(d),legend="none",color=I("darkred"))
plot(scatterplot_rev)


#Fem plot de densitat de totes les reviews en un Mapa de Barcelona extret de Google
#juntant totes les reviews fetes des de 2011-2023

#Clau API de Google per obtenir el mapa estàtic
ggmap::register_google(key = "AIzaSyD8iMYZb-WxtU3PGxe8L5a40XkrRN6GaCQ")


bcn_map <- ggmap(get_googlemap("Barcelona,Spain",
                               zoom = 13, scale = 4,
                               maptype ='terrain',
                               color = 'color'))
bcn_map

pl <- bcn_map + stat_density_2d(aes(x=ln, y=lt, fill = ..level..),alpha = .5,data = d, 
                                geom = "polygon") + 
  scale_fill_gradient2(low = "blue",mid = "green", 
                       high = "red") + 
  labs(title=sprintf("Density of reviews"))

png(file=sprintf("C:/Users/alexc/OneDrive/Escriptori/PMAAD/finalmap_allyears.png"),
    width=800, height=600)
plot(pl)
dev.off()

#Afegim com a tercera dimensió el temps i veiem
#l'evolució del mapa de calor del succés puntual
#al llarg dels mesos d'un mateix any

#Primer de tot convertim la data date en el datatype
#corresponent i afegim tres columnes més indicant dia
#mes i any per tal de filtrar:

d$day <- substr(d$date, 9, 10)
d$month <- substr(d$date, 6, 7)
d$year <- substr(d$date,1,4)

d$day <- as.numeric(d$day)
d$month <- as.numeric(d$month)
d$year <- as.numeric(d$year)

#Fem un bucle i grafiquem tenint com a filtre els diferents anys. Anem
#a veure si al llarg dels anys els llocs on es fan reviews a Barcelona
#és a dir els llocs de més influència turística són els mateixos:



for (c in range(d$year)[1]:range(d$year)[2]){
  
  pl <- bcn_map + stat_density_2d(aes(x=ln, y=lt,fill = ..level..),alpha = .5,data = filter(d,year==c), 
                                  geom = "polygon") + 
    scale_fill_gradient2(low = "blue",mid = "green", 
                         high = "red") + 
    labs(title=sprintf("Density of reviews in %s", c))
   
  png(file=sprintf("C:/Users/alexc/OneDrive/Escriptori/PMAAD/finalmap%s.png", c),
  width=800, height=600)
  plot(pl)
  dev.off()}
  
#Intentem fer l'animació posant totes les imatges en GIF

png_files <- list.files("C:/Users/alexc/OneDrive/Escriptori/PMAAD/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "reviewed_points_progress_05.gif", width = 800, height = 600, delay = 2)

#Ara provaré de fer el mateix però recopilant/filtrant les transaccions/reviews
#per mesos, de manera que s'acumularan en un mateix plot les reviews d'un mateix
#mes de diferents anys. Vull veure si hi ha alguna diferència en els llocs on hi ha
#reviews segons la estacionalitat/època de l'any

months <- c("January","February","March","April","May","June","July","August",
           "September","October","November","December")

for (c in range(d$month)[1]:range(d$month)[2]){
  
  pl <- bcn_map + stat_density_2d(aes(x=ln, y=lt,fill = ..level..),alpha = .5,data = filter(d,month==c), 
                                  geom = "polygon") + 
    scale_fill_gradient2(low = "blue",mid = "green", 
                         high = "red") + 
    labs(title=sprintf("Density of reviews in %s (2011-2023)", months[c]))
  
  png(file=sprintf("C:/Users/alexc/OneDrive/Escriptori/PMAAD/finalmapmonths%s.png", months[c]),
      width=800, height=600)
  plot(pl)
  dev.off()}

#Intentem fer l'animació posant totes les imatges en GIF

png_files <- list.files("C:/Users/alexc/OneDrive/Escriptori/PMAAD/", pattern = "finalmapmonths.*png$", full.names = TRUE)
#Ordenem manualment
ordered_png_files <- c(png_files[5],png_files[4],png_files[8],png_files[1],png_files[9],
                       png_files[7],png_files[6],png_files[2],png_files[12],png_files[11],
                       png_files[10],png_files[3])
gifski(ordered_png_files, gif_file = "reviewed_points_progress_months_05.gif", width = 800, height = 600, delay = 2)

#setwd("C:/Users/alexc/OneDrive/Escriptori/PMAAD/")
#write.csv(d2, file = "reviews.csv", row.names = FALSE) 


