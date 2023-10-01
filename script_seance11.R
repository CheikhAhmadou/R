# Semis de points - Analyse

# donnees Avril 2014 - prise en charge de passagers UBER - Etat de New York
# Objectif: comparer la distribution spatiale de la densite de prise en charge de passagers UBER 
# du 1er avril et du 2 avril 2014 - Manhattan.

# Lire les donnees, du fichier "uber-raw-data-apr14_v2.txt"
# ex:les 20 premieres lignes
readLines("uber-raw-data-apr14_v2.txt", n=20)

DDD <-  read.table("uber-raw-data-apr14_v2.txt", sep=",", header=T)
# Verification
head(DDD)
dim(DDD)
# Preparer les donnees
# separer Dates et heures en 2 objets distincts
# strptime: changer un character string en "date-time"
# as.POSIXct:  pour manipuler un objet de classe "date time"
Hours <- format(as.POSIXct(strptime(DDD$DateTime,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M") ##heure et minute
head(Hours)
length(Hours)

Dates <- format(as.POSIXct(strptime(DDD$DateTime,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%d/%m/%Y") ## jour, mois et année
head(Dates)
length(Dates)
range(Dates)

# New data frame
MyData <- data.frame(Dates=Dates, Hours=Hours,Lon=DDD$Lon, Lat=DDD$Lat)
head(MyData)
tail(MyData)
dim(MyData)


####################################
## Selectioner day 1 et creer un nouvel objet data.frame pour la date du 01/04/2014
Day1 <- subset(MyData, Dates == "01/04/2014", select = c(Dates,Lon,Lat), row.names=F) 
dim(Day1)
head(Day1)
tail(Day1)

## Selectioner day 2 et creer un nouvel objet data.frame

Day2 <- subset(MyData, Dates == "02/04/2014", select = c(Dates,Lon,Lat)) 
dim(Day2)
head(Day2)
tail(Day2)

###### /// Note: exemple subset day 3 to 4 for instance
Day0304 <- subset(MyData, Dates == c("03/04/2014","04/04/2014"), select = c(Dates,Lon,Lat)) 
dim(Day0304)
head(Day0304)
tail(Day0304)
######/// fin de la note

###
## Visualisation1
Day <- Day1
range(Day$Lon)
range(Day$Lat)

library(maps)
plot(c(-74.4,-73.5), c(40.6,41),type="n", main="Uber pick up locations - 1st April 2014")
map("county", fill = TRUE, col = "grey", add=T)
points(Day$Lon, Day$Lat, col=2)


#########################################################

# Objectif: Retenir les points tombant dans le polygone Manhattan

# extraire les coordonnees du polygone Manahattan >>> Poly1
Poly1 <- map("county","new york,new york")
Poly1
class(Poly1)
mode(Poly1)


####
#  Créer un objet  "SpatialPolygons"
library(sp)

# Etape 1: creer objet polygon
p <- Polygon(cbind(Poly1$x, Poly1$y))

# Etape 2: transformer coordonnees du polygone en coordonnees spatiales
ps <- Polygons(list(p),1)

# Etape 3: transformer coordonnees spatiales en polygone spatial (Coord ref syst)
spsMan <- SpatialPolygons(list(ps), proj4string=CRS("+proj=longlat +datum=WGS84"))
class(spsMan)

# Verif
plot(spsMan)


###########################################
##### Day 1 
# Tous les points du Day 1
Pts <- cbind(Day1$Lon,Day1$Lat) 	# Day1
dim(Pts)

# Les points tombant dans spsMan (spatial polyg) pour Day1
ptsIn <- over(SpatialPoints(Pts, proj4string=CRS("+proj=longlat +datum=WGS84")), spsMan ,returnlist=TRUE)
sum(ptsIn, na.rm=T)	# nombre total de points dans le polygone
head(ptsIn)
length(ptsIn)

# Retenir les coordonnées des points tombant dans spsMan (spatial polyg) pour Day1
#on créé une nouvelle data.frame
NND <- data.frame(Nlon=Pts[,1],Nlat=Pts[,2], PtsIn=ptsIn)
head(NND)
NewCoord <- subset(NND, PtsIn == 1, select = c(Nlon,Nlat))
dim(NewCoord)
head(NewCoord)

#
par(mar=c(4,4,1,1))
plot(c(-74.05,-73.9), c(40.7,40.86),type="n")
map("county","new york,new york", fill = TRUE, col = "grey", add=T)
points(NewCoord,col=4, pch="o", cex=0.25)
##################################################

# revision!: point moyen centre
mc <- apply(NewCoord, 2, mean)

## Sur le graphique
points(cbind(mc[1], mc[2]), pch='*', col='red', cex=2)

# distance type entre les points
StDist <- sd(dist(NewCoord))
# Cercle
library(plotrix)
draw.circle(mc[1],mc[2],radius=StDist, border=2)

# Etendue des distances entre les points
range(dist(NewCoord))

# distance moyenne entre les points

mean(dist(NewCoord))	# distance euclidienne

###########
library(raster)

# Function area : Calcul de la superifice en m2
ManArea <- area(spsMan) # m2
ManArea
# calcul de la densité de points par km2
DensDay <- nrow(NewCoord) / (ManArea/1000^2) 	# densite moyenne km2, nombre de prises en charge de passagers par km2
DensDay
##

# rasteriser le polygone
#1- creer une raster layer
r <- raster(spsMan)
res(r) <- 1/500 	# change resolution, ici 0.002 = ~200m
r


#2- Transferer les valeurs du polygone sur la raster layer
rr <- rasterize(spsMan, r)
# Verif
plot(rr, legend=F)
#
quads <- as(rr, "SpatialPolygons")	# Transformer le raster en spatial polygon (mailles)
plot(quads, add=TRUE) 	# representer graphiquement les mailles
points(NewCoord, col='red', cex=.5)	# representer les points

# 3- Transferer les valeurs des points sur la raster layer et calculer la frequence par maille

NNC <- rasterize(coordinates(NewCoord), rr, fun='count', background=0)
NNC
# Note: ici la resolution est de 0.002deg*0.002deg, ce qui correspond a environ 200m*200m

#
plot(NNC)
plot(spsMan, add=TRUE)

# Ajouter un mask
ncUBER <- mask(NNC, rr)
plot(ncUBER)
plot(spsMan, add=TRUE)


## nombre de mailles par frequence
fff <- freq(ncUBER, useNA='no')
fff
dim(fff)
plot(fff)
####

# Nombre de mailles dans polygone Manhattan
Squadrats <- sum(fff[,2])
Squadrats 

# Nombre de prise en charge total
cases <- sum(fff[,1] * fff[,2])
cases

# Nombre de prise en charge moyen par maille
mu <- cases / Squadrats
mu
#

ncUBER1 <-  ncUBER 



####################################
## with Day 2, run fonctions above

ncUBER2 <-  ncUBER


########### Difference entre day 2 et day 1 ##############
DDif <- ncUBER2 - ncUBER1 
DDif
coul <- c(topo.colors(15)[1:5], "white", topo.colors(20)[9:20])
plot(DDif, col = coul, main= paste("Prise en charge UBER -","\n Difference de densite entre le 2 et le 1er Avril 2014"))
plot(spsMan, add=TRUE)

# Est-ce que les moyennes des densites sont significativement differentes?
# Puis tester la difference des variances
t.test(ncUBER2,ncUBER1)



