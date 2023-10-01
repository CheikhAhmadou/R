### R session 9 ######
# Structure spatiale (1)
###############################
# 1. Introduction

# 2. Lire les données "Data_oz_15sts.txt"
Datoz <- read.table()
Datoz


# 3. Vérifier les attributs de l'objet




# 4. Visualisation
## Carte 1
install.packages("leaflet")
library(leaflet)
map <- leaflet(data = Datoz) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(ID), label = ~as.character(oz))
map


########
# Carte 2 ## pour visualiser les calculs ultérieurs
library(maps)
plot(Datoz$lon, Datoz$lat, 
		col="grey", pch=19, main = "Stations Location" )
text(Datoz$lon, Datoz$lat + 0.02, rownames(Datoz), col=2)
map("usa", add=T, col=4)




# 5. Réaliser résumé numérique et une boite de dispersion (ozone)




#################################################
# 6. Point moyen centré et point médian centré

PtMC <- c(mean(Datoz$lon), mean(Datoz$lat)) ; PtMC 
PtmC <- c(median(Datoz$lon),median(Datoz$lat)); PtmC 

# représenter les points sur la carte
points(PtMC[1], PtMC[2], pch=19, col=3)
points(PtmC[1], PtmC[2], pch=19, col=5)


# 7. Calcul de distance
# 7.1. Euclidienne
# Distance eucidienne entre station 1 et point moyen centré
# 1ere etape: on créé une matrice avec les coordonnées des 2 points considérés
MM <- as.matrix(rbind(st1= c(Datoz$lon[1],Datoz$lat[1]), PtMC))
MM


# 2eme étape on applique la fonction dist
DDist <- dist(MM)
DDist



# 7.2. Sphérique
# Distance sphérique entre le point moyen centré et les 15 stations 
library(sp)
MatP <- as.matrix(cbind(lon=Datoz$lon, lat=Datoz$lat))
MatP

Distkm <- spDistsN1(MatP ,  PtMC , longlat=T)
cbind(1:15, Distkm) 


# 7.3. Distance moyenne et distance type au point moyen centré

# Distance moyenne au point moyen centré


# Distance type au point moyen centré "Distance de Bachi"


# Verif
# quantiles des distances au point moyen centré 


# distribution stat des dist. au point moyen centré


# 7.4. Matrice des distances 



# Verif
# summary, range, hist, length, dim




#Image de la matrice des distances 



######################################
# 8. Autocorrélation spatiale (test de Mantel, Indice de Moran )
# 

#Visualisation des niveaux d'ozone 
# Map

plot(Datoz$lon, Datoz$lat, type="n", 
	main="Ozone measurements - Chicago (Summer 1987)" )
points(Datoz$lon,Datoz$lat, pch=19, col="grey")
text(Datoz$lon, Datoz$lat+0.02, 
	as.character (round(Datoz$oz)), col="purple")
map("usa", add=T, col=4)


# 8.1. Test de Mantel
# Test d’autocorrélation spatiale linéaire
# On a besoin des
# 1-distances euclidiennes des niveaux d'ozone 
# 2- distances euclidiennes spatiales (distance entre les stations)



# Verif




# 8.2. Indice de Moran
# Test d’autocorrélation spatiale- effet de voisinage
# on a besoin de 1- les données ozone (Datoz$oz )
# et 2-Coefficients inverses des distances






# 8.3. Corrélogramme






### Map ######################################
library(maps)

range(Datoz$oz)
rr <- seq(31,43,by=2)
coul <- rev(rainbow(7,0.6,1,0.05,0.6))


plot(Datoz$lon, Datoz$lat, type="n", 
	main="Ozone measurements - Chicago (Summer 1987)" )

for(i in 1:7){
ff <- rr[i]
points(Datoz$lon[Datoz$oz >= ff],Datoz$lat[Datoz$oz >= ff], pch=21, bg=coul[i], col=1,cex=2)
}
text(Datoz$lon, Datoz$lat+0.02, 
	as.character (round(Datoz$oz)), col="purple")

map("usa", add=T, col=4)


