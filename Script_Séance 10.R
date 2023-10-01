#lire les données 
Datoz<- read.table("Data_oz_15sts.txt", header = 1)
Datoz

######"matrix des 15 permiersspoints geographiques 

# 2- distances euclidiennes spatiales (distance entre les stations)

DistOz<-dist(Datoz$oz)
DistOz

#2-distance euclidiennees entre les coord des stations

MatP<-as.matrix(cbind(lon=Datoz$lon, lat=Datoz$lat))
MatP


#
library(ade4)

#distance euclidienne
#
DistP<-dist(MatP)
DistP

## Verif du test de mantel

# test de mantel : test de corrélation entre deux matrix de distance euclidienne
mantel.rtest(DistP,DistOz) # relation est significative avec un effet de gradiient qui n'est pas super fort

#le teste de mantle nous indique que nous avons une autocoorélation 


#etude l'indice de voisinnage avce l'indice de Moran

####
coefdist<-1/as.matrix(dist(MatP)) # Matp: matrix de distance euclidienne spatiale
coefdist # plus les distances sont grandes , plus le coefficient est petit.

#
diag(coefdist)<-0
coefdist

#### Le coefficient de Moran
library(ape)

Moran.I(Datoz$oz, coefdist) # ==0.13 et >0 , les stations proches ont tendances à voir des concentrations en ozone comporable

#indice de moran compris entre -1 et 1, comme un coefficient de corrélation
#I>0: autocorrélation positive
# I=0 : pas d'autocorrélation(distribution spatiale aléatoire)
#I<0: autocorrélation négative

# Ici , les stations proches les une des autres ont tendance à avoir des concentrations en ozone comporables.


##CORRELOGRAMME
#on cacul l'indice de moran par tranche de distance 

library(pgirmess)

# 1 Matrice des coordonnées géographiques , 2-Données Ozone 
Corlog<-correlog(MatP, Datoz$oz, "Moran")
Corlog

plot(Corlog) # Donnes Indice de moran en fonction des distances entre les classes;

## Le point rouge du diagramme montre que l'effet de voisinage est trés significative , 
#les stations situés à 20km ont un effet de voisinnage trés significative 


# Indice de Geary : pour voir s'il y'a une petite difference entre les resultats
##1. Signifie aucune autocorrélation
# indice de Geary =1 ; autocorrélation positive;
#indice >1 correlation négative 

Corlog2<-correlog(MatP, Datoz$oz, "Geary")
Corlog2

plot(Corlog2) ## indice de Geary en fonction des classes de distances 

## SEMIVARIANCE: VARIOGRAMME : calcul la variance spatiale par classe de valeur
# variance : dispersion autour de la moyenne
install.packages("geoR")
library(geoR)
vg1<-variog(coords=MatP, data=Datoz$oz)
vg1 # on note : $v , ce sont les valeurs de la variance 
plot(vg1, type="o", main="variogramme")

##Representer graphiquement les limites des intervalles de distances 
#spatiale et le nombre de paires de stations tombant dans chaque intervall
abline(v=vg1$bins.lim)
text(vg1$u, 0, vg1$n) # avec $u , le centre de classe et vg1 , le nombre de paires
plot(vg1, type="o", main="variogramme") # effect neguet , la premiere classes du semivariance 

## CACCUL DE LA SEMIVARIANCE
#sum(DistOz^2)/nombre de paire *2
var(Datoz$oz)

length(DistOz)
sum(DistOz^2)/(105*2)

## INTERPOLATION SPATIALE: KRIKING
library(fields)

# Methode Krigeage
InterMod<-Krig(MatP, Datoz$oz)

#
Mod<-predict(InterMod)
Mod

#Modele , Observation , Biais du modéle
cbind(Mod, Datoz, Mod-Datoz$oz) ## Mod-Obs=biais du modéle, Obs-Mod=résidus du modéle

#Evaluation de la performanec du modele
cor.test(Mod, Datoz$oz)

#les residues
residu<-Datoz$oz-Mod

boxplot(residu)

summary(residu)


##Predire la valeur d'un point + erreur standard
PredP<-rbind(c(-87.7, 41.8))

predict(InterMod, PredP)

predictSE(InterMod, PredP) # standard erreur

## Predire les valeurs du model sur une grille +carte
ModGrid<-predictSurface(InterMod)
surface(ModGrid)

attributes(ModGrid)

## 
ModGridE<-predictSurfaceSE(InterMod)
surface(ModGridE)



#########################EXERCICE##########
#1 Comparer les indices d'autocorrélaltion
Datoz2<- read.table("Data_oz_20sts.txt", header = 1)
Datoz2


## Test de mantel
DistOz2<-dist(Datoz2$oz)
DistOz2

#2-distance euclidiennees entre les coord des stations

MatP2<-as.matrix(cbind(lon=Datoz2$lon, lat=Datoz2$lat))
MatP2

# 
DistP2<-dist(MatP2)
DistP2
mantel.rtest(DistP2,DistOz2) # relation est significative avec un effet de gradiient qui n'est pas super fort;
 
#Coeffecient de mantel est comme un coeficient de correclation , ici le coefficient de mantel est : obervation: 0.11

#le teste de mantle nous indique que nous avons une autocoorélation 


#etude l'indice de voisinnage avce l'indice de Moran

coefdist2<-1/as.matrix(dist(MatP2)) # Matp: matrix de distance euclidienne spatiale
coefdist2 # plus les distances sont grandes , plus le coefficient est petit.

# Le diagonales
diag(coefdist2)<-0
coefdist2

#### Le coefficient de  Moran
Moran.I(Datoz2$oz, coefdist2) # I Moran = $observed= 0.08, p-values= 0.016 significative à 95% , nous avons une autocorrélation positive  


##  CORRELOGRAMME
#Geary
CorlogD2<-correlog(MatP2, Datoz2$oz, "Geary") # un effet de voisinnage tres fin sur la premiere classe
CorlogD2

plot(CorlogD2) ## indice de Geary en fonction des classes de distances 

##2 Comporaison des modeles selon la methiode de Krige.Krig(), ## avec le package feild, predictSurface
# predictSurfaceSE? Surface 


## INTERPOLATION SPATIALE: KRIKING
library(fields)

# Methode Krigeage
InterMod2<-Krig(MatP2, Datoz2$oz)

#
Mod2<-predict(InterMod2)
Mod2


#Modele , Observation , Biais du modéle
cbind(Mod2, Datoz2, Mod2-Datoz2$oz) ## Mod-Obs=biais du modéle, Obs-Mod=résidus du modéle


#Evaluation de la performanec du modele
cor.test(Mod2, Datoz2$oz) # correlation =0.93 , montre qur le modele est trés performant 

#les residues
residu2<-Datoz2$oz-Mod2

boxplot(residu2)

summary(residu2)
#
##Predire la valeur d'un point + erreur standard
PredP<-rbind(c(-87.7, 41.8))

predict(InterMod2, PredP)

predictSE(InterMod2, PredP) # standard erreur

## Predire les valeurs du model sur une grille +carte
ModGrid2<-predictSurface(InterMod2)
surface(ModGrid2)

attributes(ModGrid2)

##
ModGridE2<-predictSurfaceSE(InterMod2)
surface(ModGridE2)










