# Script 3- Session 3 - Importer/exporter des donn�es
#
########################################
# 1- Utiliser la fonction scan()
#######################################
F1 <- scan ("F1.txt", what=character(), sep=",")
F1

F2 <- scan("F2.txt", sep=";", what=numeric())
F2

############################################
# 2- lire les lignes d'un fichier de donn�es avec readLines()
############################################
readLines("F3.txt", n=1)
readLines("F3.txt", n=2)
F3 <- scan("F3.txt", sep=";", skip=1,what=numeric() )
F3
##########################################
# 3- Exporter des donn�es en .txt avec write()
##########################################
F4 <- c(1:10)
F4
write(F4,"MyF4.txt", sep=";")

F5 <- LETTERS[1:10]
F6 <- c(F5,F4)
F6

write(F6,"MyF6.txt", sep="\t", ncolumns=10)

########################################
# 4- Importer des donn�es de type tableau
########################################
readLines("PopDepMetro_2015.txt", n=10)
#
POP <- read.table("PopDepMetro_2015.txt",sep="\t", header=T)
POP
mean(POP$Pop)
##### Exercice ####



########################################
# 5- Exporter des donn�es de type tableau
########################################
# Cr�er une data.frame
F7 <-  data.frame (LL = LETTERS[1:20], NN = 11:30)
F7   
# Exporter le tableau en .txt
write.table (F7,"MyF7.txt", sep="\t", row.names = F)


########################################
# 6- Importer/exporter un csv
########################################
F8 <- read.csv("F8.csv",  sep=";")
F8 
# Cr�er une data.frame
F9 <- data.frame(CC= c("red","blue","green"), 
			NN= c(30,36,43))
F9
# Exporter le tableau en .csv
write.table (F9,"MyF9.csv", sep=";", row.names = F)

##########################################
# Autre format
# ex: shape files

# on telecharge le dossier zip "Income_schooling.zip" dans le repertoire courant
download.file("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling.zip", 
              destfile = "Income_schooling.zip" , mode='wb')
# unzip
unzip("Income_schooling.zip", exdir = ".")
# Intaller puis activer le package maptools

library(maptools)
s1 <- readShapeSpatial("Income_schooling")
s1

summary(s1)

# library(sp)
spplot(s1, z="Income")

##plus d'infos# https://github.com/mgimond/Spatial/



#################################################
# ex requ�te SQL


#Server : ServerName
#User  : UserName
#Pass : Password
#Database : DatabaseName

# Installer puis activer le package RODBC
library("RODBC")

# connexion
#odbcChannel <- odbcConnect("DatabaseName")
#odbcClose(odbcChannel)


### Afficher / Selectionner les noms de colonnes d'une table et cr�er un objet R

# Afficher toutes les tables/views d'une base de donn�es + infos
#sqlQuery(odbcChannel, paste("select * from INFORMATION_SCHEMA.TABLES order by TABLE_NAME"))
#sqlQuery(odbcChannel, paste("select TABLE_NAME from INFORMATION_SCHEMA.TABLES order by TABLE_NAME"))

# Crerer un objet X1 contenant la liste des colonnes d'une table
# X1 <- sqlQuery(odbcChannel, paste("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 'TableName' "))

### Selectionner toute la table

# X2 <- sqlQuery(odbcChannel, paste("SELECT * FROM TableName"))

### Selectionner les 10 premi�res lignes des 3 premi�res colonnes d'une table

# X3 <- sqlQuery(odbcChannel, "SELECT TOP 10 ColName1, ColName2,ColName3 FROM TableName")

### Selectionner les valeurs des 3 premi�res colonnes d'une table selon 2 crit�res appliqu�s aux colonnes 2 et 3, puis ordonner les lignes selon l'ordre croissant de la colonne 1
### les valeurs peuvent �tre num�riques et/ou carat�res. Ici les colonnes 2 et 3 sont numeriques

# X4 <- sqlQuery(odbcChannel, paste("SELECT ColName1, ColName2, ColName3 FROM TableName", "WHERE ColName2 > 2015 AND ColName3 = 3150 order by ColName1"))

### Selectionner les valeurs des 3 premi�res colonnes d'une table selon 2 crit�res appliqu�s aux colonnes 2 et 1, puis ordonner les lignes selon l'ordre croissant de la colonne 1
### les valeurs peuvent �tre num�riques et/ou carat�res. Ici la colonne 2 est numerique et la colonne 1 est caract�re

# X5 <- sqlQuery(odbcChannel, paste("SELECT ColName1, ColName2, ColName3 FROM TableName", "WHERE ColName2 > 2015 AND ColName1 LIKE 'Character' order by ColName1"))



#################################################
## Graphs and Maps 
################################################
windows(16, 10)
par(mar=c(4.5,4.5,3,1))
plot(c(-130,-65),c(25,50),xlab="longitude", ylab="latitude", type="n", main="USA - Etats contigus")
abline(v=seq(-130,-65,by=5), col="grey",  lty=2)
abline(h=seq(25,50,by=5) ,col="grey",  lty=2)

# activer le package maps
library(maps)
# Ajouter la couche contour du pays +iles (polygones)
map("usa", add=T)
# Ajouter la couche contours des �tats (polygones)
map("state", add=T)
# Ajouter la couche centres des �tats (points, x y)
points(state.center, col=4,pch=19)
# Noms des �tats
text(state.center$x, state.center$y+0.5 , state.name, col=2, cex=0.5)

###### Exercice
windows(16, 10)
par(mar=c(4.5,4.5,3,1))
plot(c(-130,-65),c(25,50),xlab="longitude", ylab="latitude", type="n", main="Population urbain - 1973")
abline(v=seq(-130,-65,by=5), col="grey",  lty=2)
abline(h=seq(25,50,by=5) ,col="grey",  lty=2)

# activer le package maps
library(maps)
# Ajouter la couche contour du pays +iles (polygones)
map("usa", add=T)
# Ajouter la couche contours des �tats (polygones)
map("state", add=T)
# Ajouter la couche centres des �tats (points, x y)
text(state.center$x, state.center$y, USArrests$UrbanPop, col=4, cex=1.25)
# Noms des �tats
text(state.center$x, state.center$y+0.5 , state.abb, col=2, cex=1)




############## Autre carte #################
# visualiser le tableau de donn�es
USArrests
#
range(USArrests$UrbanPop)
rownames(USArrests)

Names <- rownames(USArrests)
PopU <- USArrests$UrbanPop

windows(14, 8)
par(mai=c(1,1,0.75,0.25))
par(bg = "grey88")

plot(c(-130,-65),c(25,50),xlab="longitude", ylab="latitude",type="n", main="USA - Urban Population (1973)")
map("state", add=T)

CL <- seq(from=30,to=90, by=5)
length(CL)
coul <- rainbow(13,s=seq(from=0.05,to=1, length.out=13), start=0.7, end=0.9)

for(i in 1:13){
cc <- CL[i]
map("state",region= Names[PopU > cc], fill=T, col=coul[i], add=T)
}
legend(-70,39,rev(paste("[",CL,":", CL+5,")", sep="")), fill=rev(coul), cex=0.75, title="%")

##########################
### Exporter carte/graphique en tiff ou pdf...
#tiff("PlotMapB-World.tiff", width = 16, height = 10, units = 'in', res = 600)
#pdf("PlotMapB-World.pdf", width = 16, height = 10)
## A la fin des commandes graphiques
# dev.off()










