### Script 7  ### Analyses multivariées

# Lire le fichier de données: "Data_INSEE_2015"

MyData <- 

# Verif
head(MyData)
dim(MyData)
summary(MyData)


# Corrélation entre les variables, matrice de correlation
pairs(MyData[,2:6])
cor(MyData[,2:6])

##****** cf dessous Note valeurs aberrantes
# MyData[76,5]
# MyData[76,5] <- 350

#### hist(MyData$MedSpe)
#2- Préparer les données pour l'analyse multivariée 
# 

MyData2 <- data.frame(Lat=scale(MyData$Lat),NivVie=scale(MyData$NivVie), TXCh=scale(MyData$TXCh),
		MedSpe=scale(MyData$MedSpe),row.names=MyData$Dep)

##!!****** Note valeurs aberrantes
hist(MyData2$MedSpe)
hist(MyData$MedSpe)

# Verif l'organisation des données
dim(MyData2)
head(MyData2)
MyData2

#trouver 3 départements ... 


# 2- Distance euclidienne
# Le calcul des distances euclidiennes s'effectue avec la fonction dist ()
# la fonction dist () calcule: sqrt(sum((xi - yi)^2))

# Exemple: calcul distance euclidienne entre Ain et Aisne
head(MyData2)
# 
x1 <- MyData2[1,]
x1
y1 <- MyData2[2,]
y1

sqrt(sum((x1 - y1)^2))
dist(MyData2[1:2,])

## question


dist(MyData2)

# afficher tous les résultats sous forme de matrice (les valeurs sont arrondies 2 chiffres après la virgule
as.matrix(round(dist(MyData2),2))
# afficher tous les résultats pour le premier département
as.matrix(round(dist(MyData2),2))[1,]

##

# 3- Classification hiérarchique
### Utiliser toujours des données centrées réduites ########
# la fonction hclust () est appliquée sur les distances euclidiennes entre les variables

# calcul de toutes les distances euclidiennes
DDist <- dist(MyData2, method = "euclidean")
DDist

dim(DDist)
range(DDist)

# classification hclust()

hc <- hclust(DDist)
hc

# Graphique
x11() ; plot(hc)

## Nombre de groupes que l'on souhaite retenir
# exemple 7 groupes
Clusters <- cutree(hc, k=7) 
Clusters 

# ajouter colonne à data.frame
MM3 <- cbind(MyData2, Clust = Clusters)
MM3

#### Visualiser le résultat avec une carte
## Carte
nk <- 7
Dep <- names(Clusters); Dep

coul <- rainbow(nk , 0.3, 1, 0, 0.9)

library(maps)

x11()

plot(c(-5,10),c(41,51.5),xlab="longitude", ylab="latitude",
col=NULL, main="Classification hiérarchique") 
for (i in 1:nk ){
map("france",region= Dep[Clusters == i], exact=F, col=coul[i], 	add=T, fill=T)
}

legend (-5, 45 , as.character(1:nk), fill = coul, horiz=F)
###################



# Afficher la liste des départements tombant dans un cluster, ex Cluster=6
cc <- 2
CC <- Clusters[Clusters == cc]
CC

# Quel sont les caractéristiques moyennes du cluster 6 ? 
DD <- data.frame(CN=CC,
	LatN=MyData2$Lat[Clusters == cc],
	NivVieN=MyData2$NivVie[Clusters == cc], 
	TXChN=MyData2$TXCh[Clusters == cc], 
	MedSpeN=MyData2$MedSpe[Clusters == cc])

DD

apply(DD [,2:5], c(2), mean)
sapply(DD[,2:5], mean)


# Exercice





###########

# Exercice


###################################################################
## 4- Analyse en composante principale

PCR <- princomp(MyData2, cor = TRUE)

summary(PCR)
attributes(PCR)

VarEx <- (PCR$sdev^2 / sum(PCR$sdev^2))*100

plot(VarEx, xlab="Comp", ylab="% variance") 

loadings(PCR)



PC1 <- PCR$scores[,1] ; round(PC1, 2); range(round(PC1, 2))

PC2 <- PCR$scores[,2] ; round(PC2, 2)


## Verification

cor(MyData2, PC1)

loadings (PCR)

cor(MyData2, PC2)

####



## Graphique

biplot(PCR)

# Plot - Alternative
plot(PCR$scores[,1]/PCR$sdev[1], PCR$scores[,2]/PCR$sdev[2], xlab="PCA 1",
ylab="PCA 2", type="n")

arrows(0,0,PCR$loadings[,1]*PCR$sdev[1],PCR$loadings[,2]*PCR$sdev[2],
length=0.1, angle=20, col="red")

text(PCR$loadings[,1]*PCR$sdev[1]*1.2,PCR$loadings[,2]*PCR$sdev[2]*1.2,
rownames(PCR$loadings), col="red", cex=0.7)

text(PCR$scores[,1]/PCR$sdev[1],PCR$scores[,2]/PCR$sdev[2], rownames(PCR$scores),col="blue", cex=0.7)



#### Carte PC1 / PC2
PComp <- PC1
range(PComp )

seuils <- seq(-4,4,by=1)

Dep <- names(PComp) ; Dep

coul <- rainbow(9,0.5,1,0,0.9)


plot(c(-5,10),c(41,51.5),xlab="longitude", ylab="latitude",
col=NULL, main="")

for (i in 1:9){
map("france",region= Dep[PComp >= seuils[i]], exact=T, col=coul[i], add=T, fill=T)
}

legend(-5, 47 , rev(paste("[",-4:4)), fill = rev(coul), horiz=F)


### Exercie






