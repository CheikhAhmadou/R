### Explorer les donn�es ######

# 1- donn�ées non spatialis�es, exemple 

# Lire le fichier "FranceRoad-Pkm_1998-2016.csv"
# Nommer l'objet PKM


# utiliser les fonctions head, tail, dim, min, max, mean, 
# median, quantile, summary

# transformer un vecteur numerique en serie temporelle
# ts(), verifier avec class

# Graphique
# ajouter une droite de regression sur le graphique: abline, lsfit


# Autres types de donn�es non spatialis�es, data()


#########################################################
# 2- donn�es saptialis�es
# exemple: USArrests (dataset dans R)
# appliquer les fonctions, class, attributes, dim, nrow, ncol, names, rownames

# summary

#########
# autre exemple: lire le fichier "Data_oz_20sts.txt"�


# appliquer les fonctions, class, attributes, dim, nrow, ncol, names, rownames

# appliquer summary (r�sum� num�rique)

# graphique: boxplot (boite de dispersion)


########
# Autre exemple	# Matrice 
# donn�es spatialis�es en point de grille (Lon/Lat)

readLines("Temp.csv", n=10)

# Dans les metadata, on extrait les longitudes puis les latitudes
temp <- scan("Temp.csv",nlines=6, what="character", sep=";")
Lon <- as.numeric(temp[7:11])
Lat <- as.numeric(temp[15:18])

# Ensuite, on extrait les donn�es
temp2 <- scan("Temp.csv", skip=6, sep=";")
temp2

# On organise les donn�es en matrice (ici on a 5 lon et 4 lat)
aa1 <- matrix(temp2,c(5,4))

# on represente l'image
library(fields)
image.plot(Lon, Lat, aa1)

summary(aa1) ## !! pas adapt� pour les matrices

# Caluls sur l'ensemble des valeurs
mean(aa1)
range(aa1)
sd(aa1)

# Appliquer des calculs  en ligne (lon) / col (lat)
apply(aa1, c(1), min)
apply(aa1, c(1), mean)

apply(aa1, c(2), min)
apply(aa1, c(2), mean)

# Visite : https://www.rspatial.org/intr/7-explore.html



#######################################################
### Repr�sentation Graphique
## objets g�om�triques: point, ligne, segment, polygone

# Visite: https://www.r-graph-gallery.com/base-R.html

# D�finir les marges

par (mar=c(4.5, 4.5, 3, 1))
par (oma=c(1, 1, 1, 1))

# zone graphique 
#1- D�finir axe x puis axe y

xx <- c(1,15) # valeur min, valeur max
yy <- c(1,8) # valeur min, valeur max


# afficher zone graphique + titre...
plot(xx, yy, type="n", main = "Dessin")

#  points
points(3,4)
points(3, 4, col=2, pch=2, cex=4)
points(c(2,6,12), c(1,8,5), col=3, pch=3, cex=1.5)

# ? points

#texte, c'est comme pour point

text(10, 4,"Texte 1", col=6)

text(11, 5,"Texte #2?", col="orange", font=3, family="mono",cex=3)

# ligne hor/ver
abline(h=6, col=4)

abline(v=7, col=5, lty=2, lwd=1)

#segments
segments(x0=3, y0=7, x1 = 14, y1 = 2, col="pink", lwd=3)

#fl�che ## comme segment
arrows(x0=3, y0=7, x1 = 10, y1 = 2, length = 0.1, angle = 20,code = 2, 	col=2)


#cercle
library(plotrix)

draw.circle(13,7,0.75,col=2,border="purple",  lwd=2)

#rectangle

rect(xleft=4.5, ybottom=1, xright=5, ytop=4, col=3, border=1) 

#polygones

polygon(c(1,3,2), c(2,2,6), col=7, border=3, lwd=3)

polygon(c(14,14.5,14.75,14), c(2,2,3,4.2), col=4, border=3, lwd=1)







