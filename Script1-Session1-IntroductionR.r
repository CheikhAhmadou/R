###############################################
# Script 1 - Introduction R
###############################################
# http://www.r-project.org/
# https://cran.r-project.org/mirrors.html

# Fixer le répertoir courant
# setwd("")

#verification liste packages installés
library()

# Installation package
install.packages("maps")
install.packages("fields")
install.packages("spam")

# Obtenir information sur la fonction install.packages
? install.packages


# Activation package
library(maps)
library(fields)
library(spam)

# Obtenir information sur la fonction library
? library


# Exemple 1 (cf section "example" dans package maps, fonction map
# Instructions: executer chaque ligne séparément et observer ce que cela produit
# Afficher carte du monde/usa/France
map()
map("usa")
map("france")
? map	# info fonction map


# Exemple 2
# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
# afficher le contenu du jeu de données ozone
ozone
?ozone # infos
# afficher le haut puis le bas du jeu de données
# vous pouvez fixer le nombre de lignes à afficher en ajoutant une , et le nombre de lignes, ex: 10)
head(ozone)
tail(ozone)
# commentaire: x represente les longitudes des points d'observation
# (degres decimal, ici nous sommes à l'ouest car les valeurs sont négatives
# y represente les latitudes
# median sont les concentrations medianes d'ozone, unité ppb



# Fonctions mathématiques


# Fonctions de calcul élémentaires


# Liens
# Debuter avec R
# Liste tutos en ligne en français
# https://cran.r-project.org/other-docs.html#nenglish
# https://cran.r-project.org/doc/contrib/Paradis-rdebuts_fr.pdf


# Analyse spatiale avec R
# https://rspatial.org/intr/index.html
# https://rspatial.org/raster/spatial/1-introduction.html/

# R graph gallery
# http://www.r-graph-gallery.com/	


