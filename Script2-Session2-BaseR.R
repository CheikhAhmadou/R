###############################################
# Script R Session 2- Les bases du Langage R
###############################################
# 1- Cr�er un objet R / assignement
####################################
# Afficher la liste des objets cr��s
objects()
#ou
ls() # lister tous les objets créés

a

# cr�er "a" contenant la valeur 1
a <- 1
# ou, autre fa�on, cr�er b contenant la valeur 2
assign( "b" , 2) # c'est la méme chose que b<-2

# autres exemples
bb <- 3+5
bb
cc <- 10+4*5
cc
dd <- bb-cc
dd

# Exercice


## creer un objet vecteur numerique
# concatener des chiffres
v1 <- c(1,2,3,4)
v2 <- c(6, 8, 3, 1, 10) 	
v2
v3 <- c(v1,v2)	# Concatener 2 vecteurs
v3
v4 <- c(v1, v2, v3)	
v4
v5 <- v4+1
v5			

# Exercice




# Creer des s�quences de nombres avec ":" ou "seq()" ou "rep()"
s1 <- 0:10
s1
s2 <- 5:-4
s2
s3 <- 1.5:8.5
s3
s4 <- seq(from=0, to=10, by=2)
s4
#s4 est la s�quence de nombres entre 0 et 10 d�intervalle 2
s5 <- seq(from=1, to= 10, length.out=3)
s5
# s5 est la s�quence de 3 nombres d'intervalle r�gulier entre 1 et 10

r1 <- rep(1, times = 5)	# On replique 5 fois le chiffre 1 
r1
r2 <- c(1,5,7,3)
r2

r3 <- rep(r2,each=2)
r3

r4 <- rep(r3, times=5, each=2)
r4

#pour plus d�informations ?rep

#Utiliser plusieurs fonctions dans une seule ligne de commande
rep( c(1,2,5), times=3)			# 2 fonctions
mean( rep (c(1,2,5), times=3))		# 3 fonctions
round(mean (rep (c(1,2,5), times=3)), 2)	# 4 fonctions

#Exercice


#Extraire les valeurs d�un vecteur
vA <- c(132,98,153,118,136)
vA[2]
vA[1 :3]
c(vA[2], vA[4 :5])

# Operateurs logiques et de comparaison
x <- 1 :10
x
x>5
length(x [x > 2])
range(x [x > 2])	#expliquer la fonction range
sum(x [x > 2])
mean(x [x > 2])
length(x [x > 2 & x != 5])

# Exercice


# Vecteur numerique, vecteur logique
 vn <- c(0,1,1,0,1)
class(vn)
vl <- as.logical(vn)
vl
class(vl)
vn <- as.numeric(vl)
class(vn)
vn

# Vecteur caractere
c1 <- c("a","b","c","d")
c1
LL <- letters[1:10]
LL
class(LL)
LL[2]
# avec la function paste()
c3  <- paste(1:5)
c3
c4 <- paste("A", 1:5, sep = "")
c4
c5 <- paste("Voiture", 1:5, sep = "")
c5
class(c5)

# vecteur numerique/caractere

cc <- c(1,3,4)
class(cc)
cc1 <- as.character(cc)
cc1
class(cc1)
cc2 <- as.numeric(cc1)
cc2
class(cc2)

# Combiner un vecteur numeric et un vecteur character
vA
vv <- c("A1", "A2", "A3", "A4", "A5")
Tabb <- cbind(vv,vA)

# ou tableau de donn�es
Tabb2 <- data.frame(Arret = vv, Freq = vA)


# Attributs d�un objet
mode(sum)
mode(mean)
mode(cc2)
mode(c5)
class(sum)
class(mean)
class(cc2)
class(c5)


######## Plot Graphs and Maps ################################
