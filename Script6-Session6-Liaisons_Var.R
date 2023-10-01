### Script 6 ### Liaison entre 2 variables

# Importer le fichier de donn�es: "Data_INSEE_2015"
# utiliser fonction read.table
# Nommer l'objet data.frame "MyData"
# Compl�ter

MyData <- 



# Verification
head(MyData)

#1- Centrer et r�duire une variable

# a- Centrer une variable
# ex:
mean(MyData$Esp)	# verif

EspC <- MyData$Esp - mean(MyData$Esp) # centrer
# Afficher resultat
EspC 

# verification
range(MyData$Esp)
range(EspC)

# Verif graphique
windows(10,5)
par(mfrow=c(1,2))
par(mar=c(4.5,4.5,1,1))

hist(MyData$Esp)
abline(v=mean(MyData$Esp), col=2)
hist(EspC)
abline(v=0, col=2)


# centrer puis r�duire la variable
mean(MyData$Esp)	# verif
sd(MyData$Esp)	# verif

EspCR <- (MyData$Esp - mean(MyData$Esp)) / sd(MyData$Esp)
EspCR 

# methode 1
par(mfrow=c(2,2))
hist(scale(MyData$Esp))
hist(scale(MyData$NivVie))
hist(scale(MyData$TXCh))
hist(scale(MyData$MedSpe))


# methode 2
temp <- sapply(MyData[ ,3:6], scale)


hist(temp[,1])
hist(temp[,2])
hist(temp[,3])
hist(temp[,4])



# Verification
range(round(EspCR ,2))

windows(10,3)
par(mfrow=c(1,3))
par(mar=c(4.5,4.5,1,1))

hist(MyData$Esp)
abline(v=mean(MyData$Esp), col=2)
hist(EspC)
abline(v=0, col=2)
hist(EspCR)
abline(v=0, col=2)

#### Exercice 






# 2- Relation entre 2 variables
# verifier les donn�es
head(MyData)
dim(MyData)
range(MyData$NivVie)
range(MyData$Esp)

pairs(MyData[,c(3,6)])	# matrice de nuages de points

# Covariance et correlation

cov(MyData$NivVie, MyData$Esp)

(sum((MyData$NivVie-mean(MyData$NivVie))*(MyData$Esp-mean(MyData$Esp))))/(96-1)


cov(NivVieCR, EspCR)

cor(NivVieCR, EspCR)

sum(NivVieCR*EspCR) / sqrt(sum(NivVieCR^2) * sum(EspCR^2))

cor(MyData$NivVie,MyData$Esp)


# test de corr�lation

cor.test(MyData$NivVie,MyData$Esp)

attributes(cor.test(MyData$NivVie,MyData$Esp))

cor.test(MyData$NivVie,MyData$Esp)$estimate

cor.test(MyData$NivVie,MyData$Esp)$p.value


#3 - R�gression lin�aire (m�thode des moindre carr�s)
# a- Utiliser lsfit() 
windows(10,5)
par(mfrow=c(1,2))

# Graph1
plot(MyData$NivVie,MyData$Esp)
abline(lsfit(MyData$NivVie,MyData$Esp), col=3)
abline(v=mean(MyData$NivVie), col=2)
abline(h=mean(MyData$Esp), col=2)

# coordonn�es du point moyen centr�
pp <- c(mean(MyData$NivVie), mean(MyData$Esp))
pp
points(pp[1], pp[2],col=4, cex=2, lwd=2)


# Graph2
plot(MyData$Esp,MyData$NivVie)
abline(lsfit(MyData$Esp,MyData$NivVie), col=3)
abline(v=mean(MyData$Esp), col=2)
abline(h=mean(MyData$NivVie), col=2)

# coordonn�es du point moyen centr�
pp <- c(mean(MyData$Esp), mean(MyData$NivVie))
pp
points(pp[1], pp[2],col=4, cex=2, lwd=2)

# Equation de la droite
LS1 <- lsfit(MyData$NivVie,MyData$Esp)
LS1

attributes(LS1)

#
coefficients(LS1)
coefficients(LS1)[[1]]
coefficients(LS1)[[2]]

cov(MyData$NivVie,MyData$Esp) / var(MyData$NivVie)

# Pr�dire 
coefficients(LS1)[[2]]*20000+coefficients(LS1)[[1]]

(83-coefficients(LS1)[[1]])/coefficients(LS1)[[2]]


# Si on permute les axes, les r�sultats sont l�g�rement diff�rents

LS2 <- lsfit(MyData$Esp,MyData$NivVie)
attributes(LS2)

#
coefficients(LS2)
coefficients(LS2)[[1]]
coefficients(LS2)[[2]]

coefficients(LS2)[[2]]*83+coefficients(LS2)[[1]]
(20000-coefficients(LS2)[[1]])/coefficients(LS2)[[2]]



## b- Mod�le linaire, lm()
LM1 <- lm(Esp ~ NivVie, data=MyData) 
LM1

# Pr�dire
P1 <- data.frame(NivVie=20000)
predict (LM1, P1)

predict (LM1, P1, interval="confidence")

# Exercice




### R�sum� des informations fournies par lm
summary(LM1)
summary(LM1)$r.squared

summary(LM2)
summary(LM2)$r.squared

### Toutes les infos fournies par lm
attributes(LM1)

# extraire les R�sidus
LM1$residuals


windows(10,5)
par(mfrow=c(1,2))

hist(LM1$residuals)
hist(LM2$residuals)

# Verif / corr�lation
summary(LM1)$r.squared

1-sum(summary(LM1)$residuals^2) / sum(((MyData$Esp-mean(MyData$Esp))^2))

sqrt(summary(LM1)$r.squared)

cor(MyData$Esp, MyData$NivVie)


## c- Mod�le de r�gression lin�aire multiple
# exemple, pr�dire Esp�rance de vie en fonction des Lat, NivVie, TXCh, MedSpe
LM3 <- lm(Esp~Lat+NivVie+TXCh+MedSpe, data= MyData)
LM3

summary(LM3)

sqrt(summary(LM3)$r.squared)

# pr�dire
P3 <- data.frame(Lat = 47, NivVie=20000, TXCh = 9.5, MedSpe=80)
predict (LM3,P3)


# comparer mod�le et obs pour �valuer la performance du mod�le

P4 <- data.frame (Lat = MyData$Lat, NivVie = MyData$NivVie, TXCh = MyData$TXCh , MedSpe= MyData$MedSpe)

ModelEsp <- predict(LM3, P4)

ModelEsp 

plot(MyData$Esp,ModelEsp)
abline(lsfit(MyData$Esp,ModelEsp), col=2)
cor.test(MyData$Esp,ModelEsp)

####### Maps 
#####	Verif Carte	########################

range(scale(MyData$Esp))
range(scale(ModelEsp))

#### Map Observations
EspCR <- scale(MyData$Esp)
range(EspCR)
##
library(maps)
seuil <- seq(from=-4,to=4, by=1)
length(seuil)

coul1 <- rev(rainbow(4,s=seq(from=0.2,0.8, length.out=4),start=0.65, end=0.7))
coul2 <- rev(rainbow(5,s=seq(from=1,0.1, length.out=5),start=0, end=0.2))
coul <- c(coul1,coul2)


plot(c(-5,10),c(41,51.5),xlab="longitude", ylab="latitude",
col=NULL, main="Ano stand. de l'esperance de vie en 2015") 
for(i in 1:8){
aa <- seuil [i]
bb <- seuil [i+1]

DDep <- as.character(MyData$Dep[EspCR >= aa & EspCR < bb])
if (length(DDep) > 0){
map("france", regions= as.character(MyData$Dep[EspCR >= aa & EspCR < bb]), fill = TRUE, col = coul[i], add=T, exact=F) 
}
}

legend(8,47,paste(paste("[",paste(seuil[1:8], seuil[2:9], sep=";")),")"), fill=coul, cex=0.75, title="", h=F)

###############

#### Map Model
EspCR <- scale(ModelEsp)

range(EspCR)
##
library(maps)
seuil <- seq(from=-4,to=4, by=1)
length(seuil)

coul1 <- rev(rainbow(4,s=seq(from=0.2,0.8, length.out=4),start=0.65, end=0.7))
coul2 <- rev(rainbow(5,s=seq(from=1,0.1, length.out=5),start=0, end=0.2))
coul <- c(coul1,coul2)


plot(c(-5,10),c(41,51.5),xlab="longitude", ylab="latitude",
col=NULL, main="Ano stand. de l'esperance de vie en 2015 - Model") 
for(i in 1:8){
aa <- seuil [i]
bb <- seuil [i+1]

DDep <- as.character(MyData$Dep[EspCR >= aa & EspCR < bb])
if (length(DDep) > 0){
map("france", regions= as.character(MyData$Dep[EspCR >= aa & EspCR < bb]), fill = TRUE, col = coul[i], add=T, exact=F) 
}
}

legend(8,47,paste(paste("[",paste(seuil[1:8], seuil[2:9], sep=";")),")"), fill=coul, cex=0.75, title="", h=F)


