#Packages#
library(MASS)
library(ade4)
library(RColorBrewer)

###########################################################################################DONNEES########################################################################################
##########################################################################################################################################################################################
#Importation du jeu de donnees#
table=read.table('~/Desktop/Mamo_Table.data',header=FALSE,sep=",")
colnames(table)=c("Anomalie","Age","Forme","Marge","Densite","Severite")
table$Severite=as.factor(table$Severite)
#Discretisation de l'attribut age#
table <- subset(table, table$Age != "?")
table$Age=cut(as.numeric(table$Age),breaks=c(as.numeric(quantile(as.numeric(table$Age))[1]),as.numeric(quantile(as.numeric(table$Age))[2]),as.numeric(quantile(as.numeric(table$Age))[3]),as.numeric(quantile(as.numeric(table$Age))[4]),as.numeric(quantile(as.numeric(table$Age))[5])))
levels(table$Age)=c("1","2","3","4")
#Eviction des donnees manquantes#
table=replace(table,table=="?",NA)
table=na.omit(table)
#Eviction de donnees aberantes#
table$Anomalie=replace(table$Anomalie,table$Anomalie==55,5)
table=droplevels(table)
attach(table)

#Visulaisation peu performante des corrélations#
pairs(~Anomalie+Age+Forme+Marge+Densite,data=table,main="Simple Scatterplot Matrix",pch=20,col=Severite)


#############################################################################################AFCM##################################################################################################################################################################################################################################################################################

#Les variables sont qualitatives on pense à l'analyse factorielle de correspondances multiples


#On réalise l'AFCM en choisissant 2 axes#
acmsev <- dudi.acm(table,scannf=FALSE,nf=2)

#Il est possible de visualiser les valeurs propres#
barplot(acmsev$eig)

#On ecrit une fonction keepvar pour determiner le nombre d'axes a conserver pour un certain seuil d'information du modèle (ici 70%)#
var=acmsev$eig/sum(acmsev$eig)
keepvar= function (seuil){
	res=0
	for(i in 2:(length(var)+1)){
		res=res+var[i-1]
		if(res>seuil){
			print(i)
			print(res)
			break
		}
	}
}

keepvar(0.7)
#On obtient 12 pour conserver 73% de l'information du modèle#

#On retient donc les 16 premiers axes#
acmsev <- dudi.acm(table,scannf=FALSE,nf=16)

#Il est possible d'obtenir la répartition dans les différentes classes de chaque attributs sous forme nominal#
matX <- as.matrix(acm.disjonctif(table))
matD <- diag(1/dim(acmsev$tab)[1],dim(acmsev$tab)[1],dim(acmsev$tab)[1])
mat1n <- rep(1,dim(acmsev$tab)[1])
t(matX)%*%mat1n

#Il est possible d'obtenir la répartition dans les différentes classes de chaque attributs sous forme de pourcentages#
t(matX)%*%matD%*%mat1n

#Equivalent aux fréquences relatives associés à chaque variable#

frequences <- t(matX)%*%matD%*%mat1n
matDm <- diag(as.numeric(frequences),dim(t(matX)%*%mat1n)[1],dim(t(matX)%*%mat1n)[1])
round(matDm,4)

#Il est aussi possible de l'avoir sous forme plus exploitable#
sapply(1:6, function(x) round(summary(table[,x])/dim(acmsev$tab)[1],4))

#Il est possible de retrouver table$tab avec#
matDmm1 <- diag(1/as.numeric(frequences),dim(t(matX)%*%mat1n)[1],dim(t(matX)%*%mat1n)[1])
mat1nm <- matrix(1,nrow=dim(acmsev$tab)[1],ncol=dim(t(matX)%*%mat1n)[1])
matY <- matX%*%matDmm1-mat1nm
round(matY,4)


#Cercle de correlation des variables dans l'espace projeté#
s.arrow(acmsev$co,clabel = 0.5)

#Boxplot des différentes valeures projetées sur le premier axe#
boxplot(acmsev,col=brewer.pal(6, "Set1"))

#Distribution des variables sur le premier axe#
score(acmsev,xax=1)

#Représentation par classes des données projetées#
scatter(acmsev, col = brewer.pal(6, "Set1"))


#Représentation par classes de la variable de réponse et des résultats de la biopsie#
par(mfrow=c(1,2))
s.class(acmsev$li, Severite, col = brewer.pal(6, "Set1") )
s.class(acmsev$li, Anomalie, col = brewer.pal(6, "Set1") )

###################################################################################################################################################################################################################################################################################BONUS####################################################################################################################################################################################################################################################################################

#Essai d'autres types de visualisation#

s.potatoe <- function(dfxy, fac, xax = 1, yax = 2, col.border = rep(1, length(levels(fac))), col.fill = rep(1, length(levels(fac))), shape = -0.5, open = "FALSE", ...) {
	dfxy <- data.frame(dfxy)
	opar <- par(mar = par("mar"))
	par(mar = c(0.1, 0.1, 0.1, 0.1))
	on.exit(par(opar))
	x <- dfxy[, xax]
	y <- dfxy[, yax]
	for (f in levels(fac)) {
		xx <- x[fac == f]
		yy <- y[fac == f]
		xc <- chull(xx, yy)
		qui <- which(levels(fac) == f)
		border <- col.border[qui]
		col <- col.fill[qui]
		xspline(xx[xc], yy[xc], shape = shape, open = open, border = border, col = col, ...)
	}
}

shape=1

col.border <- c("red", "blue")
col.fill <- c(rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.3))


bkg <- function(...) s.class(acmsev$li, Severite, inc = FALSE, cell = 0, adda = FALSE, cst = 0, col = col.border, clab = 1.5, ...)


bkg(sub = paste("shape =", round(1, 2)), csub = 2.5)
s.potatoe(acmsev$li, Severite, col.border = col.border, shape = shape, col.fill = col.fill)

######################################################################################

shape=1

col.border <- c("red", "green", "blue","orange")
col.fill <- c(rgb(1, 0, 0, 0.3), rgb(0, 1, 0, 0.3), rgb(0, 0, 1, 0.3),rgb(1,0.7,0.3 ,0.3))


bkg <- function(...) s.class(acmsev$li, Anomalie, inc = FALSE, cell = 0, adda = FALSE, cst = 0, col = col.border, clab = 1.5, ...)


bkg(sub = paste("shape =", round(1, 2)), csub = 2.5)
s.potatoe(acmsev$li, Anomalie, col.border = col.border, shape = shape, col.fill = col.fill)




