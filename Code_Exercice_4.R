#Packages#
library(MASS)
library(ade4)
library(RColorBrewer)
library(corrplot)
 
 
#Importation du jeu de donnees#
table=read.table('~/Desktop/wine.data',header=FALSE,sep=",") 
colnames(table)=c("Identifiant", "Alcool", "Acide_malique", "Cendre", "Alcalinite", "Magnesium", "Phenols", "Flavanoides", "Non_flavanoides", "Proanthocyanidines", "Couleur", "Teinte", "OD", "Proline")
table$Identifiant=as.factor(table$Identifiant)

###########################

#On représente la matrice des corrélations#
corr=cor(table[,-1])
corrplot.mixed(corr)

#On réalise une ACP pour étudier les corélation entre variables#
acp=dudi.pca(table[,-1],scannf=FALSE,nf=2)
res <- scatter(acp, clab.row = 0, posieig = "none")
s.class(acp$li, fac = table$Identifiant, col = brewer.pal(3, "Set1"), add.plot = TRUE, cstar = 0, cellipse = 0)

###########################

#Réalisation d'un AFD sur les variables avec comme réponse le vigneron#
afd=lda(table$Identifiant ~ .,data=table)
plot(afd,col=as.integer(table$Identifiant),pch=20)

#Réalisation de piechart pour les moyennes de chaque vignerons selon chaque attributs#
par(mfrow=c(4,4))
apply(afd$means, 2,  function(x) pie(x,col=brewer.pal(3, "Set1")))

#Réalisation de barchat pour les moyennes de chaque vignerons selon chaque attributs#
mat=as.matrix(afd$means)
par(mfrow=c(4,4))
for(i in 1:13){
	barplot(mat[,i],col=brewer.pal(3, "Set1"),sub=colnames(mat)[i])
}