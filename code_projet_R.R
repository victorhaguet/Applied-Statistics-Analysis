#@author - Pierre Lague & Victor Haguet
#@date - 15/12/2021

# Projet de programmation logiciels statistiques
# NB : toutes les interprétations des résultats du programme sont dans le compte rendu de l'étude

#packages graphes

library('tidyverse')
library(ggpubr)
library("corrplot")

#packages test de la normalité

library(stats)
library(goftest)

#package acp

library(FactoMineR)
library(factoextra)

heart<-read.table("./archives.txt", header = TRUE, sep = ",", ,fileEncoding = "utf16")
dim(heart)
heart<-heart[,c("age","trestbps","chol","thalach","oldpeak","target")]
names(heart) <- c("age", "trestbps", "chol", "thalach", "oldpeak", "target")
head(heart,10)

heart <- filter(heart, trestbps < 172)
heart <- filter(heart, chol < 394)
heart <- filter(heart, thalach > 71)
heart <- filter(heart, oldpeak < 4.2)


## Boxplots des variables explicatives

ageP<-ggplot(data=heart, aes(x=as.factor(target), y=age)) +
  geom_boxplot()+
  labs(x="Malade", y="Age")

  
  
trestbpsP<-ggplot(data=heart, aes(x=as.factor(target), y=trestbps)) +
  geom_boxplot()+
  labs(x="Malade", y="Trestbps")

cholP<-ggplot(data=heart, aes(x=as.factor(target), y=chol)) +
  geom_boxplot()+
  labs(x="Malade", y="Chol")

thalachP<-ggplot(data=heart, aes(x=as.factor(target), y=thalach)) +
  geom_boxplot()+
  labs(x="Malade", y="Thalach") 

oldpeakP<-ggplot(data=heart, aes(x=as.factor(target), y=oldpeak)) +
  geom_boxplot()+
  labs(x="Malade", y="Oldpeak")

arrange<-ggarrange(ageP,trestbpsP,cholP,thalachP,oldpeakP,
          ncol = 2, nrow = 3)

## Analyse univari?e

## Courbe de densit?s de chaque variable explicatives

# age

univar_age = ggplot(data=heart, aes(age)) +
  geom_histogram(aes(y=stat(density)),fill ="#EE9988", colour = "black", bins=10) +
  geom_density(col="blue", adjust = 1.5)+
  ggtitle("Age de la population etudiee") +
  xlab("Age (en annee)") +
  ylab("Effectifs")

# trestbps

univar_trestbps = ggplot(data=heart, aes(trestbps)) +
  geom_histogram(aes(y=stat(density)),fill ="#EE9988", colour = "black", bins = 10) +
  geom_density(col="blue", adjust = 1.5)+
  ggtitle("Pression sanguine de la population") +
  xlab("trestbps") +
  ylab("Effectifs")

# chol

univar_chol = ggplot(data=heart, aes(chol)) +
  geom_histogram(aes(y=stat(density)),fill ="#EE9988", colour = "black", bins = 12) +
  geom_density(col="blue", adjust = 1.5)+
  ggtitle("Taux de cholesterol de la population") +
  xlab("chol") +
  ylab("Effectifs")

# thalach

univar_thalach = ggplot(data=heart, aes(thalach)) +
  geom_histogram(aes(y=stat(density)),fill ="#EE9988", colour = "black", binwidth = 13) +
  geom_density(col="blue", adjust = 1.5)+
  ggtitle("Rythme cardiaque maximale de la population") +
  xlab("thalach") +
  ylab("Effectifs")

#oldpeak

univar_oldpeak = ggplot(data=heart, aes(oldpeak)) +
  geom_histogram(aes(y=stat(density)),fill ="#EE9988", colour = "black", bins = 14) +
  scale_x_continuous(name="oldpeak", limits=c(-1.2, 5.2))+
  geom_density(col="blue", adjust = 1.5)+
  ggtitle("Rythme cardiaque apres un effort de la population") +
  ylab("Effectifs")

# test de la normalité 

# age 

meanAge<-mean(heart$age)
sdAge<-sd(heart$age)
ks.test(heart$age,"pnorm", mean=meanAge, sd=sdAge)
shapiro.test(heart$age)

# trestbps 

meanTrestbps<-mean(heart$trestbps)
sdTrestbps<-sd(heart$trestbps)
ks.test(heart$trestbps,"pnorm", mean=meanTrestbps, sd=sdTrestbps)
shapiro.test(heart$trestbps)

#chol

meanChol<-mean(heart$chol)
sdChol<-sd(heart$chol)
ks.test(heart$chol,"pnorm", mean=meanChol, sd=sdChol)
shapiro.test(heart$chol)

#thalach

meanThalach<-mean(heart$thalach)
sdThalach<-sd(heart$thalach)
ks.test(heart$thalach,"pnorm", mean=meanThalach, sd=sdThalach)
shapiro.test(heart$thalach)

#oldpeak

meanOldpeak<-mean(heart$oldpeak)
sdOldpeak<-sd(heart$oldpeak)
shapiro.test(heart$oldpeak)
shapiro.test(heart$oldpeak)

# Analyse Bivariée :

#matrice de corrélation

heart.cor <- cor(heart)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot::corrplot(heart.cor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
                   col = col(200), addCoef.col = "black", cl.pos = "n", order = "AOE")



#regression logistique de notre variable dépendante en fonction des variables indépendantes

myModel1 <- glm(target~age, data=heart, start=c(0, 0), family='binomial', control=list(maxit=1000, trace=TRUE, epsilon=1e-16))
summary(myModel1)

myModel1 <- glm(target~thalach, data=heart, start=c(0, 0), family='binomial', control=list(maxit=1000, trace=TRUE, epsilon=1e-16))
summary(myModel1)

myModel1 <- glm(target~chol, data=heart, start=c(0, 0), family='binomial', control=list(maxit=1000, trace=TRUE, epsilon=1e-16))
summary(myModel1)

myModel1 <- glm(target~oldpeak, data=heart, start=c(0, 0), family='binomial', control=list(maxit=1000, trace=TRUE, epsilon=1e-16))
summary(myModel1)

myModel1 <- glm(target~trestbps, data=heart, start=c(0, 0), family='binomial', control=list(maxit=1000, trace=TRUE, epsilon=1e-16))
summary(myModel1)


# analyse multivariee

# methode des clusters

fviz_nbclust(heart, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "methode Elbow")

heart.pca <- PCA(heart, ncp = 100, graph = FALSE)
heart.hcpc <- HCPC(heart.pca, graph = FALSE)

fviz_dend(heart.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8 ,
          main = "Clusters de la méthode sous R"# Augment l'espace pour le texte
)

acp <- PCA(heart,quali.sup=6)
hc <- HCPC(acp,nb.clust=4)
data12 <- hc$data.clust
data12$target<-as.factor(data12$target)
boxplot(oldpeak~clust,data=data12)
table(data12$target,data12$clust)
plot(hc)

#methode ACP

heart.pca <- PCA(heart, ncp = 3, graph = TRUE, quali.sup = 6)
eig.val <- get_eigenvalue(heart.pca)

fviz_eig(heart.pca, addlabels = TRUE, ylim = c(0, 50), ylab="impact sur la target", xlab = "Composantes principales", main = "Impact des composantes principales sur la variable target")
plot(heart.pca,axes=c(1,2),choix="var")
plot(heart.pca,axes=c(1,3),choix="var")
plot(heart.pca,axes=c(2,3),choix="var")
corrplot(heart.pca$var$contrib, is.corr=FALSE)
