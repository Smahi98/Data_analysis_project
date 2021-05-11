library(FactoMineR)
library(ggplot2)
library(factoextra)
library("readxl")

#importer le dataset
qualita <- read_excel("qualita.xlsx")


qualita$Open<-as.factor(qualita$Open)
qualita$High<-as.factor(qualita$High)
qualita$Low<-as.factor(qualita$Low)
qualita$Close_t<-as.factor(qualita$Close_t)
qualita$Volume<-as.factor(qualita$Volume)
qualita$Lower_Band<-as.factor(qualita$Lower_Band)
qualita$MAM<-as.factor(qualita$MAM)
qualita$MAMK<-as.factor(qualita$MAMK)
qualita$EMA10<-as.factor(qualita$EMA10)
qualita$EMA20<-as.factor(qualita$EMA20)
qualita$MACD<-as.factor(qualita$MACD)
qualita$ATR<-as.factor(qualita$ATR)
qualita$RSI<-as.factor(qualita$RSI)
qualita$SO.K<-as.factor(qualita$SO.K)
qualita$QQQ_Close<-as.factor(qualita$QQQ_Close)




res<-tab.disjonctif(qualita)

#caclul du frequence de chaque modalit�
nrow(res)
apply(res,2,sum)
fr<-apply(res,2,sum)/nrow(res)
#Applcation de ACM 
facto<-MCA(qualita, ncp = 5,graph = FALSE )
facto

#nombre de qualitaiables
s <- ncol(qualita) 
s
#nombre d'observation
n <- nrow(qualita)
n
#nombre de modalites  
p <- sum(unlist(lapply(qualita,function(x){length(levels(x))}))) 
p
#calcul les valeurs propres
facto$eig
plot(1:15,facto$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres")

#Nuage de modalites

#calcul du cosinus carree de chaque modalite
facto$var$cos2

#calcul de la moyenne pour detereminer les modalires les biens presentes
mean(facto$eig[,1])

#contribution de chaque modalite
contrib <- facto$var$contrib
contrib
fviz_contrib(facto, choice = "var", axes = 1, top = 30)
fviz_contrib(facto, choice = "var", axes = 2, top = 30)
fviz_contrib(facto, choice = "var", axes = 3, top = 30)
fviz_contrib(facto, choice = "var", axes = 4, top = 30)
fviz_contrib(facto, choice = "var", axes = 5, top = 30)
#CAH de la contibutions des modalit�s
cont<-HCPC(contrib)

#nuage des modalites avec leurs valeur de contribution
plot(facto,choix="ind",invisible="ind")
fviz_mca_var(facto, col.qualita = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)

#nuages des individus 
#calcul du cos2
facto$ind$cos2

#contribution des Individus
contrib <-facto$ind$contrib
contrib
fviz_contrib(facto, choice = "ind", axes = 1, top = 200)
fviz_contrib(facto, choice = "ind", axes = 2, top = 200)
fviz_contrib(facto, choice = "ind", axes = 3, top = 200)
fviz_contrib(facto, choice = "ind", axes = 4, top = 200)
fviz_contrib(facto, choice = "ind", axes = 5, top = 200)


#CAH de la contibutions des individus
resul<-HCPC(contrib)

#Nuage des qualitaiables

#les coefficients de corr�lation 
facto$var$eta2

#graph des coef de corr
plot(facto,choix="var")
