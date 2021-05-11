#Installation du package readxl
install.packages("readxl")
#chargement du package readxl
library(readxl)
#lecture du fichier excel
data=read_excel("dataset.xlsx",sheet="Feuil1",col_names = TRUE)
#suppression de la date
data=data[,-1]

# Question 1

# y  représente la variable expliquée "Close_forcast"
y=data[,18]
# Détermination de la variable entrante
coefficient=0
j=1
for (i in 1:17) {
  x=data[i:i]
  if ( cor(x,y, method="pearson") > coefficient){
    coefficient=cor(x,y, method="pearson")
    j=i
  }
}
variable_entrante_1=data[j:j]
# donc c'est la variable Close_t  qui a la plus grande corrélation avec y
# On va supprimer la variable entrante du tableau initiale et l'ajouter dans un nouveau tableau qui contient la variable expliquée (d'indice 18)
nouveau_tableau=data[18:18]
data=data[,-j]
nouveau_tableau <- cbind(nouveau_tableau, variable_entrante_1)
# Calcul du modèle de régression de y sur la variable entrante
modele <- lm(Close_forcast~Close_t,data=nouveau_tableau)
summary(modele)
# résidu du modèle de régression
residu=residuals(modele)
# Cherchons maintenant la variable qui a la plus grande corrélation avec le résidu
r=0
j=1
for (i in 1:16) {
  x=data[i:i]
  if ( cor(x,residu, method="pearson") > r){
    r=cor(x,residu, method="pearson")
    j=i
  }
}
# C'est la variable SO.K qui est candidate pour intégrer le modèle
# Son coefficient de corrélation avec le résidu est
print(r)
# D'après la table de la loi de student t(n-2,0,975)=1.96 avec n=200 et alpha=0.05
# if suffit de comparer abs(r)/sqrt((1-r^2)/198) avec t(n-2,0.975)
print(abs(r)/sqrt((1-r^2)/198))
# On voit bien que abs(r)/sqrt((1-r^2)/198) < t(n-2,0.975)
# Donc l'hypothèse de nullité de r est acceptée.
# Le critère d'arrêt est donc vérifié , l'algorithme se termine

# Question 2

# Calcul du R carré et R carré ajusté
modele <- lm(Close_forcast~Close_t,data=nouveau_tableau)
summary(modele)
# Test d'homoscédasticité
plot(predict(modele),resid(modele))

# test de normalité
shapiro.test(resid(modele))
ks.test(resid(modele),pnorm)

# Valeurs aberrantes 
A=sqrt(deviance(modele)/df.residual(modele))
aberant=abs(predict(modele))/A
plot(aberant)
#On remarque que les observations d'indice compris entre 60 et 120 reprèsentent des valeurs abberantes 

# Question 3

# Calcul de l'AIC
AIC(modele)
