#install.packages("Amelia")
rm(list=ls()) # to clear
graphics.off() # to close figure


#-----------------------------------------------------------
#!         loading package into the R session              !
#-----------------------------------------------------------
library(xts)
library(readxl)
library(astsa)
library(urca)
library(ggfortify)
library(forecast)
library(fpp2)
library(gets)


#https://agailloty.github.io/blog/econom?trie/modelisation-garch

x<-as.data.frame(read_excel("sp500.xls"))
head(x)
str(x)
sum(is.na(x))
nom_var<-c("Date","SP500")
colnames(x)<-nom_var



#XTS
library(xts)
x = as.xts(x[,-1], order.by = as.Date(x[,1]))
head(x)
dim(x)


# =========================================================================================================
# PARTIE 1 - Analyse exploratoire des donnees 
# STATISTIQUES DESCRIPTIVES ET TRANSFORMATION 
# =========================================================================================================

par(mfrow = c(1,1))
plot(x)
plot(log(x))
n = length(x)

#Rentabilite continue
log_ret <- diff(log(x)) 
head(log_ret)


#-Rentabilite
n = length(x) #Nb rows
simple_ret <- diff(x)/x[1:(n-1)]
head(simple_ret)
dim(simple_ret)
simple_ret = na.omit(simple_ret)
dim(simple_ret)

#-Rentabilite totale-
total_log_ret <- sum(log_ret,na.rm = T) #somme pour log-returns
total_log_ret

total_Simple_ret <- prod(1+simple_ret,na.rm = T)-1 # produit simple returns
total_Simple_ret


#----Calcul de la rentabilite continue---
returns =  diff(log(x))
head(returns)
#Graphique de la rentabilite :
plot(returns)

simple_ret_2 = simple_ret * 100
par(mfrow = c(1,1))
plot(simple_ret_2)

# GRAPHIQUE RETURN 
plot(simple_ret_2,main = " La rentabilite simple en %")

#Quand on estime la volatiliten on travaille avec les return 
library(quantmod)
x2 = x
x2 = round(x2,2)
chartSeries(round(x,2), main = " Series ")
chartSeries(x2,TA=c(addVo(),addBBands()))



daily_return_x <- dailyReturn(x)

head(daily_return_x)
weekly_return_x <- weeklyReturn(x)
head(weekly_return_x)

#--- Skewness, Kurtosis, Moment-----

#Chargement des librairies PerformanceAnalytics , xts et tsbox :
#install.packages("PerformanceAnalytics")
#install.packages("xts")
#install.packages("tsbox")
library(PerformanceAnalytics) 
library(xts)
library(tsbox) #to convert to xts..

#On peut egalement calculer les return a l'aide des library suivantes 

#Calcul des rentabilite via le package PerformanceAnalytics et sa fonction Return.calculate :
#Rentabilite continue 
ret_xts = Return.calculate(x, method =  "log")
head(ret_xts)
head(simple_ret)
#Rentabilite   
ret_xts = Return.calculate(x, method =  "difference")
head(ret_xts) 

#Graphique des rentabilite:
plot(ret_xts)


#Graphique de X :
X_Ret <- log_ret
class(X_Ret)[1] #"ts"
plot(X_Ret)


#Distribution des rentabilite:
n <- 500
zs <- seq(-4, 4, 0.01)
normal_density <- dnorm(zs)
m = mean(X_Ret,na.rm = T)
s=  sd(X_Ret,na.rm = T)
ReturnsNormal  = (X_Ret - m)/ s
myhist <- hist(ReturnsNormal,100, prob=TRUE, xlim=c(-6,6))
lines(zs, normal_density, type = "l", col = "red", lwd = 4)


#Distribution des rentabilite via la fonction chart.Histogram du package PerformanceAnalytics
X_Ret_xts =ts_xts(X_Ret) # cette ligne converti les rentabilite en format xts (format requis pour PerformanceAnalytics)
chart.Histogram(X_Ret_xts)

#QQPlot :
qqnorm(normal_density); qqline(normal_density)


#Fonction  (marche ap)
moment <- function(x, order = 1, center = FALSE,sample = FALSE ) {
  if (center)
    x <- x - mean(x,na.rm=T)
  
  s <- 0
  if(sample)
    s = 1
  sum(x ^ order) / (length(x)-s)
}


moment(X_Ret,order=1)


mean(X_Ret)

variance =  moment(X_Ret,center=TRUE,order=2,sample= TRUE)
EcartType = sqrt(variance)
EcartType

sd(X_Ret)


#Skewness
skewness(X_Ret,method="moment") #PerformanceAnalytics

#Kurtosis
kurtosis(X_Ret,method="moment")  #PerformanceAnalytics


# =========================================================================================================
# PARTIE 3 - MODELISATION Garch 
# =========================================================================================================
#mcleod li http://forums.cirad.fr/logiciel-r/viewtopic.php?t=5054

#garch http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/


#Introduction 

#ARCH : AutoRegressive Conditional Heteroskedacity et le GARCH pour Generalised ARCH. 
#Il s'agit d'un type de modele qui permet d'estimer et prevoir la volatilite du prix d'un action ou 
#d'un rendement a court terme en se basant sur les valeurs que prennent ces prix ou rendements quelques periodes 
#de temps auparavant.
#Le terme AutoRegressive signifie qu'on regresse le modele a partir de lui-meme, c'est-a-dire que les 
#variables exogenes sont les retards de la variable endoene a l'ordre q. Le terme Heteroskedacity accentue 
#le fait que la variance n'est constante dans le temps. 

#Nous disons qu'il y a heteroscedasticite si dans la serie temporelle il y a une ou des sous-periodes dont la variance 
#est differente de la variance des autres periodes. Dans les series financieres, souvent la variation de la variance 
#est souvent causee par un evennement particulier qui peut surgir sur le marche, donc si notre variance est
#heteroscedastique,elle l'est conditionnellement aux interactions du marche d'ou le terme conditional hetereoskedacity.

#La grande nouveaute qu'apporte les modeles de types ARCH est que non seulement il prend en compte la
#valeur de la variable a N-p periodes mais aussi le changement dans la valeur de la variance a N-p, 
#ce faisant il ameliore grandement la prevision de la volatilite.

#Les modeles GARCH suivent le meme principe que le modele ARCH mais ajoute un second membre a l'equation
#qui est la moyenne mobile d'ordre q

# Packages utilises  

#Il existe un certain nombre de Package qui peuvent nous permettre d'estimer les modeles 
#de volatilite. Les Packages que nous utiliserons sont le "rugarch" pour les modeles GARCH
#univaries et le "rmgarch" (pour les modeles multivaries), tous deux ecrits par Alexis 
#Ghalanos. Nous utiliserons egalement le package "quantmod", qui nous permettra d'utiliser
#certains fonctionnalite pour des donnees financieres standard 


install.packages(c("quantmod","rugarch","rmgarch"))
library(quantmod)
library(rugarch)
library(rmgarch)


# Modele GARCH Univarie

# Ici nous allons utilise fonctionnalite offerte par le package "rugarch" ecrit par Alexios Galanos
#Nous n'allons utiliser que le GARCH univarie car nous n'avons pas d'exogenes a proprement
#parler a inclure dans le modele.
#Pour un GARCH multivarie il faudra lire les donnees de plusieurs entreprises en meme temps

#Specification du modele 

# La premiere chose a faire est de savoir quel type de modele GARCH 
#on veut estimer. C'est la fonction "ugarchspec()" qui est utilisee pour informer R
#du type de modele. Il existe en fait une specification 
#par defaut et la maniere de l'invoquer est la suivante

ug_spec = ugarchspec()
ug_spec

#'ug_spec' est maintenant une liste qui contient toutes les specifications du model 
#Regardons les : 
#
#GARCH Model		: sGARCH(1,1)
# le s ici c'est pour standardGARCH 
#FI  : factionnal integration 
# distribution : normal 
#Ce sont les configurations par defaut. sGARCH signifie standard GARCH c'est le GARCH simple 
#car il existe une grande variete de modeles GARCH. 

#Les questions cles ici sont les specifications du "Mean Model" (ici un modele ARMA(1,1)) 
#et les specifications du "GARCH Model", ici un "sGarch(1,1)" qui est en fait un GARCH (1,1).
#Pour obtenir des details sur toutes les specifications possibles et sur la maniere de les 
#modifier, il est preferable de consulter le documentation
#("https : cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_packages.pdf")

#Disons qu'on veuilles changer le "mean model" d'un ARMA(1,1) en un ARMA(1,0), c'est-a-dire un modele AR(1).

ug_spec <- ugarchspec(mean.model = list(armaOrder = c(1,1)))
ug_spec

#On peut appeler "ug_spec" a nouveau pour verifier que la specification du modele a bien change.

#Voici la specification d'un exemple de modele EWMA ("Exponentially Weighted Moving Average") 

#ewma_spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
#                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
#                       distribution.model = "norm", fixed.pars = list(omega = 0 ))

#Estimation du modele 


#Maintenant que nous avons specifie un modele a estimer, nous devons trouver les meilleurs 
#parametres, c'est-a-dire que nous devons estimer le modele. Cette etape est realisee par
#la fonction "ugarchfit".

ugfit = ugarchfit(spec = ug_spec, data = simple_ret)

#La liste "ugfit" contient desormais une serie de resultats de l'estimation. 
#Regardons les resultats 

ugfit

# On arrive a reconnaitre certains parametres du modele de GARCH
#"ar1" est le coefficient AR1 du modele moyen (ici tres petit et fondamentalement non 
#significatif), "alpha1" est le coefficient du carre des residus dans l'equation GARCH 
#et beta1" est le coefficient de la variance decalee (lagged variance)

#Souvent, on voudra utiliser les resultats du modele pour une analyse plus approfondie. 
#Il est donc important d'expliquer comment extraire des informations telles que les 
#estimations des parametres, leurs erreurs-types ou les residus.

#L'object 'ugfit' contient toutes les informations.  Dans cet objet, vous pouvez trouver deux tiroirs
#(drawers) (@fit et @model).Chacun des tiroirs contient une serie de choses
# On peut regarder ce qu'elle contiennent en regardant par exemple : 

paste("Elements dans @model slot")
names(ugfit@model)
paste("Objet contenus dans @fit slot")
names(ugfit@fit)


#Les coefficents d'estimations 
ugfit@fit$matcoef

# Si on veut extraire les coefficients estimees on fait : 

ugfit@fit$coef
ug_var  <- ugfit@fit$var #Enregistre la variance conditionnelle estimee
ug_res2 <- (ugfit@fit$residuals)^2 # Enregistre les residus estimee 

plot(ug_res2, type = "l")
lines(ug_var, col = "green")

 # Ici on a trace le carree residuel et on a ajouter la variance conditionnelle estimee. 
# La ligne noir c'est le carree des residus estimee. 
# On peut voir qu'on a une periode de large volatilite.

options(repr.plot.res = 300, repr.plot.height = 3) # parametres graphiques, res pour resolution
resid <- ugfit@fit$residuals

# marche plus ici
plot.xts(resid, main = "Residus du modele", col = "lightblue")
resid2 <- xts((ugfit@fit$residuals)^2, order.by = as.Date(index(x)))
plot.xts(resid2, main = "Carre des residus du modele", col = "lightblue")



# Modele de prevision 

#Souvent, on veut utiliser un modele estime pour prevoir ulterieurement la variance conditionnelle. 
#La fonction utilisee a cette fin est la fonction "ugarchforecast". L'application est assez simple :

ugfore <- ugarchforecast(ugfit, n.ahead = 20)
ugfore

ugfore1 <- ugarchforecast(ugfit, n.ahead = 1)
ugfore1



#Comme vous pouvez le constater, ont a etabli des previsions pour les 10 prochains jours, tant pour les rendements attendus 
#('Series') et pour la volatilite conditionnelle (racine carree de la variance conditionnelle : square root of the conditional variance ). 

#Comme pour l'objet cree pour l'ajustement des modeles (model fitting) "ugfore" contient deux 
#emplacements (@model) et (@forecast) et on peut utiliser "names(ugfore@forecast)" pour savoir 
#sous quels noms les elements sont enregistres. Par exemple on peut extraire la volatilite conditionnelle prevue comme suit : 

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l")

# A noter que la volatilite est la racine carree de la variance conditionnelle. 
#  Pour mettre ces previsions en contexte, affichons-les avec les 50 dernieres observations
#utilisees dans l'estimation 

par(mfrow = c(1,1))

ug_var_t  <- c(tail(ug_var,20 ), rep(NA,20)) # gets the last 20 observations
ug_res2_t <- c(tail(ug_res2,20), rep(NA,20)) # gets the last 20 observations 
ug_f <- c(rep(NA,20), (ug_f)^2)

plot(ug_res2_t,xlim = c(0, 40), ylim = c(0, 0.0010), type = "l")
lines(ug_f, col = "orange")
lines(ug_var_t, col = "green ")


#On peut voir comment la prevision de la variance conditionnelle se rapproche de la derniere estimation de la 
#variance conditionnelle. En fait, elle a augmente a partir de la, lentement, vers la valeur de la variance inconditionnelle. 

#  Le package 'rugarch' comporte de nombreuses fonctionnalites supplementaires qu'on peut explorer grace a la documentation.



###### EGARCH 


ug_spec = ugarchspec()
ug_spec

#'ug_spec' est maintenant une liste qui contient toutes les specifications du model 
#Regardons les : 
#
#GARCH Model		: sGARCH(1,1)
# le s ici c'est pour standardGARCH 
#FI  : factionnal integration 
# distribution : normal 
#Ce sont les configurations par defaut. sGARCH signifie standard GARCH c'est le GARCH simple 
#car il existe une grande variete de modeles GARCH. 

#Les questions cles ici sont les specifications du "Mean Model" (ici un modele ARMA(1,1)) 
#et les specifications du "GARCH Model", ici un "sGarch(1,1)" qui est en fait un GARCH (1,1).
#Pour obtenir des details sur toutes les specifications possibles et sur la maniere de les 
#modifier, il est preferable de consulter le documentation
#("https : cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_packages.pdf")

#Disons qu'on veuilles changer le "mean model" d'un ARMA(1,1) en un ARMA(1,0), c'est-a-dire un modele AR(1).

ug_spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "norm", fixed.pars = list(omega = 0 ))

ug_spec


#On peut appeler "ug_spec" a nouveau pour verifier que la specification du modele a bien change.

#Voici la specification d'un exemple de modele EWMA ("Exponentially Weighted Moving Average") 

#ewma_spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
#                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
#                       distribution.model = "norm", fixed.pars = list(omega = 0 ))

#Estimation du modele 


#Maintenant que nous avons specifie un modele a estimer, nous devons trouver les meilleurs 
#parametres, c'est-a-dire que nous devons estimer le modele. Cette etape est realisee par
#la fonction "ugarchfit".

ugfit = ugarchfit(spec = ug_spec, data = simple_ret)

#La liste "ugfit" contient desormais une serie de resultats de l'estimation. 
#Regardons les resultats 

ugfit

# On arrive a reconnaitre certains parametres du modele de GARCH
#"ar1" est le coefficient AR1 du modele moyen (ici tres petit et fondamentalement non 
#significatif), "alpha1" est le coefficient du carre des residus dans l'equation GARCH 
#et beta1" est le coefficient de la variance decalee (lagged variance)

#Souvent, on voudra utiliser les resultats du modele pour une analyse plus approfondie. 
#Il est donc important d'expliquer comment extraire des informations telles que les 
#estimations des parametres, leurs erreurs-types ou les residus.

#L'object 'ugfit' contient toutes les informations.  Dans cet objet, vous pouvez trouver deux tiroirs
#(drawers) (@fit et @model).Chacun des tiroirs contient une serie de choses
# On peut regarder ce qu'elle contiennent en regardant par exemple : 

paste("Elements dans @model slot")
names(ugfit@model)
paste("Objet contenus dans @fit slot")
names(ugfit@fit)


#Les coefficents d'estimations 
ugfit@fit$matcoef

# Si on veut extraire les coefficients estimees on fait : 

ugfit@fit$coef
ug_var  <- ugfit@fit$var #Enregistre la variance conditionnelle estimee
ug_res2 <- (ugfit@fit$residuals)^2 # Enregistre les residus estimee 

plot(ug_res2, type = "l")
lines(ug_var, col = "green")

# Ici on a trace le carree residuel et on a ajouter la variance conditionnelle estimee. 
# La ligne noir c'est le carree des residus estimee. 
# On peut voir qu'on a une periode de large volatilite.





# Modele de prevision 

#Souvent, on veut utiliser un modele estime pour prevoir ulterieurement la variance conditionnelle. 
#La fonction utilisee a cette fin est la fonction "ugarchforecast". L'application est assez simple :

ugfore <- ugarchforecast(ugfit, n.ahead = 20)
ugfore

#Comme vous pouvez le constater, ont a etabli des previsions pour les 10 prochains jours, tant pour les rendements attendus 
#('Series') et pour la volatilite conditionnelle (racine carree de la variance conditionnelle : square root of the conditional variance ). 

#Comme pour l'objet cree pour l'ajustement des modeles (model fitting) "ugfore" contient deux 
#emplacements (@model) et (@forecast) et on peut utiliser "names(ugfore@forecast)" pour savoir 
#sous quels noms les elements sont enregistres. Par exemple on peut extraire la volatilite conditionnelle prevue comme suit : 

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l")

# A noter que la volatilite est la racine carree de la variance conditionnelle. 
#  Pour mettre ces previsions en contexte, affichons-les avec les 50 dernieres observations
#utilisees dans l'estimation 

par(mfrow = c(1,1))

ug_var_t  <- c(tail(ug_var,20 ), rep(NA,10)) # gets the last 20 observations
ug_res2_t <- c(tail(ug_res2,20), rep(NA,10)) # gets the last 20 observations 
ug_f <- c(rep(NA,20), (ug_f)^2)

plot(ug_res2_t, type = "l")
lines(ug_f, col = "orange")
lines(ug_var_t, col = "green ")


#On peut voir comment la prevision de la variance conditionnelle se rapproche de la derniere estimation de la 
#variance conditionnelle. En fait, elle a augmente a partir de la, lentement, vers la valeur de la variance inconditionnelle. 

#  Le package 'rugarch' comporte de nombreuses fonctionnalites supplementaires qu'on peut explorer grace a la documentation.


###### #########################################   TGARCH 


ug_spec = ugarchspec()
ug_spec

#'ug_spec' est maintenant une liste qui contient toutes les specifications du model 
#Regardons les : 
#
#GARCH Model		: sGARCH(1,1)
# le s ici c'est pour standardGARCH 
#FI  : factionnal integration 
# distribution : normal 
#Ce sont les configurations par defaut. sGARCH signifie standard GARCH c'est le GARCH simple 
#car il existe une grande variete de modeles GARCH. 

#Les questions cles ici sont les specifications du "Mean Model" (ici un modele ARMA(1,1)) 
#et les specifications du "GARCH Model", ici un "sGarch(1,1)" qui est en fait un GARCH (1,1).
#Pour obtenir des details sur toutes les specifications possibles et sur la maniere de les 
#modifier, il est preferable de consulter le documentation
#("https : cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_packages.pdf")

#Disons qu'on veuilles changer le "mean model" d'un ARMA(1,1) en un ARMA(1,0), c'est-a-dire un modele AR(1).

ug_spec = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "TGARCH"),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "norm", fixed.pars = list(omega = 0 ))


ug_spec
#On peut appeler "ug_spec" a nouveau pour verifier que la specification du modele a bien change.

#Voici la specification d'un exemple de modele EWMA ("Exponentially Weighted Moving Average") 

#ewma_spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
#                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
#                       distribution.model = "norm", fixed.pars = list(omega = 0 ))

#Estimation du modele 


#Maintenant que nous avons specifie un modele a estimer, nous devons trouver les meilleurs 
#parametres, c'est-a-dire que nous devons estimer le modele. Cette etape est realisee par
#la fonction "ugarchfit".

ugfit = ugarchfit(spec = ug_spec, data = simple_ret)

#La liste "ugfit" contient desormais une serie de resultats de l'estimation. 
#Regardons les resultats 

ugfit

# On arrive a reconnaitre certains parametres du modele de GARCH
#"ar1" est le coefficient AR1 du modele moyen (ici tres petit et fondamentalement non 
#significatif), "alpha1" est le coefficient du carre des residus dans l'equation GARCH 
#et beta1" est le coefficient de la variance decalee (lagged variance)

#Souvent, on voudra utiliser les resultats du modele pour une analyse plus approfondie. 
#Il est donc important d'expliquer comment extraire des informations telles que les 
#estimations des parametres, leurs erreurs-types ou les residus.

#L'object 'ugfit' contient toutes les informations.  Dans cet objet, vous pouvez trouver deux tiroirs
#(drawers) (@fit et @model).Chacun des tiroirs contient une serie de choses
# On peut regarder ce qu'elle contiennent en regardant par exemple : 

paste("Elements dans @model slot")
names(ugfit@model)
paste("Objet contenus dans @fit slot")
names(ugfit@fit)


#Les coefficents d'estimations 
ugfit@fit$matcoef

# Si on veut extraire les coefficients estimees on fait : 

ugfit@fit$coef
ug_var  <- ugfit@fit$var #Enregistre la variance conditionnelle estimee
ug_res2 <- (ugfit@fit$residuals)^2 # Enregistre les residus estimee 

plot(ug_res2, type = "l")
lines(ug_var, col = "green")

# Ici on a trace le carree residuel et on a ajouter la variance conditionnelle estimee. 
# La ligne noir c'est le carree des residus estimee. 
# On peut voir qu'on a une periode de large volatilite.





# Modele de prevision 

#Souvent, on veut utiliser un modele estime pour prevoir ulterieurement la variance conditionnelle. 
#La fonction utilisee a cette fin est la fonction "ugarchforecast". L'application est assez simple :

ugfore <- ugarchforecast(ugfit, n.ahead = 20)
ugfore

#Comme vous pouvez le constater, ont a etabli des previsions pour les 10 prochains jours, tant pour les rendements attendus 
#('Series') et pour la volatilite conditionnelle (racine carree de la variance conditionnelle : square root of the conditional variance ). 

#Comme pour l'objet cree pour l'ajustement des modeles (model fitting) "ugfore" contient deux 
#emplacements (@model) et (@forecast) et on peut utiliser "names(ugfore@forecast)" pour savoir 
#sous quels noms les elements sont enregistres. 
#Par exemple on peut extraire la volatilite conditionnelle prevue comme suit : 

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l")
plot(ugfore@forecast$seriesFor,col="blue")

# A noter que la volatilite est la racine carree de la variance conditionnelle. 
#  Pour mettre ces previsions en contexte, affichons-les avec les 50 dernieres observations
#utilisees dans l'estimation 

par(mfrow = c(1,1))

ug_var_t  <- c(tail(ug_var,20 ), rep(NA,10)) # gets the last 20 observations
ug_res2_t <- c(tail(ug_res2,20), rep(NA,10)) # gets the last 20 observations 
ug_f <- c(rep(NA,20), (ug_f)^2)

plot(ug_res2_t, type = "l")
lines(ug_f, col = "orange")
lines(ug_var_t, col = "green ")


#On peut voir comment la prevision de la variance conditionnelle se rapproche de la derniere estimation de la 
#variance conditionnelle. En fait, elle a augmente a partir de la, lentement, vers la valeur de la variance inconditionnelle. 

#  Le package 'rugarch' comporte de nombreuses fonctionnalites supplementaires qu'on peut explorer grace a la documentation.





