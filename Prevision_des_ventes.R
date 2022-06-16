##########################################################################
# Ce fichier loads monthly retail sales per day.
# ces donnees s'etaient regle pour l'inflation.
# on va utiliser ces donnees pour previsionner les deux annees prochaines
# de ventes par jour.
# Cree par : Ayyoub Bouras 19/03/2022
#########################################################################
# Clear all variables in workspace
rm(list=ls())
# Importer le fichier csv de Prevision
library(fpp2)
#load the data
data<- read.csv("/home/ubuntu/Desktop/master ENSIAS ETUDE/Homework/Proba/MINI PROJET/forcast.csv")
# declare this is time series data
Y<- ts(data[,2],start=c(1992,1),frequency=12)
#########################################################################
#Priminalary Analysis
#########################################################################
# Time Plot
autoplot(Y) + ggtitle("Tracé du Temps : Commerce de détail réelles aux États-Unis par jour")+
  ylab("Des Millions de Dollards de 2022")

#On remarque une tendance dans les donnees . Investigation:
#On prend La premiere derivée des données Y pour enlever la tendance
DY<-diff(Y)
# Changement in Time Plot 
autoplot(DY) + ggtitle("Tracé du Temps : Le Changement de la Commerce de détail réelles aux États-Unis par jour")+
  ylab("Des Millions de Dollards de 2022")

#Les series Apparaient stationnaire par rappornt aux tendances . 
#En invistiguant maintenant les saisons de changement:
ggseasonplot(DY)+
  ggtitle("Tracé par Saison :  Le Changement de la Commerce de détail aux États-Unis par jour")+
  ylab("Des Millions de Dollards de 2022")
#un autre Tracé par saison :
ggsubseriesplot(DY)+ggtitle("Tracé par Saison : les differences dans les mois pour chaque année")

#####################################################################################
# Notre series Y a des proprietes (saisonalité,tendance)
#Pour Enlever la tendance on a fait la premiere dérivé 
#maintenant on va enlever la saisonalité
# On va faire plusieurs Méthodes pour Appliquer la Prévision
#####################################################################################

################
# 1- Benchmark Méthode pour la Prévision (seasonal naive method):
###############
fit<-snaive(DY) #Residual sd: 457.8827 ca doit etre prés de 0 donc on va chercher un autre model plus optimal
print(summary(fit))
checkresiduals(fit) # Les données qui ne sont pas bien integrées ou utilisées
################
#2-Fit ETS method (peut utilisé data avec sa tendance sans probléme)
###############
fit_ets<-ets(Y) ##Residual sd: 0.0231
print(summary(fit_ets))
checkresiduals(fit_ets)

################
#2-Fit on ARIMA model 
###############
fit_arima<- auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace=TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

##########################
# on va utiliser ETS method pour la Prevision car il a le plus petit sd
##########################
Prevision<- forecast(fit_ets,h=24)
autoplot(Prevision,include=180)
print(summary(Prevision))



