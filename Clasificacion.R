# Relevant Information:
# This data set includes descriptions of hypothetical samples
# corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota Family.
# Attribute Information: (classes: edible=e, poisonous=p)

rm(list=ls())

setwd("")

library(RWeka)

hongos<-read.csv("mushrooms.csv",stringsAsFactors = TRUE)
View(hongos)
str(hongos)
hongos$veil_type<-NULL


table(hongos$type) # 4208 comestibles, 3916 venenosos

CambioTipo<-function(key){
  switch(as.character(key),
         'p'='venenoso',
         'e'='comible')
}

CambioOlor<-function(key){
  switch(as.character(key),
         'a'='almendra',
         'l'='anis',
         'c'='creosota',
         'y'='pescado',
         'f'='fetido',
         'm'='mohoso',
         'n'='ninguno',
         'p'='acre',
         's'='picante')
}

hongos$type<-sapply(hongos$type,CambioTipo)
hongos$odor<-sapply(hongos$odor,CambioOlor)

hongos$type<-as.factor(hongos$type)
hongos$odor<-as.factor(hongos$odor)

# Emplear el modelo 1R para predecir el comportamiento
hongos_1R<-OneR(type~.,data=hongos)
hongos_1R
summary(hongos_1R)
sapply(hongos_1R, class)

# Mejora el desempeño del modelo
# JRip() modelo basado en Java implementado en Ripper
hongos_JRip<-JRip(type~.,data=hongos)
hongos_JRip
summary(hongos_JRip)
