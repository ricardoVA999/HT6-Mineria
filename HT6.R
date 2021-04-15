setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT6-Mineria")
library(caret)
library(rpart)
library(e1071)
library(rpart.plot)
library(dummies)

houses = read.csv('train.csv')
houses[is.na(houses)]<-0
houses$Id<-NULL


houses$clasification <- ifelse(houses$SalePrice > 290000, "Caras", ifelse(houses$SalePrice>170000, "Intermedia", "Economicas"))
houses<- houses[,c(4,12,17,34,38,46,62,67,81)]
houses<-cbind(houses,dummy(houses$clasification,verbose = T))

porciento <- 70/100
set.seed(1234)

economicas<-houses[houses$clasification=="Economicas",]
intermedias<-houses[houses$clasification=="Intermedia",]
caras<-houses[houses$clasification=="Caras",]


numFilasTrainEcon<-sample(nrow(economicas), porciento*nrow(economicas))
trainEcon<-economicas[numFilasTrainEcon,]

numFilasTrainInter<-sample(nrow(intermedias), porciento*nrow(intermedias))
trainInter<-intermedias[numFilasTrainInter,]

numFilasTrainCaras<-sample(nrow(caras), porciento*nrow(caras))
trainCaras<-caras[numFilasTrainCaras,]


training<-rbind(trainInter, trainEcon, trainCaras)
test<-houses[setdiff(rownames(houses),rownames(training)),]

table(training$clasification)
table(test$clasification)

#Modelo para las casas caras
modeloCaras<-glm(housesCaras~., data = training[,c(1:8,10)],family = binomial(), maxit=100)
predCaras<-predict(modeloCaras,newdata = test[,1:8], type = "response")
prediccionCaras<-ifelse(predCaras>=0.5,1,0)
confusionMatrix(as.factor(test$housesCaras),as.factor(prediccionCaras))

#Modelo para las casas Intermedias
modeloInter<-glm(housesIntermedia~., data = training[,c(1:8,12)],family = binomial(), maxit=100)
predInter<-predict(modeloInter,newdata = test[,1:8], type = "response")
prediccionInter<-ifelse(predInter>=0.5,1,0)
confusionMatrix(as.factor(test$housesIntermedia),as.factor(prediccionInter))

#Modelo para las casas economicas
modeloEcon<-glm(housesEconomicas~., data = training[,c(1:8,11)],family = binomial(), maxit=100)
predEcon<-predict(modeloEcon,newdata = test[,1:8], type = "response")
prediccionEcon<-ifelse(predEcon>=0.5,1,0)
confusionMatrix(as.factor(test$housesEconomicas),as.factor(prediccionEcon))