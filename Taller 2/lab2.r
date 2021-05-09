library(caret)
library(randomForest)

#dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv"
data <- read.csv(url,header=FALSE, fill=TRUE)
data <- na.omit(data)
colnames(data) <- c("age","anaemia","creatinine_phosphokinase","diabetes","ejection_fraction",
                    "high_blood_pressure","platelets","serum_creatinine","serum_sodium","sex","smoking",
                    "time","DEATH_EVENT")

#Se deben pasar todas las variables a int
data <- data[-c(1),]
data$age <-as.numeric(data$age)
data$anaemia <-as.numeric(data$anaemia)
data$creatinine_phosphokinase <-as.numeric(data$creatinine_phosphokinase)
data$diabetes <-as.numeric(data$diabetes)
data$ejection_fraction <-as.numeric(data$ejection_fraction)
data$high_blood_pressure <-as.numeric(data$high_blood_pressure)
data$platelets <-as.numeric(data$platelets)
data$platelets <- data$platelets / 1000
data$serum_creatinine <-as.numeric(data$serum_creatinine)
data$serum_sodium <-as.numeric(data$serum_sodium)
data$sex <-as.numeric(data$sex)
data$smoking <-as.numeric(data$smoking)
data$time <-as.numeric(data$time)
data$DEATH_EVENT <-as.numeric(data$DEATH_EVENT)

#Dataset
data3 <- data[,c(1,3,5,7,8,9,13)]
data3$DEATH_EVENT <- factor(data3$DEATH_EVENT)

set.seed(71)
training.ids <- createDataPartition(data3$DEATH_EVENT, p = 0.7, list = FALSE)

#Un tipo de randomForest
mod <- randomForest(x = data3[training.ids,1:6],
                    y = data3[training.ids,7],
                    ntree = 500,
                    keep.forest = TRUE)

pred <- predict(mod, data3[-training.ids,])
table(data3[-training.ids,"DEATH_EVENT"], pred, dnn = c("Actual","Predicho"))

library(ROCR)
probs <- predict(mod, data3[-training.ids,], type = "prob")
pred <- prediction(probs[,2],data3[-training.ids,"DEATH_EVENT"])
perf <- performance(pred,"tpr","fpr")
plot(perf)
a <- performance(pred,measure = "auc")
#auc -> a@y.values

#Realizado en clase

#Otro random Forest
mod2 <- randomForest(DEATH_EVENT ~ ., data = data3, importance = TRUE, proximity = TRUE)

#Importancia
round(importance(mod2),2) 

#Graficar para verificar atributos que aportan mas al proceso de clasificacion
graph <- varImpPlot(mod2)

# Salto a **********

#Proximidad   -> Escalamiento multidimensional
prox <- cmdscale(1 - mod2$proximity, eig = TRUE)  
op <- par(pty ="s")
pairs(cbind(data3[,1:6],prox$points),cex = 0.6, gap = 0,
      col = c("red","green","blue","yellow","cyan","black")[as.numeric(data3$DEATH_EVENT)],
      main="Heart failure clinical records: Predictors and MDS of Proximity Based on RandomForest")
par(op)

#Valores propios asociados de manera creciente
print(prox$GOF) #Son bajos, no s por que
indiv <- MDSplot(mod2,data3$DEATH_EVENT)


#Se pueden quitar variables que entregan menor informacion para verificar el error y el rendimiento  **********

importantData <- data[,c(1,3,5,13)]
importantData$DEATH_EVENT <- factor(importantData$DEATH_EVENT)

#Se crea un nuevo randomForest

mod3 <- randomForest(DEATH_EVENT ~ ., data = importantData, ntree = 500, importance = TRUE, proximity = TRUE) #Pareciera ser que el error aumenta
print(mod3)
#Se obtiene el grafico
plot(mod3)

#MASS #REVISAR
require(MASS)
parcoord(data3[,1:6],var.label = TRUE,col = c("red","green","blue")[as.numeric(data3$DEATH_EVENT)])
legend("bottomright",legend = c("DEATH_EVENT"),fill = 1:4)

#mtry = 2 / recomendacion
mod4 <- randomForest(DEATH_EVENT ~ ., data = importantData, ntree = 500, mtry = 2, importance = TRUE, proximity = TRUE) 
print(mod4) # Se obtiene un 28,76% de OOB

#mtry = 3 / testeo
mod5 <- randomForest(DEATH_EVENT ~ ., data = importantData, ntree = 500, mtry = 3, importance = TRUE, proximity = TRUE) 
print(mod5) # Se obtiene un 27,42% de OOB -> disminuye

#mtry = 4 / testeo
mod6 <- randomForest(DEATH_EVENT ~ ., data = importantData, ntree = 500, mtry = 5, importance = TRUE, proximity = TRUE) 
print(mod6) # Se obtiene un 27,76% de OOB