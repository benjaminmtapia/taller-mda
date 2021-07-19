require(e1071)
library(ggplot2)

#analisis estadistico

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

attach(data)

#Dataset
data2 <- data[,c(1,3,5,7,8,9,13)]
data2$DEATH_EVENT <- factor(data2$DEATH_EVENT)

#SVM
formula = DEATH_EVENT ~.
model <- svm(formula, data2)
summary(model)

'''
Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.08333333 
    epsilon:  0.1 
Number of Support Vectors:  250
'''

x<- subset(data2, select =  -DEATH_EVENT)
y <- DEATH_EVENT
model2 <- svm(x,y)
pred <- predict (model,x)
table(pred,y)


plot(cmdscale(dist(data2[,-7])), col = as.integer(data2[,7]), pch = c("o","+")[1:299 %in% model$index + 1])

obj<-tune(svm,DEATH_EVENT~.,data=data2,kernel="radial",ranges=list(cost=2^(-1:4)),tunecontrol=tune.control(sampling="cross",cross=2))
plot(obj)
summary(obj$best.model)

'''

Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.08333333 
    epsilon:  0.1 


Number of Support Vectors:  250
'''

pred<- predict(obj$best.model,x)
table(pred,DEATH_EVENT)


obj<-tune(svm,DEATH_EVENT~.,data=data2,kernel="linear",ranges=list(gamma = 2^(-2:4),cost=2^(-1:4)),tunecontrol=tune.control(sampling="cross",cross=2))
summary(obj)

'
Parameter tuning of ‘svm’:

- sampling method: 2-fold cross validation 

- best parameters:
 gamma cost
  0.25    2

- best performance: 0.1546886 
'

pred<-predict(obj$best.model,x,decision.values = TRUE)
table(pred,DEATH_EVENT)



obj<-tune(svm,DEATH_EVENT~.,data=data2,kernel="radial",ranges=list(cost=2^(-7:14),gamma=2^(-7:14)),tunecontrol = tune.control(sampling = "cross", cross = 2))
summary(obj$best.model)
plot(obj)
'
- sampling method: 2-fold cross validation 

- best parameters:
  gamma cost
 0.0625    1

- best performance: 0.1542959 
'
pred<-predict(obj$best.model,x)
table(pred,DEATH_EVENT)

#variables mas importantes
importantData <- data[,c(1,3,5,7,13)]
obj<-tune(svm,DEATH_EVENT~.,data=data2,kernel="radial",ranges=list(gamma = 2^(-2:4),cost=2^(-1:4)),tunecontrol=tune.control(sampling="cross",cross=2))
summary(obj)
pred<-predict(obj$best.model,x,decision.values = TRUE)
table(pred,DEATH_EVENT)

require(RWeka)


