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


#SVM
formula = DEATH_EVENT ~.
model <- svm(formula, data)
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

x<- subset(data, select =  -DEATH_EVENT)
y <- DEATH_EVENT
model2 <- svm(x,y)
pred <- predict (model,x)
table(pred,y)


plot(cmdscale(dist(data[,-13])), col = as.integer(data[,13]), pch = c("o","+")[1:150 %in% model$index + 1])

obj<-tune(svm,DEATH_EVENT~.,data=data,kernel="radial",ranges=list(cost=2^(-1:4)),tunecontrol=tune.control(sampling="cross",cross=2))
plot(obj)
summary(obj$best.model)

'''

Call:
best.tune(method = svm, train.x = DEATH_EVENT ~ ., data = data, ranges = list(cost = 2^(-1:4)), tunecontrol = tune.control(sampling = "cross", 
    cross = 2), kernel = "radial")


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


obj<-tune(svm,DEATH_EVENT~.,data=data,kernel="linear",ranges=list(gamma=2^(-2:4),cost=2^(-1:4),tunecontrol=tune.control(sampling="cross",cross=2)))
summary(obj)


pred<-predict(obj$best.model,x)
table(pred,DEATH_EVENT)



obj<-tune(svm,DEATH_EVENT~.,data=data,kernel="radial",ranges=list(gamma=2^(-7:12),cost=2^(-7:14),tunecontrol=tune.control(sampling="cross",cross=2)))
pred<-predict(obj$best.model,x)
table(pred,DEATH_EVENT)