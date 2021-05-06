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

set.seed(100)
training.ids <- createDataPartition(data3$DEATH_EVENT, p = 0.7, list = FALSE)

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