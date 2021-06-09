library(mclust)
library(ggplot2)
library(corrplot)
library(cluster)
library(factoextra)
library(sm)
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

#Variables numericas: age, ceratinine_phosphokinase, ejection_fraction, platelets, serum_creatinine, serum_sodium
#Variables categoricas: anaemia, diabetes, high_blood_pressure, sex,smoking

#Distribuciones de las caracteristicas en base a las etiquetas
#Variables numericas

death.f <- factor (data$DEATH_EVENT, levels = c(0,1), labels = c("Sobrevive","Muere"))
sm.density.compare(data$age,data$DEATH_EVENT)
title(main="age")
colfill<-c(2:(2+length(levels(death.f))))
legend(locator(1), levels(death.f), fill=colfill)
plot(x = data$DEATH_EVENT, y = data$age)

sm.density.compare(data$serum_creatinine,data$DEATH_EVENT)
title(main="CPK")
colfill<-c(2:(2+length(levels(death.f))))
legend(locator(1), levels(death.f), fill=colfill)
plot(x = data$DEATH_EVENT, y = data$serum_creatinine)

sm.density.compare(data$ejection_fraction,data$DEATH_EVENT)
title(main="ejection_fraction")
colfill<-c(2:(2+length(levels(death.f))))
legend(locator(1), levels(death.f), fill=colfill)
plot(x = data$DEATH_EVENT, y = data$ejection_fraction)

sm.density.compare(data$platelets,data$DEATH_EVENT)
title(main="platelets")
colfill<-c(2:(2+length(levels(death.f))))
legend(locator(1), levels(death.f), fill=colfill)
plot(x = data$DEATH_EVENT, y = data$platelets)

sm.density.compare(data$serum_creatinine,data$DEATH_EVENT)
title(main="serum_creatinine")
colfill<-c(2:(2+length(levels(death.f))))
legend(locator(1), levels(death.f), fill=colfill)
plot(x = data$DEATH_EVENT, y = data$serum_creatinine)

sm.density.compare(data$serum_sodium,data$DEATH_EVENT)
title(main="serum_sodium")
colfill<-c(2:(2+length(levels(death.f))))
legend(locator(1), levels(death.f), fill=colfill)
plot(x = data$DEATH_EVENT, y = data$serum_sodium)

#Pareciera ser que las que entregan mayor informacion corresponden 
#a ejection_fraction y serum_creatinine, como lo indican los autores del paper


#Variables categoricas
dist_anaemia <- with(data,table(DEATH_EVENT,anaemia))
barplot(dist_anaemia, beside = TRUE, legend = TRUE, args.legend=list(title="death_event"), main = "anaemia", ylab = "count", xlab = "anaemia")

dist_diabetes <- with(data,table(DEATH_EVENT,diabetes))
barplot(dist_diabetes, beside = TRUE, legend = TRUE, args.legend=list(title="death_event"), main = "diabetes", ylab = "count", xlab = "diabetes")

dist_highBlood <- with(data,table(DEATH_EVENT,high_blood_pressure))
barplot(dist_highBlood, beside = TRUE, legend = TRUE, args.legend=list(title="death_event"), main = "high_blood_pressure", ylab = "count", xlab = "high_blood_pressure")

dist_sex <- with(data,table(DEATH_EVENT,sex))
barplot(dist_sex , beside = TRUE, legend = TRUE, args.legend=list(title="death_event"), main = "sex", ylab = "count", xlab = "sex")

dist_smoking <- with(data,table(DEATH_EVENT,smoking))
barplot(dist_smoking, beside = TRUE, legend = TRUE, args.legend=list(title="death_event"), main = "smoking", ylab = "count", xlab = "smoking")

#Matriz de correlacion de Pearson
data2 <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
matrix <- cor(data2,method = c("pearson"))
round(matrix,2)

#Se puede identificar que las caracteristicas no estan correlacionadas
# a excepcion de sex y smoking, donde se verifica una correlacion
# ligeramente positiva

#Verificacion de las distribuciones usando Shapiro Wilk
#H0: Las variables numericas siguen una distribucion normal
#H1: Las variables numericas no siguen una distribucion normal

age.shapiro <- shapiro.test(data2$age)
CPK.shapiro <- shapiro.test(data2$creatinine_phosphokinase)
ejection.shapiro <- shapiro.test(data2$ejection_fraction)
platelets.shapiro <- shapiro.test(data2$platelets)
serum_creatinine.shapiro <- shapiro.test(data2$serum_creatinine)
serum_sodium.shapiro <- shapiro.test(data2$serum_sodium)

#Considerando un p valor de 0.05, no existe suficiente evidencia
#estadistica para rechazar h0, por lo que las variables numericas
#no siguen una distribucion normal

#Variable numericas
data3 <- data[,c(1,3,5,7,8,9)]

#BIC
BIC <-mclustBIC(data3, prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC)
summary(BIC)

#Se realiza el mclust en base al mejor BIC
mod=Mclust(data3,x=BIC) 
summary(mod)
plot(mod, what = "classification") 

#considerando el tiempo
data4<- data[,-c(2,6,10,13,11,4)]
BIC2<- mclustBIC(data4)

mod2<- Mclust(df3,G=4,modelNames="VVI")
plot(mod2, what="classification")
summary(mod2)

#dataset completo
BIC3<- mclustBIC(data)
mod3 <- Mclust(data, G=2, modelNames = "VVE")
plot(BIC3,what="classification")
#Comparacion con k medias
nclusters <- fviz_nbclust(data2, pam, method = "wss")+ geom_vline(xintercept = 4, linetype = 2)
matriz.diferencias <- daisy(data2)
cluster <- pam(matriz.diferencias,k=4,diss=TRUE)
clusplot(cluster)