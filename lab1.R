library(mclust)
library(ggplot2)
library(corrplot)
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
data$serum_creatinine <-as.numeric(data$serum_creatinine)
data$serum_sodium <-as.numeric(data$serum_sodium)
data$sex <-as.numeric(data$sex)
data$smoking <-as.numeric(data$smoking)
data$time <-as.numeric(data$time)
data$DEATH_EVENT <-as.numeric(data$DEATH_EVENT)


#Se intentan graficar
pDeath<-  plot.barchart(df_death,df_death$Var1,df_death$Freq,"Estado","Frecuencia","Estado del paciente")


#Edad y muerte
contingency.ageDeath <- table(data$age,data$DEATH_EVENT)
contingency.smokingDeath <- table(data$smoking,data$DEATH_EVENT)
contingency.diabetesDeath <- table(data$diabetes,data$DEATH_EVENT)
contingency.ejectionDeath <- table(data$ejection_fraction,data$DEATH_EVENT)
contingency.creatinineDeath <- table(data$serum_creatinine,data$DEATH_EVENT)

#graficos
plot.barchart<-function(
  data,
  x,
  y,
  xlabel,
  ylabel,
  title
){
  ggplot(data, aes(x=x, y=y, fill=x,label=y))+ 
    scale_fill_brewer(palette="Set3") + 
    ggtitle(title)+
    labs(x=xlabel,y=ylabel)+
    geom_bar(stat="identity")+
    theme(panel.border = element_blank(),panel.background= element_blank())
}


#Pruebas de independencia chi cuadrado
smoking.chi <- chisq.test(contingency.smokingDeath,simulate.p.value=FALSE)
diabetes.chi <- chisq.test(contingency.diabetesDeath,simulate.p.value=FALSE)
ejection.chi <- chisq.test(contingency.ejectionDeath,simulate.p.value=TRUE)
creatinine.chi <- chisq.test(contingency.creatinineDeath,simulate.p.value=TRUE)


#sin variables demograficas
df2 <- data[,c(1,10,12)]
#se quitan variables binarias y demograficas: alta presion, diabetes, anemia, edad, sexo y fumar
library(corrplot)
df3<- data[,-c(2,6,10,13,11,4)]

#Distribucion normal

creatinine_shapiro<- shapiro.test(df3$creatinine_phosphokinase)
#p valor < 2.2 e -16, por lo tanto no hay dist normal

ej_shapiro<- shapiro.test(df3$ejection_fraction)
#p valor = 7.216 e-09, no hay dist normal
plateletes_shapiro<- shapiro.test(df3$platelets)
#p-value = 2.883e-12, no hay distribucion normal

sc_shapiro<- shapiro.test(df3$serum_creatinine)
#p-value < 2.2e-16

ss_shapiro<- shapiro.test(df3$serum_sodium)
#p-value = 9.215e-10, no hay dist normal

time_shapiro<- shapiro.test(df3$time)
#p-value = 6.285e-09, no hay distribucion normal

#Por ende, ninguna de las variables seleccionadas tiene distribucion normal


#Matriz de Correlacion

#quitando las variables binarias tampoco se ve una relacion fuerte y directa
cor1<- cor(df3, method="pearson")
corrplot(cor1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
cor2<-cor(data,method="pearson")
corrplot(cor2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#Respecto al dataset completo no hay ninguna relación directa considerablemente fuerte
#No se puede eliminar ninguna variable por correlacion


#componentes principales
mainComp <- princomp(df3[,-1],cor=TRUE)

#Standard deviations:
#   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5 
#   1.1687022 1.0263117 0.9758482 0.9423384 0.8605453 

mainComp2 <- princomp(data[,-1],cor=TRUE)
#   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7    Comp.8    Comp.9   Comp.10 
# 1.3705926 1.2897875 1.1161642 1.0770952 1.0129771 0.9902277 0.9497127 0.9092745 0.8480106 0.8340641 
#  Comp.11   Comp.12 
# 0.7173861 0.6221990 

#Importance of components:
#                       Comp.1    Comp.2    Comp.3    Comp.4    Comp.5
#Standard deviation     1.168702 1.0263117 0.9758482 0.9423384 0.8605453
#Proportion of Variance 0.273173 0.2106631 0.1904559 0.1776003 0.1481076
#Cumulative Proportion  0.273173 0.48383w61 0.6742920 0.8518924 1.0000000

m1 <- mclustBIC(df3)
plot(m1)

m2<- Mclust(df3,G=7,modelNames="VVI")
plot(m2, what="classification")