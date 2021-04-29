library(mclust)
library(ggplot2)
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


#Obtencion de informacion relevante

#Media de edad y moda del estado de los pacientes (vivo/muerto)
age_mean <- summary(data$age)
ageMean <- mean(data$age)
deathFreq <- data.frame(table(data$DEATH_EVENT))
deathMode <- deathFreq[which.max(deathFreq$Freq),1] #203 sobreviven y 96 fallecen

#Frecuencias relevantes
diabetes_freq <- data.frame(table(data$diabetes)) #174 no son diabeticos y 125 lo son
anaemia_freq <- data.frame(table(data$anaemia)) #170 no sufren de anemia y 129 sufren de anemia
highBloodPressure_freq <- data.frame(table(data$high_blood_pressure)) #194 no sufren de presion alta y 105 sufren de presion alta
sex_freq <- data.frame(table(data$sex)) #105 mujeres y 194 hombres
smoking_freq <- data.frame(table(data$smoking)) #203 no son fumadores y 96 son fumadores


#Data frames seleccionados
df_age <- data.frame(table(data$age))
df_diabetes <- data.frame(table(data$diabetes))
df_sex <- data.frame(table(data$sex))
df_smoking <- data.frame(table(data$smoking))
df_ejection <- data.frame(table(data$ejection_fraction))
df_creatinine <- data.frame(table(data$serum_creatinine))
df_death <- data.frame(table(data$DEATH_EVENT))

#summary de edades
age_mean <- summary(data$age)
#Se obtiene que la edad aproximada de los pacientes corresponde a 60.83 

#frecuencias de sexo
gender_freq<- data.frame(table(data$sex))
#por lo tanto, sabemos que son 194 hombres y 105 mujeres
#summary de diabetes
diabetes_freq <- data.frame(table(data$diabetes))
#174 no son diabeticos y 125 si

#Matriz de covarianza
cov_matrix <- cov(data)
#Para identificar tienden a aumentar o disminuir a la vez el coef es positivo

#Matriz de correlacion
library(corrplot)
cor_matrix <- cor(data)
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#Se identifica que serum_creatinine presenta un grado de correlacion con death event
#Correlacion verifica si hay relacion lineal entre las variables

#Se grafica el numero de vivos (203) y fallecidos (96)
pDeath<-  plot.barchart(df_death,df_death$Var1,df_death$Freq,"Estado","Frecuencia","Estado del paciente")
show(pDeath)

#Tablas de contingencia
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

#H0: La observacion de la clase es independiente de la muerte
#H1: La observacion de la clase es dependiente de la muerte
ejection.chi <- chisq.test(contingency.ejectionDeath,simulate.p.value=TRUE)
creatinine.chi <- chisq.test(contingency.creatinineDeath,simulate.p.value=TRUE)

#Parte de mclust

#Mclust
life_state <- data$DEATH_EVENT
#df<- data[,c(2,4,5,6,8,13)]
df <- data
clPairs(df,life_state)

m1<-Mclust(df)
summary(m1)

m2 <- Mclust(df,G=3)
summary(m2, parameters = TRUE)

#BIC criterio de informacion bayesiano para saber que modelo elegir
BIC<-mclustBIC(df)
plot(BIC)
summary(BIC)

m3<- Mclust(df,G=3,modelNames = "VEI")
plot(m3, what="classification")
#legend("bottomright", legend= 1:3, #numero de clustersde mod6
 #     col = mclust.options("classPlotColors"),pch= mclust.options("classPlotSymbols"),title= "Classlabels:")

df2 <- data[,c(5,8,13)]
BIC2<-mclustBIC(df2)
plot(BIC2)
summary(BIC2)

m4 <- Mclust(df2,G=1,modelNames = "EEE")
plot(m4, what="classification")

#legend("bottomright", legend= 1:8, #numero de clustersde mod6
 #      col = mclust.options("classPlotColors"),pch= mclust.options("classPlotSymbols"),title= "Classlabels:")