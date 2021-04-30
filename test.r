library(mclust)
library(ggplot2)

#Obtencion del dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv"
data <- read.csv(url,header=FALSE, fill=TRUE)
data <- na.omit(data)
colnames(data) <- c("age","anaemia","creatinine_phosphokinase","diabetes","ejection_fraction",
                    "high_blood_pressure","platelets","serum_creatinine","serum_sodium","sex","smoking",
                    "time","DEATH_EVENT")
#Eliminacion de la fila contenedora de las clases
data <- data[-c(1),]
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
data <- as.data.frame(apply(data,2,as.numeric))
#Variables

#age -> edad del paciente (years)
#anemia -> disminucion de globulos rojos o hemoglobina (boolean)
#high blood pressure -> tiene hipertension (boolean)
#creatinine phosphokinase -> nivel de enzima CPK en la sangre (mcg/L)
#diabetes -> tiene diabetes (boolean)
#ejection fraction -> porcentaje de sangre expulsada en cada contraccion del corazon (percentage)
#platelets -> plaquetas en la sangre (kiloplatelets/mL)
#sex -> genero del paciente (binary)
#serum creatinine -> nivel de suero de creatinina en la sangre (mg/dL)
#serum sodium -> nivel de suero de sodio en la sangre (mEq/mL)
#smoking -> el paciente fuma (boolean)
#time ->  periodo de seguimiento(days)
#[target] death event -> si el paciente muere durante el periodo de seguimiento (boolean)

#Analisis estadistico variables fisiologicas
anaemiaFreq <- data.frame(table(data$anaemia))
anaemiaContingency <- table(data$anaemia)

hbpFreq <- data.frame(table(data$high_blood_pressure))
hbpContingency <- table(data$high_blood_pressure)

diabetesFreq <- data.frame(table(data$diabetes))
diabetesContingency <- table(data$diabetes)

smokingFreq <- data.frame(table(data$smoking))
smokingContingency <- table(data$smoking)

deathFreq <- data.frame(table(data$DEATH_EVENT))
deathContingency <- table(data$DEATH_EVENT)

cpkMean <- mean(data$creatinine_phosphokinase)
cpkMedian <- median(data$creatinine_phosphokinase)
cpkFreq <- data.frame(table(data$creatinine_phosphokinase))
cpkMode <- ageFreq[which.max(cpkFreq$Freq),1]
cpkHist <- hist(x = data$creatinine_phosphokinase, main = "Histograma de CPK", 
                xlab = "Medici?n (mcg/L)", ylab = "Frecuencia")

efMean <- mean(data$ejection_fraction)
efMedian <- median(data$ejection_fraction)
efFreq <- data.frame(table(data$ejection_fraction))
efMode <- ageFreq[which.max(efFreq$Freq),1]
efHist <- hist(x = data$ejection_fraction, main = "Histograma de Ejection Fracton", 
                xlab = "Medici?n (%)", ylab = "Frecuencia")

plMean <- mean(data$platelets)
plMedian <- median(data$platelets)
plFreq <- data.frame(table(data$platelets))
plMode <- ageFreq[which.max(plFreq$Freq),1]
plHist <- hist(x = data$platelets, main = "Histograma de Plaquetas", 
               xlab = "Medici?n (kiloplatelets/mL)", ylab = "Frecuencia")

serumCMean <- mean(data$serum_creatinine)
serumCMedian <- median(data$serum_creatinine)
serumCFreq <- data.frame(table(data$serum_creatinine))
serumCMode <- ageFreq[which.max(serumCFreq$Freq),1]
serumCHist <- hist(x = data$serum_creatinine, main = "Histograma de Suero Creatininina", 
               xlab = "Medici?n (mg/dL)", ylab = "Frecuencia")

serumSMean <- mean(data$serum_sodium)
serumSMedian <- median(data$serum_sodium)
serumSFreq <- data.frame(table(data$serum_sodium))
serumSMode <- ageFreq[which.max(serumSFreq$Freq),1]
serumSHist <- hist(x = data$serum_sodium, main = "Histograma de Suero Sodio", 
                   xlab = "Medici?n (mEq/mL)", ylab = "Frecuencia")

#Analisis estadistico variables demograficas
ageMean <- mean(data$age)
ageMedian <- median(data$age)
ageFreq <- data.frame(table(data$age))
ageMode <- ageFreq[which.max(ageFreq$Freq),1]
ageHist <- hist(x = data$age, main = "Histograma de Edad", 
                xlab = "Edad", ylab = "Frecuencia")


sexFreq <- data.frame(table(data$sex))
sexContingency <- table(data$sex)

timeMean <- mean(data$time)
timeMedian <- median(data$time)
timeFreq <- data.frame(table(data$time))
timeMode <- ageFreq[which.max(timeFreq$Freq),1]
timeHist <- hist(x = data$time, main = "Histograma de Tiempo de seguimiento", 
                xlab = "Tiempo (D?as)", ylab = "Frecuencia")



