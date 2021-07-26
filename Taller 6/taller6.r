#HACER VALIDACION CRUZADA BALANCEADA, DIVIDIR SENAL EN LA MITAD, ENTRENAR CON UNA Y TESTEAR CON LA OTRA
#ORDENAR MODELOS POR EFICIENCIA EN TEST

require(e1071)

#datos = read.csv("G1_001.csv")
datos = read.csv("G4_001.csv")


attach(datos)

#Paso preliminar

Ts = 0.2
Tiempo = seq(Ts,(length(VFSC))*Ts,Ts)
formula = VFSC ~ PAM

#Generacion del modelo SVM

model <- svm(formula , datos)
VFSC_estimated <- predict(model, PAM)
eficiencia<-cor(VFSC_estimated,VFSC,method = "pearson")

plot(Tiempo,VFSC,type="l")
lines(Tiempo,VFSC_estimated, col = "red")
legend("topright", c("VFSC","VFSC_estimated"), title = paste("Corr=",round(eficiencia,digits=5)), pch = 1, col=c("blue","red"),lty=c(2,1),inset = 0.01)


#Generacion del modelo utilizando tune

tuneResult <- tune(svm, formula,  data = datos,
                   ranges = list(nu = seq(0.1,0.5,0.1), cost = 2^(-4:4), type="nu-regression")
)

tunedModel <- tuneResult$best.model
VFSC_tunedModel <- predict(tunedModel, PAM)
eficienciaTuned<-cor(VFSC_tunedModel,VFSC,method = "pearson")
plot(Tiempo,VFSC,type="l")
lines(Tiempo,VFSC_estimated, col = "red")
lines(Tiempo,VFSC_tunedModel, col = "blue")
legend("topright", c("VFSC",paste("VFSC_estimated corr=",round(eficiencia,5)),paste("VFSC_Tuned corr",round(eficienciaTuned,5))), title = "Correlacion", pch = 1, col=c("blue","red"),lty=c(2,1),inset = 0.01)

#Respuesta al impulso

#Se genera la matriz de retardos
retardos_multi <- function(
  signalData,
  lags
)
{
  
  signal.uni <- signalData
  max.lag <- max(unlist(lags)) + 1
  indices <- 1:nrow(signal.uni)
  lag.mat <- embed(indices, max.lag)
  
  col.names <- list("PAMn","VFSCn")
  columns <- NULL
  lagged.columns.names <- c()
  for(colname in col.names){
    
    lag.order <- lags[[colname]]
    columns[[colname]] <- signal.uni[lag.mat[, 1], colname]
    if(!is.null(lag.order) && lag.order > 0)
      for(i in 1:lag.order){
        new.colname <- paste(colname, paste0("lag", i), sep = ".")
        lagged.columns.names <- c(lagged.columns.names, new.colname)
        columns[[new.colname]] <- signal.uni[lag.mat[, i+1], colname]
      }
    
    
    
  }
  folded.signal <- data.frame(columns)
  
  sorting <- order(lag.mat[, 1])
  folded.signal <- folded.signal[sorting, ]
  list(folded.signal = folded.signal, lagged.columns.names = lagged.columns.names)
}


# Validacion cruzada

require(doParallel)
require(e1071)
registerDoParallel(cores = 4)
cost <- 2^seq(-4, 8, 2)
nu <- seq(0.1, 0.9, 0.4)
gamma<-2^seq(-4, 8, 2)
lagsList<-seq(1,5,1)

datos=read.csv("G1_001.csv")

# Normalizacion de los datos, se estandarizan en valores entre 0 - 1

PAMn<-(datos$PAM-min(datos$PAM))/(max(datos$PAM)-min(datos$PAM))
VFSCn<-(datos$VFSC-min(datos$VFSC))/(max(datos$VFSC)-min(datos$VFSC))
data <- data.frame(PAMn,VFSCn)
#Tiempo de muestreo
Ts=0.2


tam_muestra <- floor(0.50 * nrow(datos))
## set the seed to make your partition reproducible
set.seed(123)
training_index <- sample(nrow(data), size = tam_muestra)
training_set <- data[training_index, ]
test_set <- data[-training_index, ]


parms <- expand.grid(lagsList=lagsList, cost = cost, nu = nu, gamma=gamma)
salida <- (c( foreach(i = 1:nrow(parms),  combine = rbind, .inorder = FALSE) %dopar% {
  c <- parms[i, ]$cost
  n <- parms[i, ]$nu
  g <- parms[i, ]$gamma
  l <- parms[i, ]$lagsList
  lag<-list(PAMn = l,VFSCn = 0)
  signal.train <- retardos_multi(training_set, lag)
  retDatos=signal.train$folded.signal
  x=subset(retDatos, select = -VFSCn)
  y=retDatos$VFSCn
  modelo <- e1071::svm(x, y, type = "nu-regression", kernel = "radial", cost = c, nu = n, gamma=g)
  dataframe <- data.frame(PAMn = test_set$PAMn)
  colnames <- c("PAMn")
  aux <- ncol(x)-1
  for(i in 1:aux){
    colname <- paste('PAMn.lag',i,sep="")
    colnames <- append(colnames, colname)
    newcol <- data.frame(i = test_set$PAMn)
    dataframe <- cbind(dataframe, newcol)
  }
  colnames(dataframe) <-colnames
  pred <- predict(modelo, dataframe)
  corr_pred<-cor(pred,test_set$VFSCn,method = "pearson")
  dataframe <- NULL
  c(l, c, n, g, corr_pred)
}))


#Se muestran los resultados
output <- matrix(unlist(salida), ncol = 5, byrow = TRUE)
mejoresModelos<-output[order(output[,5], decreasing = TRUE),]
print(mejoresModelos)


# Capacidad de autoregulacion
inverseStep=matrix(1,180/Ts,1)
inverseStep[(90/Ts):(180/Ts),1]=0


for (i in 1:length(mejoresModelos[,1])){
  
  
  PAMn<-(datos$PAM-min(datos$PAM))/(max(datos$PAM)-min(datos$PAM))
  VFSCn<-(datos$VFSC-min(datos$VFSC))/(max(datos$VFSC)-min(datos$VFSC))
  data <- data.frame(PAMn,VFSCn)
  lag<-list(PAMn = mejoresModelos[i,1],VFSCn = 0)
  signal.train <- retardos_multi(data, lag)
  retDatos=signal.train$folded.signal
  
  x=subset(retDatos, select = -VFSCn)
  y=retDatos$VFSCn
  mejorModelo <- svm(x, y, kernel = "radial",type = "nu-regression", cost = mejoresModelos[i,2], nu = mejoresModelos[i,3], gamma=mejoresModelos[i,4])
  
  PAMn=inverseStep
  VFSCn=inverseStep 
  data <- data.frame(PAMn,VFSCn)
  lag<-list(PAMn = mejoresModelos[i,1],VFSCn = 0)
  signal.train <- retardos_multi(data, lag)
  retDatos=signal.train$folded.signal
  x=subset(retDatos, select = -VFSCn)
  y=retDatos$VFSCn
  
  stepTime=seq(Ts,(length(retDatos$PAMn))*Ts,Ts)
  stepResponse <- predict(mejorModelo, x ) 
  plot(stepTime,retDatos$PAMn,type="l", col="red")
  lines(stepTime,stepResponse, col = "blue")
  legend("topright", c("Escalon de presi?n", "respuesta al escalon"), title = "autorregulacion", pch = 1, col=c("red","blue"),lty=c(1,1),inset = 0.01)
  print(paste("corr=",mejoresModelos[i,5]))
  readline(prompt="Press [enter] to continue")
}



