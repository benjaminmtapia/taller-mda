require(maxent)
library(tm)
library(SnowballC)
library(wordcloud)

#Se importa el dataset
data<- read.csv(system.file("data/USCongress.csv.gz",package="maxent"))

#Se definen funciones para el calculo de metricas
precision <- function(table){
  vp <- table[1,1]
  fp <- table[1,2]
  result <- vp/(vp+fp)
  return(result)
}

recall <- function(table){
  vp <- table[1,1]
  fn <- table[2,1]
  result <- vp/(vp+fn)
  return(result)
}

#Se determina el wordcloud de datos sin pre procesamiento

#wordcloud inicial
wordcloud(data$text, random.order = FALSE, colors = brewer.pal(8,"Dark2"), max.words = 150)

#se revisan cuantos documentos hay de cada categoria
categories <- table(data$major)
prop_categories <- categories/4449 *100

### Utilizando el dataset puro ###

#aplicando el metodo
corpus1 <- Corpus(VectorSource(data$text))
matrix1 <- DocumentTermMatrix(corpus1)
sparse1 <- as.compressed.matrix(matrix1)

fOriginal <- tune.maxent(sparse1,data$major,nfold=3, showall = TRUE, verbose=TRUE)

##Se crea el set de entrenamiento
#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
training_index1 <- sample(nrow(data), size = nrow(data)*0.8)
training_data1 <- data[training_index1,]
#set de prueba
test_set1 <- data[-training_index1,] #Test_data

#se aplica max entropia a conjunto de entrenamiento
training_corpus1 <- Corpus(VectorSource(training_data1$text))
training_matrix1 <- DocumentTermMatrix(training_corpus1)
training_sparse1 <- as.compressed.matrix(training_matrix1)
training_tune1 <- tune.maxent(training_sparse1, training_data1$major, nfold=3,showall = TRUE,verbose =TRUE)
training_model1 <- maxent(test_set, training_data1$major, l1_regularizer = training_tune1[8,1], l2_regularizer = training_tune1[8,2],use_sgd = training_tune1[8,3],set_heldout = training_tune1[8,4]) #modelo de maxent
training_result1 <- as.data.frame(predict(training_model1,training_sparse1))

#Se aplica el metodo al conjunto de datos de prueba
corpus_test <- Corpus(VectorSource(test_set1$text))
matrix_test <- DocumentTermMatrix(corpus_test)
sparse_test <- as.compressed.matrix(matrix_test)
results_test <- predict(training_model1,sparse_test) #Results_

recuperadoRelevante1 = 0
recuperadoNoRelevante1 = 0
nRecuperadoRelevante1 = 0
nRecuperadoNoRelevante1 = 0

for(i in 1:nrow(results_test)){
  row = results_test[i,]
  label = as.numeric(row[1])
  col <- which(colnames(row) == as.character(label))
  #Caso relevantes
  if (label == 3 || label == 5 ||  label == 12 ||  label == 18 || label == 20 || label == 21 ){
    #Recuperado
    if(as.numeric(row[1]) >= 0.55){ 
      recuperadoRelevante1 = recuperadoRelevante1 + 1
    }
    #No Recuperado
    else{
      nRecuperadoRelevante1 = nRecuperadoRelevante1 + 1
    }
  }
  #Caso no relevantes
  else{
    #Recuperado
    if(as.numeric(row[1]) >= 0.55){
      recuperadoNoRelevante1 = recuperadoNoRelevante1 + 1
    }
    #No Recuperado
    else{
      nRecuperadoNoRelevante1 = nRecuperadoNoRelevante1 + 1
    }
  }
}


### Utilizando el dataset con pre-procesamiento ###

##Filtro del texto
data$text <- gsub("A bill","",data$text)
data$text <- tolower(data$text) #todas en minusculas
data$text <- removeWords(data$text,stopwords("english")) #elimina stopwords en ingles
data$text <- stemDocument(data$text,language = "english") #stemming
data$text <- removeNumbers(data$text) #elimina numeros
data$text <- stripWhitespace(data$text) #elimina doble espacio
data$text <- removePunctuation(data$text) #elimina puntuacion

#wordcloud despues de filtrar
wordcloud(data$text, random.order = FALSE, colors = brewer.pal(8,"Dark2"), max.words = 150)

#aplicando el metodo
corpus <- Corpus(VectorSource(data$text))
matrix <- DocumentTermMatrix(corpus)
sparse <- as.compressed.matrix(matrix)

f <- tune.maxent(sparse,data$major,nfold=3, showall = TRUE, verbose=TRUE)


##se crea el set de entrenamiento
#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
training_index <- sample(nrow(data), size = nrow(data)*0.8)
training_data <- data[training_index,]
#set de prueba
test_set <- data[-training_index,]

#se aplica max entropia a conjunto de entrenamiento
training_corpus<- Corpus(VectorSource(training_data$text))
training_matrix <- DocumentTermMatrix(training_corpus)
training_sparse <- as.compressed.matrix(training_matrix)
training_tune <- tune.maxent(training_sparse, training_data$major, nfold=3,showall = TRUE,verbose =TRUE)
training_model <- maxent(training_sparse, training_data$major, l1_regularizer = training_tune[8,1], l2_regularizer = training_tune[8,2],use_sgd = training_tune[8,3],set_heldout = training_tune[8,4]) #modelo de maxent
training_result <- as.data.frame(predict(training_model,training_sparse))

#Aplicando el metodo
corpus_test <- Corpus(VectorSource(test_set$text))
matrix_test <- DocumentTermMatrix(corpus_test)
sparse_test <- as.compressed.matrix(matrix_test)
results_test <- predict(training_model,sparse_test)

recuperadoRelevante = 0
recuperadoNoRelevante = 0
nRecuperadoRelevante = 0
nRecuperadoNoRelevante = 0

<<<<<<< HEAD
#conjunto de prueba
corpus_test <- Corpus(VectorSource(test_set$text))
matrix_test <- DocumentTermMatrix(corpus_test)
sparse_test <- as.compressed.matrix(matrix_test)
results_test <- predict(training_model,sparse_test)




=======
>>>>>>> 7f9f5b0a3f9f74a75c43de43d3f92a6a92deca74
for(i in 1:nrow(results_test)){
  row = results_test[i,]
  label = as.numeric(row[1])
  col <- which(colnames(results_test) == as.character(label))
  #Caso relevantes
  if (label == 3 || label == 5 ||  label == 12 ||  label == 18 || label == 20 || label == 21 ){
    #Recuperado
    if(as.numeric(row[col]) >= 0.55){ 
      recuperadoRelevante = recuperadoRelevante + 1
    }
    #No Recuperado
    else{
      nRecuperadoRelevante = nRecuperadoRelevante + 1
    }
  }
  #Caso no relevantes
  else{
    #Recuperado
    if(as.numeric(row[col]) >= 0.55){
      recuperadoNoRelevante = recuperadoNoRelevante + 1
    }
    #No Recuperado
    else{
      nRecuperadoNoRelevante = nRecuperadoNoRelevante + 1
    }
  }
}
