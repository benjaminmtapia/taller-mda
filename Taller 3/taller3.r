require(maxent)
library(tm)
library(SnowballC)
library(wordcloud)
data<- read.csv(system.file("data/USCongress.csv.gz",package="maxent"))



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


#wordcloud inicial
wordcloud(data$text, random.order = FALSE, colors = brewer.pal(8,"Dark2"), max.words = 150)

#se revisa cuantos documentos hay de cada categoria
categories <- table(data$major)
prop_categories <- categories/4449 *100

##filtro del texto
data$text <- removeWords(data$text,stopwords("english")) #elimina stopwords en ingles
data$text <- tolower(data$text) #todas en minusculas
data$text <- stemDocument(data$text,language = "english") #stemming
data$text <- removeNumbers(data$text) #elimina numeros
data$text <- stripWhitespace(data$text) #elimina doble espacio
data$text <- removePunctuation(data$text) #elimina puntuacion

#wordcloud despues de filtrar
wordcloud(data$text, random.order = FALSE, colors = brewer.pal(8,"Dark2"), max.words = 150)


#aplicando el método
corpus <- Corpus(VectorSource(data$text))
matrix <- DocumentTermMatrix(corpus)
sparse <- as.compressed.matrix(matrix)

f <- tune.maxent(sparse,data$major,nfold=3, showall = TRUE, verbose=TRUE)
#model <- maxent(sparse, data$major, l1_regularizer = )


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
#relevante <- as.data.frame(training_result)
#relevante <- data.frame("relevante","no relevante")

recuperadoRelevante = 0
recuperadoNoRelevante = 0
nRecuperadoRelevante = 0
nRecuperadoNoRelevante = 0

for(i in 1:nrow(training_result)){
  row = training_result[i,]
  label = as.numeric(row[1,1])
  col <- which(colnames(row) == as.character(label))
  #relevant = nrow(subset(cursor, label == 3 ||  label == 5 ||  label == 12 ||  label ==18 || label == 20 || label == 21 )) == 1
  #Caso relevantes
  if (label == 3 || label == 5 ||  label == 12 ||  label == 18 || label == 20 || label == 21 ){
    #Recuperado
    if(as.numeric(row[1,col]) >= 0.7){ #EL PROBLEMA ES QUE HAY QUE ACCEDER A LA COLUMNA DEL ROW$"LABEL"
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
    if(as.numeric(row[1,col]) >= 0.7){
      recuperadoNoRelevante = recuperadoNoRelevante + 1
    }
    #No Recuperado
    else{
      nRecuperadoNoRelevante = nRecuperadoNoRelevante + 1
    }
  }
}

