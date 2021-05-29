require(maxent)
library(tm)
library(SnowballC)
library(wordcloud)
data<- read.csv(system.file("data/USCongress.csv.gz",package="maxent"))


#wordcloud inicial


##filtro del texto
data$text <- removeWords(data$text,stopwords("english")) #elimina stopwords en ingles
data$text <- tolower(data$text) #todas en minusculas
data$text <- stemDocument(data$text,language = "english") #stemming
data$text <- removeNumbers(data$text) #elimina numeros
data$text <- stripWhitespace(data$text) #elimina doble espacio
data$text <- removePunctuation(data$text) #elimina puntuacion

#wordcloud despues de filtrar


#aplicando el método
corpus <- Corpus(VectorSource(data$text))
matrix <- DocumentTermMatrix(corpus)
sparse <- as.compressed.matrix(matrix)

f <- tune.maxent(sparse,data$major,nfold=3, showall = TRUE, verbose=TRUE)
