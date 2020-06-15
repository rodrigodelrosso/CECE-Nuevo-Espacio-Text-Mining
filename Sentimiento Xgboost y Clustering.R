##########################################################
#      ANÁLISIS DE INFORMACIÓN CUALITATIVA USANDO R      #
#	                CONFERENCIAS CECE - UBA                #
#            FACULTAD DE CIENCIAS ECONÓMICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

# Expositores: Martin Masci - Rodrigo Del Rosso
# Moderador  : Diego Parras
# Lunes 15 de Junio de 2020

library("tm")
library("stringr")
library("caret")

## Setear ruta de trabajo
path = "C:/Users/rdelr/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Charlas/Text Mining CECE/"
setwd(path)

puntaje_review <- read.table(file = "clasificacion.txt", 
                             sep="\t", 
                             header=TRUE)

# Cargo el corpus
corpus <- VCorpus(x = DirSource(directory = "corpus", encoding = "windows-1251"), 
                  readerControl = list(language = "es"))

corpus[[45]]$meta
corpus[[45]]$content

# Paso a minúscuola
corpus <- tm_map(corpus, content_transformer(tolower))
corpus[[45]]$content

# Quito signos de puntuación
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus[[45]]$content

# Quito stopwords
print(stopwords("spanish"))
stp_words <- stopwords("spanish")[stopwords("spanish") != "no"]
corpus <- tm_map(corpus, content_transformer(function(x) removeWords(x, stp_words)))
corpus[[45]]$content

# Quito doble espacio y espacio al final (esto es de TOC)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+", " ", str_trim(x))))
corpus[[45]]$content

# Pruebo el tokenizador
scan_tokenizer(corpus[[45]]$content)

# Paso a document-term matrix, no considero palabras que aparecen menos de 20 veces
dt.mat <- as.matrix(DocumentTermMatrix(corpus,
                                       control=list(stopwords=FALSE,
                                                    wordLengths=c(1, Inf),
                                                    bounds=list(global=c(20,Inf)))))

# Elimino post que quedaron sin palabras
dt.mat <- dt.mat[rowSums(dt.mat)!=0,]
dim(dt.mat)

# Asigno una columna que sea id de comentario
dt.mat <- data.frame(id_comentario=gsub(".txt", "", rownames(dt.mat)), dt.mat)
rows.dt.mat <- rownames(dt.mat)

# Uno la clase y separo en "malo" y "no_malo"
dt.mat <- merge(dt.mat, puntaje_review, by="id_comentario")  # Inner join
table(dt.mat$clase_comentario)
dt.mat$clase_comentario <- factor(ifelse(dt.mat$clase_comentario=="Excelente", "excelente", "no_excelente"))
dt.mat <- dt.mat[,-1]  # Quito el id
rownames(dt.mat) <- rows.dt.mat

# Entreno un modelo que detecte sentimiento
fitControl <- trainControl(method="LGOCV", number=1, p=0.75,
                           verboseIter=TRUE, classProbs=TRUE,
                           summaryFunction=twoClassSummary)

# xgboost
xgbTreeFit <- train(clase_comentario ~ ., data=dt.mat,
                    method="xgbTree",
                    trControl=fitControl,
                    tuneLength=1,
                    metric="ROC")

xgbTreeFit$results[which.max(xgbTreeFit$results$ROC),]
plot(varImp(xgbTreeFit), top=30)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Quito la columna de clases
dt.mat.dist <- subset(dt.mat, select=-clase_comentario)

# Normalizo por fila
norma2 <- apply(dt.mat.dist, 1, function(x) sqrt(sum(x^2)))
dt.mat.dist <- dt.mat.dist * (1/as.matrix(norma2, ncol=1))
# Crequeo que tengan norma 1
apply(dt.mat.dist, 1, function(x) sqrt(sum(x^2)))

# Muestreo 1500 documentos al azar y tomo distancia coseno
dt.mat.dist <- as.matrix(dt.mat.dist[sample(c(1:nrow(dt.mat.dist)), 1500),])
dist.cos <- 1 - dt.mat.dist %*% t(dt.mat.dist)

# Hago clustering
hc.docs <- hclust(as.dist(dist.cos), method="average")
plot(hc.docs, main="Cluster of documents", xlab="" , sub="" , cex=.9, labels=FALSE)
