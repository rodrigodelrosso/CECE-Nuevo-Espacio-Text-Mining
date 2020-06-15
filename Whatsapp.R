##########################################################
#      ANÁLISIS DE INFORMACIÓN CUALITATIVA USANDO R      #
#	                CONFERENCIAS CECE - UBA                #
#            FACULTAD DE CIENCIAS ECONÓMICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

# Expositores: Martin Masci - Rodrigo Del Rosso
# Moderador  : Diego Parras
# Lunes 15 de Junio de 2020

## Limpiar la consola ##
rm(list = ls())

## Cargar paquetes ##
suppressPackageStartupMessages({

  library("rwhatsapp")
  library("dplyr")
  library("ggplot2")
  library("lubridate")
  library("tidyr")
  library("ggimage")
  library("tidytext")
  library("stopwords")
  
})

## Setear ruta de trabajo
path = "C:/Users/rdelr/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Charlas/Text Mining CECE/"
setwd(path)

dir()

## Cargar los datos ##
chat <- rwa_read(paste0(path,"chat_mim_14062020.txt")) %>% 
  filter(!is.na(author))           # Remover mensajes sin autor

head(chat,n = 10)
tail(chat,n = 10)

theme_set(theme_minimal())

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Mensajes por día")

chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Cantidad de Mensajes")

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Emojis más utilizados")

# Datos integrados en el paquete
emoji_data <- rwhatsapp::emojis %>% 
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignorar emojis combinados
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Emojis más utilizados") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más utilizadas")


## PALABRAS A REMOVER
to_remove <- c(stopwords(language = "es"),
               "media",
               "omitted",
               "ref",
               "dass",
               "schon",
               "mal",
               "android.s.wt",
               "image",
               "sticker")

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más utilizadas")

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras importante mediante tf-idf por Autor")


chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 10000))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Palabras únicas") +
  xlab("") +
  ggtitle("Diversidad Léxica") +
  coord_flip()

## Elijo una persona del grupo
persona = "Guille Klein"

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != persona) %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == persona) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # solo selecciona palabras que nadie más usa
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle(paste0("Palabras únicas de ",persona))

## WORD CLOUDS EN WHATSAPP ##

## Cargar paquetes para hacer Nube de Palabras
suppressPackageStartupMessages({
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(RColorBrewer)
  library(stringr)
  library(caret)
})

text = readLines(paste0(path,"chat_mim_14062020.txt"))

# Cargar los datos como un Corpus
corpus <- Corpus(VectorSource(text)) # formato de texto

inspect(corpus)

## Transformación del Texto

# lleva a minúsculas
d  <- tm_map(corpus, tolower)

# quita espacios en blanco
d  <- tm_map(d, stripWhitespace)

# quita la puntuación
d <- tm_map(d, removePunctuation)

# quita los números
d <- tm_map(d, removeNumbers)

stopwords("spanish")

# remueve palabras vacías genericas
d <- tm_map(d, removeWords, stopwords("spanish"))

# crea matriz de términos
tdm <- TermDocumentMatrix(d)

findFreqTerms(tdm, lowfreq=20)

frecuentes <- findFreqTerms(tdm, lowfreq=20)

findAssocs(tdm, "utdt", 0.45)

findAssocs(tdm, frecuentes, rep(0.45, rep=5))

## Sumarización

# lo vuelve una matriz
m <- as.matrix(tdm) 

# lo ordena y suma
v <- sort(rowSums(m),decreasing=TRUE)

# lo nombra y le da formato de data.frame
df <- data.frame(word = names(v),freq=v)

### Trazar frecuencia de palabras
barplot(df[1:10,]$freq, 
        las = 2, 
        names.arg = df[1:10,]$word,
        col ="lightblue", 
        main ="Palabras más frecuentes", 
        ylab = "Frecuencia de palabras")

## Nube de Palabras
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 6,
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
