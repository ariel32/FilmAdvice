library(jsonlite)
library(tm)

ED <- fromJSON("http://www.omdbapi.com/?t=EDGE+OF+TOMORROW&y=&plot=full&r=json")
SM <- fromJSON("http://www.omdbapi.com/?t=space+marine&y=&plot=full&r=json")
TM <- fromJSON("http://www.omdbapi.com/?t=The+Matrix&y=&plot=full&r=json")
EQ <- fromJSON("http://www.omdbapi.com/?t=Equilibrium&y=&plot=full&r=json")

clean_text <- function(x, stop.words, stemming = TRUE){
  
  require(tm)
  require(stringr)
  
  x <- tolower(x)
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_replace_all(x, "[[:digit:]]", " ")
  #x <- str_replace_all(x, "[a-z]", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- removeWords(x, stop.words)
  x <- unlist(strsplit(x, " "))
  x <- x[x != ""]
}

corpus <- Corpus(VectorSource(c(ED$Plot, SM$Plot, TM$Plot, EQ$Plot)))
corpus <- tm_map(corpus, clean_text, stopwords(kind = "en"))
corpus <- tm_map(corpus, PlainTextDocument)

# Создание терм-документной матрицы с "сырыми" частотами слов:
dt.tf <- TermDocumentMatrix(corpus)

# Расчет TF-IDF-весов слов:
dt.tfidf <- weightTfIdf(dt.tf, normalize = TRUE)
dimnames(dt.tfidf)$Docs <- c("ED", "SM", "TM", "EQ")

#install.packages("lsa")
library(lsa) # пакет, содержащий функцию для расчета косинусного сходства

# Расчет косинусного сходства для всех пар документов:
cos.mat <- as.dist(cosine(as.matrix(dt.tfidf)))

# Просмотр матрицы сходства:
cos.mat


# Визуализация матрицы сходства в виде графа:

# преобразование cos.mat в таблицу для дальнейшего преобразование в граф:
m <- data.frame(t(combn(c("ED", "SM", "TM", "EQ"), 2)),
                as.numeric(cos.mat))
names(m) <- c("c1", "c2", "similarity")

# визуализация графа:
#install.packages("igraph")
require(igraph)
g <- graph.data.frame(m, directed = FALSE)
E(g)$weight <- m$similarity

plot(g, edge.width = E(g)$weight*120,
     layout = layout.circle,
     vertex.color = "#d95f02", 
     vertex.label.color = "white",
     vertex.size = 30,
     vertex.label.cex = 1.5,
     edge.color = ifelse(E(g)$weight*100 > 10, "#7570b3", "#1b9e77") )


# LDA-анализ ------------------------------------------------------------------
# Приведенный код основан на примере из http://bit.ly/1jEogWC
#install.packages("topicmodels")
library(topicmodels)

# LDA-анализ основан на использовании имитаций Монто-Карло с использованием
# сэмплера Гиббса. Ниже задаются параметры, управляющие процессом имитаций:
burnin = 2000
iter = 10000
keep = 50

# Оптимальное число тем будет найдено в результате поиска по диапазону от 2 до 20:
n.topics <- seq(2, 20, 1)

# Подгонка нескольких LDA-моделей с разными значениями k (количество тем):
set.seed(101)
fitted_many <- lapply(n.topics, function(k) LDA(t(dt.tf),
                                                k = k,
                                                method = "Gibbs",
                                                control = list(burnin = burnin,
                                                               iter = iter,
                                                               keep = keep)))

# Извлечение значений максимального правдоподобия для каждой модели
# (за исключением первых burnin/keep значений):
logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])

# Расчет гармонических средних по значениям максимального правдоподобия,
# полученных выше для каждой модели (см. http://epub.wu.ac.at/3558/1/main.pdf).
# Оптимальной является модель с максимальным значением такого гармонического среднего:
library(psych)
hm_many <- sapply(logLiks_many, function(h) harmonic.mean(h))

# Графическое изображение гармонических средних:
plot(n.topics, hm_many, type = "l")

# Оптимальное число тем:
n.topics[which.max(hm_many)]
## 20?

# Просмотр наиболее вероятных тем для каждого документа:
topics(fitted_many[[which.max(hm_many)]], 1)

# Просмотр наиболее вероятных слов, характеризующих наиболее вероятную тему:
terms(fitted_many[[which.max(hm_many)]], 10)
