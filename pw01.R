setwd('/HDD/Learning/Bedu/Bedu-R')

urls <- data.frame(
  url = c(
    'https://www.football-data.co.uk/mmz4281/1920/SP1.csv',
    'https://www.football-data.co.uk/mmz4281/1920/SP2.csv'),
  fileName = c(
    '1920SP1.csv',
    '1920SP2.csv')
)

download.file(url = urls$url, destfile = urls$fileName, mode = "wb")

library(dplyr)

datos <- lapply(urls$fileName, read.csv)
# goles anotados por equipos que jugaron en casa (FTHG)
# goles anotados por equipos que jugaron como visitante (FTAG)
datos <- lapply(datos, select, c("FTHG", "FTAG"))
datos <- do.call(rbind, datos)

contingency <- table(datos)
total <- dim(datos)[1]

# 1) La probabilidad (marginal) de que el equipo que juega en casa 
# anote x goles (x = 0, 1, 2, ...)
x <- sort(unique(datos$FTHG))
probas1 <- data.frame(x=x, prob=x)

for (i in x) {
  probas1[x==i,"prob"] = round((sum(contingency[,i + 1])/total) * 100, 2)
}

probas1


# 2) La probabilidad (marginal) de que el equipo que juega como 
# visitante anote y goles (y = 0, 1, 2, ...)
probas2 <- data.frame(y=sort(unique(datos$FTAG)), prob=1)

for (i in x) {
  probas2[y==i,"prob"] = round((sum(contingency[i + 1,])/total) * 100, 2)
}

probas2


# 3) La probabilidad (conjunta) de que el equipo que juega en casa anote 
# x goles y el equipo que juega como visitante anote y goles 
# (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
round(contingency/total * 100, 2)