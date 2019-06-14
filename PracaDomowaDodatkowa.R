library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

setwd("~/GitHub/WUM_projekt2")

osoby <- read_xlsx("bazy_danych/01B Po osobach - nowe.xlsx")

gajewska <- ggplot(osoby, aes(x = P1, fill = factor(ifelse(D1 == 1, "Kobieta", "Mężczyzna")))) +
  geom_bar(width = 0.9) +
  facet_grid(~D2) +
  xlab("Który raz w centrum nauki kopernik?") +
  ylab("Liczność") +
  ggtitle("Liczba odwiedzin Centrum nauki Kopernik w rozbiciu na roku urodzenia uczestników") + 
  labs(fill = "Płeć") +
  scale_x_continuous(breaks=1:10) +
  theme_minimal()

# 2 
setwd("~/Bogdan/ML/pdDodatkowa")

heart <- read_csv("heart.csv")

rydelek <- ggplot(heart, aes(y = chol, x = trestbps)) +
  facet_grid(~thal) +
  geom_point() + 
  ggtitle("Trestbps od Chol w rozbiciu na Thal") +
  theme_bw()

setwd("~/Bogdan/ML/pd1")

library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(visdat)
library(DataExplorer)
require(gridExtra)
library(scales)
enc <- guess_encoding("allegro-api-transactions.csv", n_max = 10000)[[1]]
allegroApiTransactions <- as.data.frame(read_csv("allegro-api-transactions.csv", locale = locale(encoding = enc[1])))
allegroCategoriesEng <- as.data.frame(read_delim("allegro-categories-eng.txt", delim=";"))
data <- full_join(allegroApiTransactions, allegroCategoriesEng)
data <- data[,-dim(data)[2]]

ggplot(data, aes(x=main_category, y=date)) + geom_boxplot() + coord_flip()

hubert <- ggplot(data, aes(x = date)) +
  facet_wrap(~main_category, nrow=9) +
  geom_density(fill = "gray", color = "black") +
  theme_bw() +
  scale_x_datetime(labels = date_format("%H")) +
  xlab("Godzina dnia") +
  ylab("Gęstość") +
  ggtitle("Dzienne rozkłady zakupów na allegro.pl 3 kwietnia 2016 r.")

setwd("~/Bogdan/ML/pdDodatkowa")
save(hubert, gajewska, rydelek, file="Plots.rda")

ggplot(data, aes(x = date)) +
  facet_grid(rows = vars(main_category)) +
  geom_density(fill = "gray", color = "black") +
  theme_bw() +
  scale_x_datetime(labels = date_format("%H")) +
  xlab("Godzina dnia") +
  ylab("Gęstość") +
  ggtitle("Dzienne rozkłady zakupów na allegro.pl 3 kwietnia 2016 r.")





