library(readxl)
library(dplyr)

osoby <- read_xlsx("bazy_danych/01B Po osobach - nowe.xlsx")

P <- osoby %>% select(
  P1,
  P1.1,
  P2,
  P3_1,
  P3_2,
  P3_3,
  P3_4,
  P3_5,
  P3_6,
  P3_7,
  P3_8,
  P3_9,
  P3_10,
  P4_1,
  P4_2,
  P4_3,
  P4_4,
  P4_5,
  P4_6,
  P4_7,
  P4_7,
  P4_8,
  P4_9
)
P_original <- P

M <- osoby %>% select(
  M1, 
  M2, 
  M3,
  M4,
  M4a, 
  M4b
)
M_original <- M


D <- osoby %>% select(
  D1,
  D2,
  D2_a,
  D3,
  D3_kto
)
D_original <- D


EAS <- osoby %>% select(
  EAS_1,
  EAS_2,
  EAS_3,
  EAS_4,
  EAS_5,
  EAS_6,
  EAS_7,
  EAS_8,
  EAS_9,
  EAS_10,
  EAS_11,
  EAS_12,
  EAS_13,
  EAS_14,
  EAS_15,
  EAS_16,
  EAS_17,
  EAS_18,
  EAS_19,
  EAS_20
)
EAS_original <- EAS

EAS_r <- osoby %>% select(
  EAS_7_r,
  EAS_17_r,
  EAS_16_r,
  EAS_8_r,
  EAS_12_r,
  EAS_20_r
)
EAS_r_original <- EAS_r


EAS_rest <- osoby %>% select(
  EAS_EMO,
  EAS_AKTYW,
  EAS_TOW,
  EAS_NIESM
)
EAS_rest_original <- EAS_rest

R <- osoby %>% select(
  R2.1,
  R2.2,
  R2.3,
  R2.4,
  R2.5,
  R2.6_1,
  R2.6_2,
  R2.6_3,
  R2.6_4,
  R2.6_5,
  R2.6_6,
  R2.6_7,
  R2.6_8,
  R2.6_9,
  R2.6_10,
  R2.6_11,
  R2.6_12,
  R2.6_13,
  R2.6_14,
  R2.7,
  R2.8,
  R2.9
)
R_original <- R

## Sprawdzenie kompletności i odsiew złych kolumn
# P M D EAS EAS_r EAS_rest R 

library(DataExplorer)
library(dplyr)
library(tidyr)

plot_missing(P)
P %>% select(-P1.1) -> P
#
P_original %>% select(-P1.1) -> P_original

plot_missing(M)
M %>% select(-M4b) -> M
M %>% select(-M3) -> M
M %>% drop_na() -> M
#
M_original %>% select(-M4b) -> M_original
M_original %>% select(-M3) -> M_original

plot_missing(D)
D %>% select(-D3_kto) -> D
D %>% drop_na() -> D
#
D_original %>% select(-D3_kto) -> D_original

plot_missing(EAS)
EAS %>% drop_na() -> EAS

plot_missing(EAS_r)

plot_missing(EAS_rest)

plot_missing(R)
R %>% drop_na() -> R

## Teraz klasteryzacja
# P M D EAS EAS_r EAS_rest R
library(genie)

# Forest
P_tree <- hclust2(dist(scale(data.matrix(P))), thresholdGini = 0.09)
M_tree <- hclust2(dist(scale(data.matrix(M))))
D_tree <- hclust2(dist(scale(data.matrix(D))))
EAS_tree <- hclust2(dist(scale(data.matrix(EAS))), thresholdGini = 0.1)
EAS_r_tree <- hclust2(dist(scale(data.matrix(EAS_r))))
EAS_rest_tree <- hclust2(dist(scale(data.matrix(EAS_rest))))
R_tree <- hclust2(dist(scale(data.matrix(R))), thresholdGini = 0.1)

# Plots
library(ggplot2)
library(dendextend)

plot(P_tree)
plot(M_tree)
plot(D_tree)
plot(EAS_tree)
plot(EAS_r_tree)
plot(EAS_rest_tree)
plot(R_tree)

# Labels
P_l <- sapply(2:15, function(i) cutree(P_tree, i))
M_l <- sapply(2:15, function(i) cutree(M_tree, i))
D_l <- sapply(2:15, function(i) cutree(D_tree, i))
EAS_l <- sapply(2:15, function(i) cutree(EAS_tree, i))
EAS_r_l <- sapply(2:15, function(i) cutree(EAS_r_tree, i))
EAS_rest_l <- sapply(2:15, function(i) cutree(EAS_rest_tree, i))
R_l <- sapply(2:15, function(i) cutree(R_tree, i))

# Medialne

# M D EAS_rest

Pl11 <- P_l[,10]
Ml4 <- M_l[,3]
Dl3 <- D_l[,2]
EASrest4 <- EAS_rest_l[,3]
EASr3 <- EAS_r_l[,2]
Rl4 <- R_l[,3]


cbind(P, Pl11) %>% group_by(Pl11) %>% summarise_all(list(median)) -> P_medians
P_medians

cbind(M, Ml4) %>% group_by(Ml4) %>% summarise_all(list(median)) -> M_medians
M_medians

cbind(D, Dl3) %>% group_by(Dl3) %>% summarise_all(list(median)) -> D_medians
D_medians

cbind(EAS_rest, EASrest4) %>% group_by(EASrest4) %>% summarise_all(list(median)) -> EAS_rest_medians
EAS_rest_medians

cbind(EAS_r, EASr3) %>% group_by(EASr3) %>% summarise_all(list(median)) -> EAS_r_medians
EAS_r_medians

cbind(R, Rl4) %>% group_by(Rl4) %>% summarise_all(list(median)) -> Rl4_medians
Rl4_medians

# PCA
library(ggbiplot)

#M
M.pca <- prcomp(M, center = TRUE, scale.=TRUE)
summary(M.pca) # bez sensu

ggbiplot(M.pca)

#D 
D.pca <- prcomp(D, center = TRUE, scale.=TRUE)
summary(D.pca) # bez sensu

ggbiplot(D.pca)

#EAS_rest
EAS_rest.pca <- prcomp(EAS_rest, center = TRUE, scale.=TRUE)
summary(EAS_rest.pca) # bez sensu

ggbiplot(EAS_rest.pca)

# Właściwie niczego się nie dowiedzieliśmy

# Niestety nie chce mi się robić korelacji, bo to trzeba ustalić, które wiersze zostały usunięte.
# P M D EAS EAS_r EAS_rest R

# 1. Indeksy NA we wszystkich
P_original %>% plot_missing() #nic
M_original %>% plot_missing()
M_ind <- M_original$M2 %>% is.na

D_original %>% plot_missing()
D_ind <- D_original$D2_a %>% is.na


EAS_original %>% plot_missing()
EAS_ind <- EAS_original$EAS_18 %>% is.na


EAS_r_original %>% plot_missing()

EAS_rest_original %>% plot_missing()

R_original %>% plot_missing()
R_original[39,]
R_original[48,]
R_original[56,]
R_ind <- 1:nrow(R_original) %in% c(39, 48, 56)
#R_original[!R_ind,] %>% plot_missing()

# 2. Indeks przecięcie i labele
ind <- (M_ind | D_ind | EAS_ind | R_ind)

Labels <- data.frame(
  P = Pl11[!ind],
  M = Ml4[!ind[!M_ind]],
  D = Dl3[!ind[!D_ind]],
  EASrest = EASrest4[!ind],
  EASr = EASr3[!ind],
  R = Rl4[!ind[!R_ind]]
)

# chisq

asdfasdf <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))

P_table <- sapply(unique(Labels$P), function(i) Labels$P == i)
M_table <- sapply(unique(Labels$M), function(i) Labels$M == i)
D_table <- sapply(unique(Labels$D), function(i) Labels$D == i)
EASrest_table <- sapply(unique(Labels$EASrest), function(i) Labels$EASrest == i)
EASr_table <- sapply(unique(Labels$EASr), function(i) Labels$EASr == i)
R_table <- sapply(unique(Labels$R), function(i) Labels$R == i)

# PM
PM <- t(P_table) %*% M_table
chisq.test(PM)

# MD
MD <- t(M_table) %*% D_table
chisq.test(MD)

# Wszystkie

table_list <- list(
  P_table = P_table,
  M_table = M_table,
  D_table = D_table,
  EASrest_table = EASrest_table,
  EASr_table = EASr_table,
  R_table = R_table
)

Chisq <- matrix(0, nrow = 6, ncol = 6)
Chisq_p <- matrix(0, nrow = 6, ncol = 6)

dimnames(Chisq) <- list(names(table_list), names(table_list))
dimnames(Chisq_p) <- list(names(table_list), names(table_list))

for(n in names(table_list)) {
  for(m in names(table_list)){
    chi <- chisq.test(t(table_list[[n]]) %*% table_list[[m]])
    Chisq[n,m] <- chi$statistic
    Chisq_p[n,m] <- chi$p.value
  }
}

Chisq
Chisq_p

# colnames(Chisq) <- c("R","EASr", "EASrest", "D", "M", "P") %>% rev
# rownames(Chisq) <- colnames(Chisq)
# 
# colnames(Chisq_p) <- colnames(Chisq)
# rownames(Chisq_p) <- colnames(Chisq)
# 

# save(Chisq, Chisq_p, file="ChisqTestMatrix.rda")

par(mfrow=c(2,3))
plot(1:3)


# Znalezione związki!!! P z R i EASr z EASrest

heatmap(matrix(1:4, nrow = 2, ncol=2))
heatmap(Chisq)
