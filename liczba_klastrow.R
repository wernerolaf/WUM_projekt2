# Przygotowanie danych

library(readxl)
library(dplyr)

wycieczki <- read_xlsx("bazy_danych/01A Obserwacje_nowe.xlsx")
osoby <- read_xlsx("bazy_danych/01B Po osobach - nowe.xlsx")

osoby %>% select(Nr,
                 P1,
                 P2,
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
                 M4,
                 M4a,
                 D1,
                 D2,
                 EAS_EMO,
                 EAS_AKTYW,
                 EAS_TOW,
                 EAS_NIESM,
                 D2.1,
                 D2.3,
                 D2.6,
                 D2.7,
                 D2.5,
                 D2.10) -> dzieci

colnames(wycieczki)[10] <- "D_zmeczenie"
colnames(wycieczki)[21] <- "D_wola"
colnames(wycieczki)[27] <- "D_uzywa"

wycieczki %>% 
  group_by(ID) %>% 
  summarise(
    D_nastroj_srednia = mean(D_nastroj,  na.rm = TRUE),
    D_zmeczenie_pocz = head(D_zmeczenie, 1),
    D_zmeczenie_koniec = tail(D_zmeczenie, 1),
    kroki_L = ifelse(max(kroki_L, na.rm = TRUE) == -Inf, max(kroki_P, na.rm = TRUE), max(kroki_L, na.rm = TRUE)),
    kroki_P = ifelse(max(kroki_P, na.rm = TRUE) == -Inf, max(kroki_L, na.rm = TRUE), max(kroki_P, na.rm = TRUE)),
    D_wola = sum(D_wola,  na.rm = TRUE),
    D_pyta = sum(D_pyta,  na.rm = TRUE),
    D_dotyka = sum(D_dotyka,  na.rm = TRUE),
    D_uzywa = sum(D_uzywa,  na.rm = TRUE),
    D_długość_wyc = max(Nastr_kiedy,  na.rm = TRUE) - min(Nastr_kiedy,  na.rm = TRUE),
    liczba_eksponatow = length(unique(`Nazwa eksponatu`)) - 1
  ) -> wycieczki_pro

wycieczki_pro$D_długość_wyc %>% as.numeric * 60 -> wycieczki_pro$D_długość_wyc

inner_join(dzieci, wycieczki_pro, by=c("Nr" = "ID")) -> ramkaPro

# Klasteryzacja hclust2

library(genie)

d <- ramkaPro[-1] %>% data.matrix()%>% scale() %>% dist

forest <- lapply(seq(0.0,1, length.out = 100), FUN = function(g) {hclust2(d, tresholdGini = g)} )

plot(forest[[90]])

# Klasteryzacja samych dzieci

d_dzieci <- dzieci[-1] %>% data.matrix()%>% scale() %>% dist

stumilowy_las <- lapply(seq(0.0,0.5, length.out = 100), FUN = function(g) {hclust2(d_dzieci, tresholdGini = g)})

plot(stumilowy_las[[80]])

labels <- cutree(stumilowy_las[[80]], 8) 

table(labels)
cbind(dzieci,labels) %>%
  group_by(labels) %>%
  summarise_all(median)

library(clusterCrit)

statystyki <- function(d, pred) {
  
  i <- intCriteria(data.matrix(d), pred, c("Gamma", "Davies_Bouldin", "Dunn", "Silhouette"))
  
  c(silhouette = i$silhouette,
    gamma = i$gamma,
    davb = i$davies_bouldin,
    dunn = i$dunn)
}

#dzieci[is.na(dzieci$D2_a) | is.na(dzieci$R2.6_3) | is.na(dzieci$R2.3),]
#dzieci

labels_children <- lapply(2:40, function(k) cutree(stumilowy_las[[20]], k))

stat_labels_children <- sapply(labels_children, function(l) statystyki(dzieci[,-1],l))

stat_labels_children %>% t %>% data.frame %>% cbind(k = 2:40) -> stat_labels_dzieci

i <- intCriteria(data.matrix(dzieci[-1]), labels_children[[1]], c("Gamma", "Davies_Bouldin", "Dunn", "Silhouette"))

library(DataExplorer)
plot_missing(dzieci)


# Plotowanie 

library(ggplot2)

ggplot() +
  geom_line(data = stat_labels_dzieci, aes(y = silhouette, x = k), color = 'hotpink') +
  geom_line(data = stat_labels_dzieci, aes(y = dunn, x = k), color = 'seagreen') +
  geom_line(data = stat_labels_dzieci, aes(y = gamma, x = k), color = 'skyblue') +
  geom_line(data = stat_labels_dzieci, aes(y = davb, x = k), color = 'salmon')




















