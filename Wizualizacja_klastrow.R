library(cluster)
library(mclust)
library(ggplot2)
library(clues)
library(reshape2)
library(kableExtra)

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

ramkaPro$krokow_na_minute<-(ramkaPro$kroki_L+ramkaPro$kroki_P)/ramkaPro$D_długość_wyc

#plot_missing(ramkaPro)

#which(is.na(ramkaPro$D_zmeczenie_koniec))

#zamiast ankiety pod koniec bylo jakies inne wydarzenie
ramkaPro[51,"D_zmeczenie_koniec"]<-2


zbior<-ramkaPro[-1]
zbior<-scale(zbior)
# Klasteryzacja hclust2

library(genie)

d <- ramkaPro[-1] %>% data.matrix()%>% scale() %>% dist

ksrednie<-lapply(2:20,function(x){srednia<-kmeans(zbior,x,nstart = 25);srednia$cluster})

clusplot(zbior, ksrednie[[1]], color=TRUE, shade=TRUE, lines=0 )


i <- lapply(1:19,function(n){intCriteria(zbior, ksrednie[[n]], c("Gamma", "Davies_Bouldin", "Dunn"))})

xd<-data.frame(do.call(rbind, i))

xd<-cbind(xd,k=2:20)

xd$gamma<-as.numeric(xd$gamma)
xd$dunn<-as.numeric(xd$dunn)
xd$davies_bouldin<-as.numeric(xd$davies_bouldin)

library(ggplot2)

ggplot() +
  geom_line(data = xd, aes(y = gamma, x = k), color = 'red') +
  geom_line(data = xd, aes(y = dunn, x = k), color = 'green') +
  geom_line(data = xd, aes(y = davies_bouldin, x = k), color = 'blue')

#sugerowana liczba klastrow to 2 oraz 5
clusplot(zbior, ksrednie[[1]], color=TRUE, shade=TRUE, lines=0 )

clusplot(zbior, ksrednie[[4]], color=TRUE, shade=TRUE, lines=0 )


#pca

zbior.pca<-prcomp(zbior)
plot(zbior.pca)
summary(zbior.pca)


