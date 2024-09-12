# Projet series chronologiques, Brugel Theo, Husson Clement, Rieu Valentin et
# Sow Amadou Moussa
# Novembre 2023

library(tidyverse)
library(forecast)
library(gap)
library(zoo)

calcul_periodicite <- function(serie, p) {
    n <- length(serie)
    N <- floor(n/p)
    periode <- rep(NA, p)

    for(i in 1:p) {
        periode[i] <- mean(serie[i+(1:(N+1)-1)*p], na.rm=TRUE)
    }

    if(n == N*p) {
        periode <- rep(periode, N)
    }
    if(n > N*p) {
        periode <- c(rep(periode, N), periode[1:(n-(N*p))])
    }

    return(periode)
}

# Pretraitement

intersection <- read.csv("./route_intersection.csv")

intersection$vehicule = ceiling(intersection$vehicule)
intersection$date = lubridate::ymd_hms(intersection$date)
intersection$E <- rep(NA, nrow(intersection))
intersection$S <- rep(NA, nrow(intersection))

common_list <- intersection %>% group_by(intersection)
common_list = split(common_list, f=common_list$intersection)

common_list$I1$E <- common_list$I1$vehicule - (common_list$I2$vehicule - common_list$S2$vehicule)
common_list$I2$E <- common_list$I2$vehicule - (common_list$I3$vehicule - common_list$S3$vehicule)

n3 <- nrow(common_list$I3)
n4 <- nrow(common_list$I4)

common_list$I3$E[1:(n3-n4)] <-
    common_list$I3$vehicule[1:(n3-n4)] - common_list$N$vehicule[1:(n3-n4)]

common_list$I3$E[(n3-n4+1):n3] <-
    common_list$I3$vehicule[(n3-n4+1):n3] - (common_list$I4$vehicule - common_list$S4$vehicule)

common_list$I4$E <- common_list$I4$vehicule - common_list$N$vehicule[(n3-n4+1):n3]

common_list$I1$S <- common_list$S1$vehicule
common_list$I2$S <- common_list$S2$vehicule
common_list$I3$S <- common_list$S3$vehicule
common_list$I4$S <- common_list$S4$vehicule

common_list$I1$post_cassure <- common_list$I1$date > lubridate::ymd_hms("2017-01-01 00:00:00")
common_list$I2$post_cassure <- common_list$I2$date > lubridate::ymd_hms("2017-01-01 00:00:00")
common_list$I3$post_cassure <- common_list$I3$date > lubridate::ymd_hms("2017-01-01 00:00:00")

common_list$S1 <- NULL
common_list$S2 <- NULL
common_list$S3 <- NULL
common_list$S4 <- NULL

intersec <- intersection %>%
    filter(intersection=="I1" | intersection=="I2" | intersection=="I3" |
           intersection=="I4")

intersec <- intersec[-c(4, 5)]
intersec$post_cassure <- intersec$date > lubridate::ymd_hms("2017-01-01 00:00:00")
intersec$index <- rep(1:(nrow(intersec)/3), each=3)

rm(intersection)

################################################################################

# Lissage par moyenne mobile

k <- 12

res_rollmean1 <- rollmean(common_list$I1$vehicule, k=2*k+1)
res_rollmean2 <- rollmean(common_list$I2$vehicule, k=2*k+1)
res_rollmean3 <- rollmean(common_list$I3$vehicule, k=2*k+1)

common_list$I1$vehicule1 <- c(rep(NA, k), res_rollmean1, rep(NA, k))
common_list$I2$vehicule1 <- c(rep(NA, k), res_rollmean2, rep(NA, k))
common_list$I3$vehicule1 <- c(rep(NA, k), res_rollmean3, rep(NA, k))

intersec$vehicule1 <- NA
intersec[which(intersec$intersection=="I1"), ]$vehicule1 <- common_list$I1$vehicule1
intersec[which(intersec$intersection=="I2"), ]$vehicule1 <- common_list$I2$vehicule1
intersec[which(intersec$intersection=="I3"), ]$vehicule1 <- common_list$I3$vehicule1

ggplot(intersec[which(intersec$intersection!="I4"), ])+
    aes(date, vehicule1)+
    geom_line()+
    geom_smooth(aes(date, vehicule, color=post_cassure), method="lm")+
    geom_vline(xintercept=lubridate::ymd_hms("2017-01-01 00:00:00"),
               color="#f5bd2f")+
    facet_wrap(~intersection)+
    scale_color_manual(values=c("TRUE"="#d1faff", "FALSE"="#57a773"),
                       labels=c("Avant I4", "Après I4"))+
    ggtitle("Impact de la création de I4 sur la circulation des autres intersections")+
    labs(x="Date", y="Nombre de véhicules", color="Période")+
    theme_dark()+
    theme(plot.title=element_text(hjust=0.5))

# Coefficients directeur des droites de regression de I1 avant et apres la
# mise en place de I4

lm(data=intersec[which(intersec$intersection=="I1" & !intersec$post_cassure), ],
   vehicule1~index)$coefficients

lm(data=intersec[which(intersec$intersection=="I1" & intersec$post_cassure), ],
   vehicule1~index)$coefficients

# Coefficients directeur des droites de regression de I2 avant et apres la
# mise en place de I4

lm(data=intersec[which(intersec$intersection=="I2" & !intersec$post_cassure), ],
   vehicule1~index)$coefficients

lm(data=intersec[which(intersec$intersection=="I2" & intersec$post_cassure), ],
   vehicule1~index)$coefficients

# Coefficients directeur des droites de regression de I3 avant et apres la
# mise en place de I4

lm(data=intersec[which(intersec$intersection=="I3" & !intersec$post_cassure), ],
   vehicule1~index)$coefficients

lm(data=intersec[which(intersec$intersection=="I3" & intersec$post_cassure), ],
   vehicule1~index)$coefficients

# Il y a une grosse augmentation sur I2 et une legere diminution sur I3.

# Tests de Chow, les p-valeurs sont tres proches de 0,
# avec un risque de 5 %, le 1er janvier 2017 est une cassure

chow.test(common_list$I1$vehicule[!common_list$I1$post_cassure],
          common_list$I1$date[!common_list$I1$post_cassure],
          common_list$I1$vehicule[common_list$I1$post_cassure],
          common_list$I1$date[common_list$I1$post_cassure])

chow.test(common_list$I2$vehicule[!common_list$I2$post_cassure],
          common_list$I2$date[!common_list$I2$post_cassure],
          common_list$I2$vehicule[common_list$I2$post_cassure],
          common_list$I2$date[common_list$I2$post_cassure])

chow.test(common_list$I3$vehicule[!common_list$I3$post_cassure],
          common_list$I3$date[!common_list$I3$post_cassure],
          common_list$I3$vehicule[common_list$I3$post_cassure],
          common_list$I3$date[common_list$I3$post_cassure])

# Ajustement du modele multiplicatif, periode de 24 et detection d'anomalies
# avec la methode des quantiles


common_list$I1$vehicule_ts <- ts(common_list$I1$vehicule, frequency=24)
common_list$I2$vehicule_ts <- ts(common_list$I2$vehicule, frequency=24)
common_list$I3$vehicule_ts <- ts(common_list$I3$vehicule, frequency=24)
common_list$I4$vehicule_ts <- ts(common_list$I4$vehicule, frequency=24)

res_decompose <- list()
res_decompose$I1 <- decompose(common_list$I1$vehicule_ts, type="multiplicative")
res_decompose$I2 <- decompose(common_list$I2$vehicule_ts, type="multiplicative")
res_decompose$I3 <- decompose(common_list$I3$vehicule_ts, type="multiplicative")
res_decompose$I4 <- decompose(common_list$I4$vehicule_ts, type="multiplicative")

common_list$I1$modele_multiplicatif <- round(as.numeric(res_decompose$I1$trend*res_decompose$I1$seasonal))
common_list$I2$modele_multiplicatif <- round(as.numeric(res_decompose$I2$trend*res_decompose$I2$seasonal))
common_list$I3$modele_multiplicatif <- round(as.numeric(res_decompose$I3$trend*res_decompose$I3$seasonal))
common_list$I4$modele_multiplicatif <- round(as.numeric(res_decompose$I4$trend*res_decompose$I4$seasonal))

common_list$I1$residus <- common_list$I1$modele_multiplicatif - common_list$I1$vehicule
common_list$I2$residus <- common_list$I2$modele_multiplicatif - common_list$I2$vehicule
common_list$I3$residus <- common_list$I3$modele_multiplicatif - common_list$I3$vehicule

# MSE plus eleve pour I3 car la serie semble plus chaotique

mean(common_list$I1$residus^2, na.rm=TRUE)
mean(common_list$I2$residus^2, na.rm=TRUE)
mean(common_list$I3$residus^2, na.rm=TRUE)

common_list$I1$anomalie <- common_list$I1$residus %>%
    cut(labels=c("TRUE", "FALSE", "TRUE"),
        breaks=quantile(common_list$I1$residus, c(0, 0.05, 0.95, 1), na.rm=TRUE),
        include.lowest=TRUE) %>%
    as.logical()

common_list$I2$anomalie <- common_list$I2$residus %>%
    cut(labels=c("TRUE", "FALSE", "TRUE"),
        breaks=quantile(common_list$I2$residus, c(0, 0.05, 0.95, 1), na.rm=TRUE),
        include.lowest=TRUE) %>%
    as.logical()

common_list$I3$anomalie <- common_list$I3$residus %>%
    cut(labels=c("TRUE", "FALSE", "TRUE"),
        breaks=quantile(common_list$I3$residus, c(0, 0.05, 0.95, 1), na.rm=TRUE),
        include.lowest=TRUE) %>%
    as.logical()

# Pour I1, plus d'anomalies apres la mise en place de I4

length(which(common_list$I1$anomalie & !common_list$I1$post_cassure))/length(which(common_list$I1$anomalie))
length(which(common_list$I1$anomalie & common_list$I1$post_cassure))/length(which(common_list$I1$anomalie))

# Pour I2, plus d'anomalies apres la mise en place de I4

length(which(common_list$I2$anomalie & !common_list$I2$post_cassure))/length(which(common_list$I2$anomalie))
length(which(common_list$I2$anomalie & common_list$I2$post_cassure))/length(which(common_list$I2$anomalie))

# Pour I3, plus d'anomalies avant la mise en place de I4

length(which(common_list$I3$anomalie & !common_list$I3$post_cassure))/length(which(common_list$I3$anomalie))
length(which(common_list$I3$anomalie & common_list$I3$post_cassure))/length(which(common_list$I3$anomalie))

# On voit bien que le modele decrit bien les donnees car les residus sont
# impredictibles.

plot(common_list$I1$date, common_list$I1$residus,
     col=as.factor(common_list$I1$anomalie), pch=16)

plot(common_list$I2$date, common_list$I2$residus,
     col=as.factor(common_list$I2$anomalie), pch=16)

plot(common_list$I3$date, common_list$I3$residus,
     col=as.factor(common_list$I3$anomalie), pch=16)

# Pourcentage de personne allant de E_i a I_i

intersec$pourcentage_E <- NA

intersec[intersec$intersection=="I1", 7] <- common_list$I1$E / common_list$I1$vehicule * 100
intersec[intersec$intersection=="I2", 7] <- common_list$I2$E / common_list$I2$vehicule * 100
intersec[intersec$intersection=="I3", 7] <- common_list$I3$E / common_list$I3$vehicule * 100

ggplot(intersec[which(intersec$intersection!="I4"), ])+
    aes(date, pourcentage_E)+
    geom_smooth(aes(date, pourcentage_E, color=post_cassure))+
    geom_vline(xintercept=lubridate::ymd_hms("2017-01-01 00:00:00"),
               color="#f5bd2f")+
    facet_wrap(~intersection)+
    scale_color_manual(values=c("TRUE"="#d1faff", "FALSE"="#57a773"),
                       labels=c("Avant I4", "Après I4"))+
    ggtitle("Impact de I4 sur le pourcentage de personnes se rendant en ville")+
    labs(x="Date", y="Pourcentage de personnes allant de Ii à Ei", color="Période")+
    theme_dark()+
    theme(plot.title=element_text(hjust=0.5))

# Previsions par jour

(forecast(common_list$I1$vehicule_ts)$fitted[1:24] *
    res_decompose$I1$seasonal[1:24]) %>%
    sum() %>%
    round()

(forecast(common_list$I2$vehicule_ts)$fitted[1:24] *
    res_decompose$I2$seasonal[1:24]) %>%
    sum() %>%
    round()

(forecast(common_list$I3$vehicule_ts)$fitted[1:24] *
    res_decompose$I3$seasonal[1:24]) %>%
    sum() %>%
    round()

(forecast(common_list$I4$vehicule_ts)$fitted[1:24] *
    res_decompose$I4$seasonal[1:24]) %>%
    sum() %>%
    round()

# Mois

(forecast(common_list$I1$vehicule_ts)$fitted[1:720] *
    res_decompose$I1$seasonal[1:720]) %>%
    sum() %>%
    round()

(forecast(common_list$I2$vehicule_ts)$fitted[1:720] *
    res_decompose$I2$seasonal[1:720]) %>%
    sum() %>%
    round()

(forecast(common_list$I3$vehicule_ts)$fitted[1:720] *
    res_decompose$I3$seasonal[1:720]) %>%
    sum() %>%
    round()

(forecast(common_list$I4$vehicule_ts)$fitted[1:720] *
    res_decompose$I4$seasonal[1:720]) %>%
    sum() %>%
    round()
