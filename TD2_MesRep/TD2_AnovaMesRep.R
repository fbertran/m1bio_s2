Promotion=data.frame(matrix(
  c(
    1,1,958,1047,933,
    1,2,1005,1122,986,
    1,3,351,436,339,
    1,4,549,632,512,
    1,5,730,784,707,
    2,1,780,897,718,
    2,2,229,275,202,
    2,3,883,964,817,
    2,4,624,695,599,
    2,5,375,436,351
  ),nrow=10,byrow=TRUE))
colnames(Promotion) <- c("Campagne", "Supermarché", "Avant", "Pendant", "Après")
Promotion
Promotion$Campagne <- as.factor(Promotion$Campagne)
Promotion$Supermarché <- as.factor(Promotion$Supermarché)
str(Promotion)

library(reshape)
Promotion.long = reshape(Promotion, idvar = c("Campagne", "Supermarché"), varying = list(3:5), v.names = "Ventes", direction = "long", timevar = "Période", times=c("Avant","Pendant","Après"))
Promotion.long
Promotion.long$Période <- factor(Promotion.long$Période,levels =c("Avant","Pendant","Après"))
str(Promotion.long)

library(lattice)
with(Promotion.long,xyplot(Ventes~Période|Campagne,group=Supermarché,type="l"))

ezStats(data =Promotion.long, dv = Ventes, within = Période, between = Campagne, wid = Supermarché)
ezPlot(data =Promotion.long, dv = Ventes, within = Période, between = Campagne, wid = Supermarché, x=.(Période), split=.(Campagne))

mod.cours2 <- ezANOVA(data =Promotion.long, dv = Ventes, within = Période, between = Campagne, wid = Supermarché, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod.cours2.res <- dae::residuals.aovlist(mod.cours2$aov)
shapiro.test(mod.cours2.res)
with(Promotion.long,xyplot(mod.cours2.res~Période|Campagne,group=Supermarché))

mod.cours2


if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emm <- emmeans(mod.cours2$aov, ~ Période)
emm

pairs(emm, adjust = "Holm")
pairs(emm, adjust = "Tukey")
plot(pairs(emm, adjust = "Tukey"))
contrast(emm,  "tukey")
contrast(emm,  "dunnett", ref="Avant")








ezPerm(data =Promotion.long, dv = Ventes, within = Période, between = Campagne, wid = Supermarché)

#IC boostrap
anovaboot = ezBoot(data =Promotion.long, dv = Ventes, within = Période, between = Campagne, wid = Supermarché, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Période), split=.(Campagne))




#Une étude expérimentale a été réalisée afin d’étudier un éventuel effet d’une différence
#en matière de présentation sur un rayonnage d’un produit d’entretien ménager.
#Huit magasins ont été choisis au hasard et répartis, également au hasard, en deux
#groupes de quatre. Les ventes du produit d’entretien ménager ont été relevées simultanément
#dans chacun de ces huit magasins à quatre reprises.

Presentation=data.frame(matrix(
  c(
    1,1,956,953,938,1049,
    1,2,1008,1032,1025,1123,
    1,3,350,352,338,438,
    1,4,412,449,385,532,
    2,5,769,766,739,859,
    2,6,880,875,860,915,
    2,7,176,185,168,280,
    2,8,209,223,217,301),nrow=8,byrow=TRUE))
colnames(Presentation) <- c("Type_de_presentation","Magasin","Relevé.1","Relevé.2","Relevé.3","Relevé.4")
Presentation
Presentation$Type_de_presentation <- as.factor(Presentation$Type_de_presentation)
Presentation$Magasin <- as.factor(Presentation$Magasin)
str(Presentation)

library(reshape)
Presentation.long = reshape(Presentation, idvar = c("Type_de_presentation", "Magasin"), varying = list(3:6), v.names = "Ventes", direction = "long", timevar = "Relevé", times=c(1,2,3,4))
Presentation.long
Presentation.long$Relevé <- as.factor(Presentation.long$Relevé)
str(Presentation.long)

library(lattice)
with(Presentation.long,xyplot(Ventes~Relevé|Type_de_presentation,group=Magasin,type="l"))

ezStats(data =Presentation.long, dv = Ventes, within = Relevé, between = Type_de_presentation, wid = Magasin)
ezPlot(data =Presentation.long, dv = Ventes, within = Relevé, between = Type_de_presentation, wid = Magasin, x=.(Relevé), split=.(Type_de_presentation))

mod4 <- ezANOVA(data =Presentation.long, dv = Ventes, within = Relevé, between = Type_de_presentation, wid = Magasin, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod4.res <- dae::residuals.aovlist(mod4$aov)
shapiro.test(mod4.res)
with(Presentation.long,xyplot(mod4.res~Relevé|Type_de_presentation,group=Magasin))

mod4

if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emm <- emmeans(mod4$aov, ~ Relevé)
emm

pairs(emm, adjust = "Holm")
pairs(emm, adjust = "Tukey")
plot(pairs(emm, adjust = "Tukey"))
contrast(emm,  "tukey")
contrast(emm,  "dunnett", ref="1")




ezPerm(data =Presentation.long, dv = Ventes, within = Relevé, between = Type_de_presentation, wid = Magasin)

#IC boostrap
anovaboot = ezBoot(data =Presentation.long, dv = Ventes, within = Relevé, between = Type_de_presentation, wid = Magasin, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Relevé), split=.(Type_de_presentation))







EXERTYPE DIET INDIV PULSE EXERTYPE DIET INDIV PULSE EXERTYPE DIET INDIV PULSE
1 1 1 112 2 1 1 166 3 1 1 215
1 1 2 111 2 1 2 166 3 1 2 225
1 1 3 89 2 1 3 132 3 1 3 189
1 2 4 95 2 2 4 134 3 2 4 186
1 2 5 66 2 2 5 109 3 2 5 150
1 2 6 69 2 2 6 119 3 2 6 177
1 1 7 125 2 1 7 177 3 1 7 241
1 1 8 85 2 1 8 117 3 1 8 186
1 1 9 97 2 1 9 137 3 1 9 185
1 2 10 93 2 2 10 151 3 2 10 217
1 2 11 77 2 2 11 122 3 2 11 178
1 2 12 78 2 2 12 119 3 2 12 173
1 1 13 81 2 1 13 134 3 1 13 205
1 1 14 88 2 1 14 133 3 1 14 180
1 1 15 88 2 1 15 157 3 1 15 224
1 2 16 58 2 2 16 99 3 2 16 131
1 2 17 85 2 2 17 132 3 2 17 186
1 2 18 78 2 2 18 110 3 2 18 164







calcs=matrix(c(
  1,"Jones",3.1,7.5,2.5,5.1,
  2,"Williams",3.8,8.1,2.8,5.3,
  3,"Adams",3.0,7.6,2.0,4.9,
  4,"Dixon",3.4,7.8,2.7,5.5,
  5,"Erickson",3.3,6.9,2.5,5.4,
  6,"Maynes",3.6,7.8,2.4,4.8
),nrow=6,byrow=TRUE)
colnames(calcs) <- c("ind", "Sujet", "Temps.ps.nm", "Temps.ps.am", "Temps.pi.nm", "Temps.pi.am")
calcs<-as.data.frame(calcs)
calcs[[3]]<-as.numeric(as.character(calcs[[3]]))
calcs[[4]]<-as.numeric(as.character(calcs[[4]]))
calcs[[5]]<-as.numeric(as.character(calcs[[5]]))
calcs[[6]]<-as.numeric(as.character(calcs[[6]]))
str(calcs)

library(reshape)
calcs.long.temp = reshape(calcs, idvar = "Sujet", varying = list(3:6), v.names = "Temps", direction = "long", timevar = "Probleme:Modele", times=c("ProbStat.NouvMod","ProbStat.AncMod","ProbIngé.NouvMod","ProbIngé.AncMod"))
str(calcs.long.temp)
calcs.long<-cbind(calcs.long.temp,t(simplify2array(strsplit(calcs.long.temp$`Probleme:Modele`,".",fixed=TRUE))))
colnames(calcs.long)[5:6] <- c("Problème","Modèle")
str(calcs.long)
head(calcs.long)


library(lattice)
with(calcs.long,xyplot(Temps~Problème|Modèle,group=Sujet,type="l"))
with(calcs.long,xyplot(Temps~Modèle|Problème,group=Sujet,type="l"))

ezStats(data =calcs.long, dv = Temps, within = .(Problème,Modèle), between = , wid = Sujet)
ezPlot(data =calcs.long, dv = Temps, within = .(Problème,Modèle), between = , wid = Sujet, x=.(Modèle), split=.(Problème))

mod4 <- ezANOVA(data =calcs.long, dv = Temps, within = .(Problème,Modèle), between = , wid = Sujet, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod4.res <- dae::residuals.aovlist(mod4$aov)
shapiro.test(mod4.res)
with(calcs.long,xyplot(mod4.res~Problème|Modèle,group=Sujet))
with(calcs.long,xyplot(mod4.res~Modèle|Problème,group=Sujet))

#pas de normalité
#résultats de mod4 pas utilisables

resperm=ezPerm(data =calcs.long, dv = Temps, within = .(Problème,Modèle), between = , wid = Sujet)

if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emmip(mod4$aov, ~ Problème | Modèle)
pairs.emm_p.m <- emmeans(mod4$aov, pairwise ~ Problème | Modèle)
pairs.emm_p.m
plot(pairs.emm_p.m)

emmip(mod4$aov, ~ Modèle | Problème)
pairs.emm_m.p <- emmeans(mod4$aov, pairwise ~ Modèle | Problème)
pairs.emm_m.p
plot(pairs.emm_m.p)


#De manière similaire à ce que nous avons fait avant.
emm_p.m <- emmeans(mod4$aov, ~ Problème | Modèle)
emm_p.m

pairs(emm_p.m, adjust = "Holm")
pairs(emm_p.m, adjust = "Tukey")
plot(pairs(emm_p.m, adjust = "Tukey"))
contrast(emm,  "tukey")
contrast(emm,  "dunnett", ref=1)



#IC boostrap
anovaboot = ezBoot(data =calcs.long, dv = Temps, within = .(Problème,Modèle), between = , wid = Sujet, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Relevé), split=.(Type_de_presentation))










Chiffres Minuscule Majuscule
Sujet Gothique Roman Gothique Roman Gothique Roman
1 2 6 18 3 20 5
2 4 9 20 6 18 2
3 3 10 15 2 21 3
4 1 12 10 9 25 10
5 5 8 13 8 20 8
6 6 10 14 10 16 6



















