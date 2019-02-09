if(!require("ez")){install.packages("ez")}
library(ez)
options(contrasts = c("contr.sum", "contr.poly"))


relaxo=data.frame(matrix(c(
1,21,22,8,6,6,
2,20,19,10,4,4,
3,17,15,5,4,5,
4,25,30,13,12,17,
5,30,27,13,8,6,
6,19,27,8,7,4,
7,26,16,5,2,5,
8,17,18,8,1,5,
9,26,24,14,8,9),nrow=9,byrow=TRUE))
colnames(relaxo)<-c("Sujet","Sem.1","Sem.2","Sem.3","Sem.4","Sem.5")
relaxo$Sujet <- factor(relaxo$Sujet)
relaxo

library(reshape)
relaxo.long = reshape(relaxo, idvar = "Sujet", varying = list(2:6), v.names = "Durée", direction = "long", timevar = "Semaine", times=c("S1","S2","S3","S4","S5"))
str(relaxo.long)
relaxo.long$Semaine <- factor(relaxo.long$Semaine)
str(relaxo.long)

library(lattice)
with(relaxo.long,xyplot(Durée~Semaine|Sujet))
with(relaxo.long,xyplot(Durée~Semaine,group=Sujet,type="l"))

ezStats(data = relaxo.long, dv = Durée, within = Semaine, wid = Sujet)
ezPlot(data = relaxo.long, dv = Durée, within = Semaine, wid = Sujet, x=.(Semaine))

mod.cours <- ezANOVA(data = relaxo.long, dv = Durée, within = Semaine, wid = Sujet, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod.cours.res <- dae::residuals.aovlist(mod.cours$aov)
shapiro.test(mod.cours.res)
qqnorm(mod.cours.res)
qqline(mod.cours.res)
with(relaxo.long,xyplot(mod.cours.res~Semaine|Sujet))

mod.cours

#Idem à ce que l'on a avec aov
#summary(aov(Durée~Semaine+Sujet,data=relaxo.long))

if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emm <- emmeans(mod.cours$aov, ~ Semaine)
emm

pairs(emm, adjust = "Holm")
plot(pairs(emm, adjust = "Holm"))#not usable to sig diff iff IC overlaps 0
pairs(emm, adjust = "Tukey")
plot(pairs(emm, adjust = "Tukey"))
contrast(emm,  "tukey")
contrast(emm,  "dunnett", ref="S2")



# custom contrasts
contrast(
  emm, 
  list(c1 = c(1/2, 1/2, -1/3, -1/3, -1/3))
)









ezPerm(data = relaxo.long, dv = Durée, within = Semaine, wid = Sujet)

#IC boostrap
anovaboot = ezBoot(data = relaxo.long, dv = Durée, within = Semaine, wid = Sujet, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Semaine))









#Une étude de la relation entre la dose d’un médicament augmentant la pression
#sanguine et l’augmentation moyenne observée de la pression sanguine diastolique
#a été menée de la manière suivante : douze lapins ont reçu, dans un ordre aléatoire,
#les six différentes doses du médicament, l’intervalle entre chacune de ces prises
#étant suffisamment important pour que le lapin ne soit plus sous l’effet de la dose
#précédente.

lapins=data.frame(matrix(c(
1,21,21,23,35,36,48,
2,19,24,27,36,36,46,
3,12,25,27,26,33,40,
4,9,17,18,27,34,39,
5,7,10,19,25,31,38,
6,18,26,26,29,39,44,
7,9,12,17,22,33,40,
8,20,20,30,30,38,41,
9,18,18,27,31,42,49,
10,8,12,11,24,26,31,
11,18,22,25,32,38,38,
12,17,23,26,28,34,35),nrow=12,byrow=TRUE))
colnames(lapins)<-c("Lapin",paste("Dose",c(0.1,0.3,0.5,1.0,1.5,3.0),sep="."))
lapins$Lapin <- factor(lapins$Lapin)
lapins

library(reshape)
lapins.long = reshape(lapins, idvar = "Lapin", varying = list(2:7), v.names = "Pression", direction = "long", timevar = "Dose", times=c(0.1,0.3,0.5,1.0,1.5,3.0))

str(lapins.long)

library(lattice)
with(lapins.long,xyplot(Pression~Dose|Lapin))
with(lapins.long,xyplot(Pression~Dose,group=Lapin,type="l"))

lapins.long$Dose <- as.factor(lapins.long$Dose)
str(lapins.long)
with(lapins.long,xyplot(Pression~Dose|Lapin))
with(lapins.long,xyplot(Pression~Dose,group=Lapin,type="l"))

ezStats(data = lapins.long, dv = Pression, within = Dose, wid = Lapin)
ezPlot(data = lapins.long, dv = Pression, within = Dose, wid = Lapin, x=.(Dose))

mod1 <- ezANOVA(data = lapins.long, dv = Pression, within = Dose, wid = Lapin, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod1.res <- dae::residuals.aovlist(mod1$aov)
shapiro.test(mod1.res)
with(lapins.long,xyplot(mod1.res~Dose|Lapin))

mod1

#Idem à ce que l'on a avec aov
#summary(aov(Pression~Dose+Lapin,data=lapins.long))

if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emm <- emmeans(mod1$aov, ~ Dose)
emm

pairs(emm, adjust = "Holm")
pairs(emm, adjust = "Tukey")
plot(pairs(emm, adjust = "Tukey"))
contrast(emm,  "tukey")
plot(contrast(emm,  "dunnett", ref="0.1"))




ezPerm(data = lapins.long, dv = Pression, within = Dose, wid = Lapin)

#IC boostrap
anovaboot = ezBoot(data = lapins.long, dv = Pression, within = Dose, wid = Lapin, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Dose))








#Dans une expérience sur l’esthétique, on demandait à chaque participant de produire
#trois dessins utilisant juste un de trois matériaux différents pour chacun des
#dessins : Crayon, Pinceau ou Feutre. La variable dépendante était la note obtenue
#après l’évaluation du dessin par un panel de juges. Cette variable est considérée
#comme quantitative continue. La variable indépendante était le type d’instrument
#utilisé pour produire le dessin. Étant donné que les participants varieraient certainement
#par leur qualité artistique, il a été décidé de demander à chacun de produire
#trois dessins, un avec chaque type d’instrument. Pour essayer de neutraliser les effets
#d’apprentissage, l’ordre des instruments a été randomisé pour chacun des participants.
#30 dessins ont été réalisés dans des conditions similaires sur 10 individus distincts.
#Leurs résultats, exprimés en unités arbitraires, ont été reproduits dans le tableau
#ci-dessous.

Notes=data.frame(matrix(c(
1,12.71,14.89,12.21,
2,18.94,16.35,12.06,
3,20.14,16.30,15.99,
4,12.00,12.41,10.36,
5,19.34,21.91,20.39,
6,25.78,20.17,22.83,
7,18.98,17.45,16.34,
8,22.21,18.31,18.58,
9,17.96,12.53,14.04,
10,23.81,18.88,20.34
),nrow=10,byrow=TRUE))
colnames(Notes)<-c("Participant","Crayon","Feutre","Pinceau")
Notes$Participant <- factor(Notes$Participant)
Notes
library(reshape)
Notes.long = reshape(Notes, idvar = "Participant", varying = list(2:4), v.names = "Note", direction = "long", timevar = "Instrument", times=c("Crayon","Feutre","Pinceau"))
Notes.long
str(Notes.long)
Notes.long$Instrument <- as.factor(Notes.long$Instrument)
str(Notes.long)

library(lattice)
with(Notes.long,xyplot(Note~Instrument|Participant))
with(Notes.long,xyplot(Note~Instrument,group=Participant,type="l"))


ezStats(data =Notes.long, dv = Note, within = Instrument, wid = Participant)
ezPlot(data =Notes.long, dv = Note, within = Instrument, wid = Participant, x=.(Instrument))

mod2 <- ezANOVA(data =Notes.long, dv = Note, within = Instrument, wid = Participant, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod2.res <- dae::residuals.aovlist(mod2$aov)
shapiro.test(mod2.res)
with(Notes.long,xyplot(mod2.res~Instrument|Participant))

mod2

if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emm <- emmeans(mod2$aov, ~ Instrument)
emm

pairs(emm, adjust = "Holm")
pairs(emm, adjust = "Tukey")
plot(pairs(emm, adjust = "Tukey"))
contrast(emm,  "tukey")
contrast(emm,  "dunnett", ref="Pinceau")


ezPerm(data =Notes.long, dv = Note, within = Instrument, wid = Participant)

#IC boostrap
anovaboot = ezBoot(data =Notes.long, dv = Note, within = Instrument, wid = Participant, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Instrument))












#Des chercheurs ont souhaité evaluer l’effet de l’alcool sur les erreurs de conduite. Ils
#ont fait conduire cinq sujets, à plusieurs reprises et pour trois quantités différentes
#d’alcool présentes dans le sang (0,04 ; 0,06 et 0,08 en mg/l). Ces périodes d’essai
#duraient 15 minutes et se déroulaient dans un simulateur.
#Les données obtenues ont été reproduites dans le tableau ci-dessous.


Conduite.long=data.frame(matrix(c(
  1,0.04,14,
  2,0.04,16,
  3,0.04,19,
  4,0.04,13,
  5,0.04,11,
  1,0.06,17,
  2,0.06,19,
  3,0.06,21,
  4,0.06,23,
  5,0.06,21,
  1,0.08,25,
  2,0.08,29,
  3,0.08,24,
  4,0.08,21,
  5,0.08,22
),nrow=15,byrow=TRUE))
colnames(Conduite.long)<-c("Sujet","Qte_alc_sang","Erreurs")
Conduite.long$Sujet <- factor(Conduite.long$Sujet)
Conduite.long
str(Conduite.long)

library(lattice)
with(Conduite.long,xyplot(Erreurs~Qte_alc_sang|Sujet))
with(Conduite.long,xyplot(Erreurs~Qte_alc_sang,group=Sujet,type="l"))

Conduite.long$Qte_alc_sang <- as.factor(Conduite.long$Qte_alc_sang)
str(Conduite.long)
with(Conduite.long,xyplot(Erreurs~Qte_alc_sang|Sujet))
with(Conduite.long,xyplot(Erreurs~Qte_alc_sang,group=Sujet,type="l"))


ezStats(data =Conduite.long, dv = Erreurs, within = Qte_alc_sang, wid = Sujet)
ezPlot(data =Conduite.long, dv = Erreurs, within = Qte_alc_sang, wid = Sujet, x=.(Qte_alc_sang))

mod3 <- ezANOVA(data =Conduite.long, dv = Erreurs, within = Qte_alc_sang, wid = Sujet, return_aov = TRUE)
if(!require("dae")){install.packages("dae")}
mod3.res <- dae::residuals.aovlist(mod3$aov)
shapiro.test(mod3.res)
with(Conduite.long,xyplot(mod3.res~Qte_alc_sang|Sujet))

mod3

if(!require("emmeans")){install.packages("emmeans",type="binary")}
library(emmeans)
emm <- emmeans(mod3$aov, ~ Qte_alc_sang)
emm

pairs(emm, adjust = "Holm")
pairs(emm, adjust = "Tukey")
plot(pairs(emm, adjust = "Tukey"))
contrast(emm,  "tukey")
contrast(emm,  "dunnett", ref="0.08")




ezPerm(data =Conduite.long, dv = Erreurs, within = Qte_alc_sang, wid = Sujet)

#IC boostrap
anovaboot = ezBoot(data =Conduite.long, dv = Erreurs, within = Qte_alc_sang, wid = Sujet, resample_within = FALSE)
ezPlot2(anovaboot, x=.(Qte_alc_sang))














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
mod.cours2.res <- dae::residuals.aovlist(mod4$aov)
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



































