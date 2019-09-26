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




























