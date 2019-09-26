Exemple1<-matrix(c(21,2,19,13),byrow=T,nrow=2,dimnames=list(c("Traitement","Contrôle"),c("Tumeur présente","Tumeur absente")))
Exemple1
mosaicplot(Exemple1, type = "pearson", shade = T, las = 2, main = "Associations et résidus du test du chi2")
chisq.test(Exemple1,correct=FALSE)
chisq.test(Exemple1,correct=FALSE)$expected

file.choose()
Exemple1bis<- read.csv("/Users/fbertran/Downloads/MASTER2COURS5_EX1.CSV",header=T)
model<-glm(cbind(Tumeur,Total-Tumeur)~Groupe,data=Exemple1bis,family=binomial(link=logit))
print(model)
summary(model)
confint.default((model))
exp(confint.default((model)))
anova(model,test="Chisq")

Exemple2<- read.csv(file.choose(),header=TRUE,row.names=1)
Exemple2
mosaicplot(Exemple2, type = "pearson", shade = TRUE, las = 2, main = "Associations et résidus du test du chi2")
chisq.test(Exemple2)
chisq.test(Exemple2)$expected

Exemple2$Groupe <- factor(c("Deux","Un seul","Aucun"), levels=c("Un seul","Deux","Aucun"))
Exemple2
model<-glm(cbind(Enfant.fume,Enfant.ne.fume.pas)~Groupe,data=Exemple2,family=binomial(link=logit))
model
summary(model)
confint.default((model))
exp(confint.default((model)))
anova(model,test="Chisq")




Exemple3<- read.csv("https://raw.githubusercontent.com/fbertran/m1bio_s2/master/MASTER2COURS5_EX3.CSV",header=T)
Exemple3

#le log dans R designe le logarithme népérien
#= le logarithme traditionnel, comme en général
#en anglais.
Thetai_emp = with(Exemple3, log((N.morts+.5)/(Total-N.morts+.5)))
Thetai_emp
with(Exemple3, cor(Thetai_emp,Dose))
with(Exemple3, plot(Dose,Thetai_emp))
with(Exemple3, plot(log(Dose),Thetai_emp))
with(Exemple3, cor(Thetai_emp,log(Dose)))

#Créons le tableau réponse avec colonne (succès; échecs)
res_exp = with(Exemple3,cbind(N.morts,Total-N.morts))
res_exp

#Attention ne pas changer l'ordre des lignes dans Exemple3
model0<-glm(res_exp~Sexe*Dose,data=Exemple3,family=binomial(link=logit))
model0
#AIC: 56.27
#Residual Deviance: 18.16

model1<-glm(cbind(N.morts,Total-N.morts)~Sexe*Log.Dose.,data=Exemple3,family=binomial(link=logit))
model1
#AIC: 43.1
#Residual Deviance: 4.994 	

anova(model1,test="Chisq")
summary(model1)
confint.default((model1))
exp(confint.default((model1)))

model1bis<-glm(cbind(N.morts,Total-N.morts)~Sexe*I(log(Dose)),data=Exemple3,family=binomial(link=logit))
model1bis

model2<-glm(cbind(N.morts,Total-N.morts)~Sexe+Log.Dose.,data=Exemple3,family=binomial(link=logit))
model2
anova(model2,test="Chisq")
anova(model2,model1,test="Chisq")
summary(model2)
confint.default((model2))
exp(confint.default((model2)))
anova(model2)

step(model1)





