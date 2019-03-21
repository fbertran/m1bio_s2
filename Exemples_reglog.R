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


Exemple3<- read.csv(file.choose(),header=T)
Exemple3

model1<-glm(cbind(N.morts,Total-N.morts)~Sexe*Log.Dose.,data=Exemple3,family=binomial(link=logit))
model1
anova(model1,test="Chisq")
summary(model1)
confint.default((model1))
exp(confint.default((model1)))

model2<-glm(cbind(N.morts,Total-N.morts)~Sexe+Log.Dose.,data=Exemple3,family=binomial(link=logit))
model2
anova(model2,test="Chisq")
anova(model2,model1,test="Chisq")
summary(model2)
confint.default((model2))
exp(confint.default((model2)))
anova(model2)

step(model1)



