Exemple=read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/DonneesExempleCours3.csv")
head(Exemple)
tail(Exemple)
str(Exemple)
Exemple$Fire
log(Exemple$Fire) #commentaire
Exemple$lnFire <- log(Exemple$Fire) # Nouvelle colonne lnFire
# dans Exemple qui contient les logarithmes
# de la colonne Fire
head(Exemple)
str(Exemple)
View(exemple)
rm(exemple)
lm(lnFire ~ X_1+X_2+X_3,data=Exemple)
summary(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
summary(lm(lnFire ~ X_2+X_3,data=Exemple))
summary(lm(lnFire ~ X_1,data=Exemple))
summary(lm(lnFire ~ X_2,data=Exemple))
summary(lm(lnFire ~ X_3,data=Exemple))
summary(lm(lnFire ~ X_1+X_2,data=Exemple))
summary(lm(lnFire ~ X_1+X_3,data=Exemple))
cor(Exemple)
summary(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
summary(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
step(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
?step
step(lm(lnFire ~ X_1+X_2+X_3,data=Exemple),direction = "forward")
step(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
step(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
step(lm(lnFire ~ X_1+X_2+X_3,data=Exemple),
     direction = "forward")
step(lm(lnFire ~ 1,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ 1,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ 1,scope~X_1+X_2+X_3,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ 1,scope=~X_1+X_2+X_3,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ X_1+X_2+X_3,scope=~X_1+X_2+X_3,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ X_1+X_2+X_3,scope=~1,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ X_1+X_2+X_3,scope=lnFire~1,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ X_1+X_2+X_3,
        scope=lnFire~X_1+X_2+X_3,data=Exemple),
     direction = "forward")
add1(lm(lnFire ~ X_1+X_2+X_3,scope=lnFire~1,data=Exemple))
add1(lm(lnFire ~ X_1+X_2+X_3,data=Exemple),scope=lnFire~X_1+X_2+X_3,
      direction = "forward")
add1(lm(lnFire ~ 1,data=Exemple),scope=lnFire~X_1+X_2+X_3,
    direction = "forward")
step(lm(lnFire ~ 1,data=Exemple)
    ,scope=lnFire~X_1+X_2+X_3,
    direction = "forward")
step(lm(lnFire ~ 1,data=Exemple)
    ,scope=lnFire~X_1+X_2+X_3)
step(lm(lnFire ~ X_1+X_2+X_3,data=Exemple))
read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")
read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")
ex1<-read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")
ex1<-read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")
head(ex1)
step(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
         ROAD.x_i4,data=ex1))
step(lm(FUEL.x_i5~1,data=ex1),scope=FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
      ROAD.x_i4)
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                           ROAD.x_i4,data=ex1)))
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13,data=ex1)))
ex1$lnF<-log(ex1$FUEL.x_i5)
head(ex1)
step(lm(lnF~TAX.x_i1+DLIC.x_i2+INC.x_i13+
         ROAD.x_i4,data=ex1))
step(lm(lnF~1,data=ex1),scope=lnF~
      TAX.x_i1+DLIC.x_i2+INC.x_i13+ROAD.x_i4)
shapiro.test(residuals(lm(lnF~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                           ROAD.x_i4,data=ex1)))
shapiro.test(residuals(lm(lnF~TAX.x_i1+DLIC.x_i2+INC.x_i13,data=ex1)))
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                           ROAD.x_i4,data=ex1)))
step(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
         ROAD.x_i4,data=ex1))
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                           ROAD.x_i4,data=ex1)))
boxplot(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                      ROAD.x_i4,data=ex1)))
qqnorm(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                     ROAD.x_i4,data=ex1)))
qqline(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                     ROAD.x_i4,data=ex1)))
lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
    ROAD.x_i4,data=ex1)
lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
     ROAD.x_i4,data=ex1)
residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
               ROAD.x_i4,data=ex1))
boxplot(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                       ROAD.x_i4,data=ex1)))
ex1
ex1[-40,]
ex1<-ex1[-40,]
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                            ROAD.x_i4,data=ex1)))
step(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
          ROAD.x_i4,data=ex1))
step(lm(FUEL.x_i5~1,data=ex1),scope=FUEL.x_i5~
       TAX.x_i1+DLIC.x_i2+INC.x_i13+ROAD.x_i4)
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13,data=ex1)))