read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")


ex1<-read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")
head(ex1)
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                            ROAD.x_i4,data=ex1)))
boxplot(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                       ROAD.x_i4,data=ex1)))
qqnorm(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                      ROAD.x_i4,data=ex1)))
qqline(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                      ROAD.x_i4,data=ex1)))
step(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
          ROAD.x_i4,data=ex1))
step(lm(FUEL.x_i5~1,data=ex1),scope=FUEL.x_i5~
       TAX.x_i1+DLIC.x_i2+INC.x_i13+ROAD.x_i4)

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






















ex1<-read.csv("http://irma.math.unistra.fr/~fbertran/enseignement/Master1_2016_2/Master2TD3_Ex1.CSV")
head(ex1)
ex1<-ex1[-40,]
shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                            ROAD.x_i4,data=ex1)))
boxplot(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                       ROAD.x_i4,data=ex1)))
qqnorm(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                      ROAD.x_i4,data=ex1)))
qqline(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
                      ROAD.x_i4,data=ex1)))
step(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13+
          ROAD.x_i4,data=ex1))
step(lm(FUEL.x_i5~1,data=ex1),scope=FUEL.x_i5~
       TAX.x_i1+DLIC.x_i2+INC.x_i13+ROAD.x_i4)

shapiro.test(residuals(lm(FUEL.x_i5~TAX.x_i1+DLIC.x_i2+INC.x_i13,data=ex1)))
