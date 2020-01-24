Teneur<-c(4.3,4.7,4.3,4.7,4.9,4.2,4.2,4.4,4.1,4.0,5.0,4.9,4.8,4.9,5.0,4.5,4.8,4.5,4.6,4.6,4.4,4.1,4.3,4.1,4.2,4.9,4.9,4.9,5.0,5.0)
G=factor(rep(gl(3,5),2))
Ech=factor(rep(1:15,2))
Ech2=factor(rep(1:5,6))
ddd=cbind(data.frame(Teneur),G,Ech)

ttt=aov(Teneur~ G+ Ech%in% G,data=ddd)
lmmod=lm(Teneur~ G+ Ech%in% G,data=ddd)
resM=residuals(ttt)
shapiro.test(resM)
library(lattice)
xyplot(resM~Ech|G)
xyplot(resM~Ech2|G)

summary(aov(Teneur~ G+Ech%in% G,data=ddd))
#Df Sum Sq Mean Sq F value   Pr(>F)    
#G            2  2.669  1.3343 125.094 4.41e-10 ***
#  G:Ech       12  0.390  0.0325   3.047   0.0223 *  
#  Residuals   15  0.160  0.0107      

summary(aov(Teneur~ G+Error(Ech%in% G),data=ddd))
#Error: Ech:G
#Df Sum Sq Mean Sq F value  Pr(>F)    
#G          2  2.669  1.3343   41.06 4.3e-06 ***
#  Residuals 12  0.390  0.0325         

2.6687/2
0.3900/12
0.1600/15

2.6687/2/(0.3900/12)
0.3900/12/(0.1600/15)

qf(.95,2,12)

qf(.95,12,15)

summary(aov(Teneur~ G+Error(Ech%in% G),data=ddd))
summary(aov(Teneur~ G+Error(Ech),data=ddd))





