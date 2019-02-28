library(readr)
Master2TD3_Ex1 <- read_csv("~/Downloads/Master2TD3_Ex1.CSV")

head(Master2TD3_Ex1)
tail(Master2TD3_Ex1)
str(Master2TD3_Ex1)

`FUEL x_i5`

lm(`FUEL x_i5`~.-Etat,data = Master2TD3_Ex1)

colnames(Master2TD3_Ex1)
colnames(Master2TD3_Ex1) <- c("Etat", "Tax", "DLIC", "INC", "ROAD", "FUEL")

head(Master2TD3_Ex1)

lm(FUEL~.-Etat,data = Master2TD3_Ex1)

modcomplet <- lm(FUEL~.-Etat,data = Master2TD3_Ex1)
qqnorm(residuals(modcomplet))
qqline(residuals(modcomplet))
shapiro.test(residuals(modcomplet))

step(modcomplet, scope=~., direction = "both")

mod2complet <- lm(FUEL~(.-Etat)^2,data = Master2TD3_Ex1)
mod2complet <- lm(FUEL~Tax+DLIC+INC+ROAD+Tax:DLIC+Tax:INC+Tax:ROAD+DLIC:INC+DLIC:ROAD+INC:ROAD,data = Master2TD3_Ex1)
qqnorm(residuals(mod2complet))
qqline(residuals(mod2complet))
shapiro.test(residuals(mod2complet))
summary(mod2complet)

mod2completplus <- lm(FUEL~(.-Etat)^2+I(Tax^2)+I(DLIC^2)+I(INC^2)+I(ROAD^2),data = Master2TD3_Ex1)
qqnorm(residuals(mod2completplus))
qqline(residuals(mod2completplus))
shapiro.test(residuals(mod2completplus))
summary(mod2completplus)

pairs(Master2TD3_Ex1[,-1])

if(!require(GGally)){install.packages("GGally")}
library(GGally)
ggpairs(data = Master2TD3_Ex1[,-1])

step(mod2completplus, scope=~., direction = "both")
modelefinal <- step(mod2completplus, scope=~., direction = "both")
qqnorm(residuals(modelefinal))
qqline(residuals(modelefinal))
shapiro.test(residuals(modelefinal))
summary(modelefinal)
#Nous retenons au final 10 variables.

#Peut servir pour plus de modèles que la fonctions step classique. Sinon fait exactement la même chose.
MASS::stepAIC(mod2completplus, scope=~., direction = "both")

if(!require(leaps)){install.packages("leaps")}
library(leaps)
exhaust <- regsubsets(FUEL~(.-Etat)^2+I(Tax^2)+I(DLIC^2)+I(INC^2)+I(ROAD^2),data=Master2TD3_Ex1,nvmax=14)
#Par défaut nvmax=8 -> pas de modèle avec plus de 8 variables
exhaust
summary(exhaust)

summary(exhaust)$adjr2
which.max(summary(exhaust)$adjr2)
#Un modèle à 9 variables est le meileur au sens du R^2 ajusté.
(summary(exhaust)$which)[9,]

summary(exhaust)$cp
#Comparons le C_p au nombre de termes dans le modèle +1
summary(exhaust)$cp-2:15
#L'écart est toujours nul pour le modèle complet : c'est le 0 à la fin de la sortie. Il faut trouver la valeur la plus proche de 0 à l'exception de celle-ci. Elle se trouve à la 4ème position.
which.min(abs((summary(exhaust)$cp-2:15)[-14]))
#Au sens du C_p le meilleur modèle est celui à quatre variables
(summary(exhaust)$which)[4,]


summary(exhaust)$bic
which.min(summary(exhaust)$bic)
#Au sens du BIC le meilleur modèle est celui à quatre variables
(summary(exhaust)$which)[4,]


#Le modèle sélectionné par élminination descendante stepwise comportait 10 variables
modelefinal
(summary(exhaust)$which)[10,]

#C'est (heureusement) le meilleur modèle à 10 variables.
coef(modelefinal)
coef(exhaust,10)


names(coef(modelefinal))==names((summary(exhaust)$which)[10,])[(summary(exhaust)$which)[10,]]
