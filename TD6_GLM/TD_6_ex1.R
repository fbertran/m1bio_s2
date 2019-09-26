plasma = read.csv("https://raw.githubusercontent.com/fbertran/m1bio_s2/master/plasma.CSV")
head(plasma)
tail(plasma)
str(plasma)

plasma$ESR = as.factor(plasma$ESR)

dev.new()
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma, ylab = "ESR")
cdplot(ESR ~ globulin, data = plasma, ylab = "ESR")
layout(1)

plasma_glm_1 <- glm(ESR ~ fibrinogen, data = plasma, 
                    family = binomial())
summary(plasma_glm_1)
#le log de la côte de succès augmente de 1
#quand fibrinogen augmente de 1

confint(plasma_glm_1, parm = "fibrinogen")
#permet d'avoir une idée de la précision avec 
#laquelle la variation du log de la côte du succès 
#est connue. Dans ce cas, la précision n'est pas bonne.
confint(plasma_glm_1, parm = "fibrinogen")/coef(plasma_glm_1)["fibrinogen"]
#nous passons de 1/5 plus de 2 fois la valeurs estimées.

exp(coef(plasma_glm_1))["fibrinogen"]
exp(confint(plasma_glm_1, parm = "fibrinogen"))
#mêmes remarques pour la côte du succès

#essayons d'ajouter la globulin pour voir si le modèle
#est significativement amélioré
plasma_glm_2 <- glm(ESR ~ fibrinogen + globulin, 
                    data = plasma, family = binomial())
anova(plasma_glm_1, plasma_glm_2, test = "Chisq")

#nous considérons aussi le modèle avec interaction
plasma_glm_3 <- glm(ESR ~ fibrinogen*globulin, 
                    data = plasma, family = binomial())
#comparons le modèle avec l'interaction et les deux effets
#pricipaux (plasma_glm_3) avec le modèle comportant
#seulement les deux effets principaux (plasma_glm_2)
anova(plasma_glm_2, plasma_glm_3, test = "Chisq")
#comparons le modèle avec l'interaction et les deux effets
#pricipaux (plasma_glm_3) avec le modèle comportant
#seulement l'effet principal fibrinogen (plasma_glm_1)
anova(plasma_glm_1, plasma_glm_3, test = "Chisq")
#permet de réaliser les comparaisons
#plasma_glm_2 vs plasma_glm_3
#plasma_glm_1 vs plasma_glm_2
#mais pas plasma_glm_1 vs plasma_glm_3 qui doit être faite
#pour pouvoir comparer le modèle avec interaction avec le
#modèle avec effet principal de fibrinogen
anova(plasma_glm_1, plasma_glm_2, 
      plasma_glm_3, test = "Chisq")
plasma_glm_0 = glm(ESR ~ 1, data = plasma, 
                   family = binomial())
#test de l'effet fibrinogen basé sur le rapport 
#de vraisemblance (à privilégier)
anova(plasma_glm_0, plasma_glm_1, test="Chisq")
#test de l'effet fibrinogen basé sur le test de Wald
#(à eviter car moins bon que le test basé sur le rapport de
#vraisemblance)
summary(plasma_glm_1)

#nécessaire de vérifier l'adéquation entre le modèle
#et les observations

install.packages("rms")
library(rms)
lrm_plasma_1 <- lrm(ESR ~ fibrinogen, data = plasma,
                    x = TRUE, y = TRUE)
print(lrm_plasma_1)
#gof = goodness of fit = qualité de l'ajustement
residuals(lrm_plasma_1, "gof")
lrm_plasma_2 <- lrm(ESR ~ fibrinogen + globulin, 
                    data = plasma, x = TRUE, y = TRUE)
print(lrm_plasma_2)
residuals(lrm_plasma_2, "gof")

lrm_plasma_3 <- lrm(ESR ~ fibrinogen*globulin, 
                    data = plasma, x = TRUE, y = TRUE)
print(lrm_plasma_3)
residuals(lrm_plasma_3, "gof")

