Exemple <- read.table(file.choose(),dec=".",sep=",",quote="\"",header=T)

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

summary(lm(lnFire~X_1,data=Exemple))
summary(lm(lnFire~X_2,data=Exemple))
summary(lm(lnFire~X_3,data=Exemple))

summary(lm(lnFire~X_1+X_2,data=Exemple))
summary(lm(lnFire~X_1+X_3,data=Exemple))
summary(lm(lnFire~X_2+X_3,data=Exemple))




n=nrow(Exemple)

out.big <- lm(lnFire ~ X_1+X_2+X_3,data=Exemple)
sigsqhat.big <- summary(out.big)$sigma^2

R2 <- double(length(8))
R2aj <- double(length(8))
cp <- double(length(8))
aic <- double(length(8))
aicc <- double(length(8))
bic <- double(length(8))

out <- lm(lnFire~1,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[1] <- outsum$r.squared
R2aj[1] <- outsum$adj.r.squared
aic[1] <- AIC(out)
aicc[1] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[1] <- AIC(out, k = log(n))
cp[1] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_1,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[2] <- outsum$r.squared
R2aj[2] <- outsum$adj.r.squared
aic[2] <- AIC(out)
aicc[2] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[2] <- AIC(out, k = log(n))
cp[2] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_2,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[3] <- outsum$r.squared
R2aj[3] <- outsum$adj.r.squared
aic[3] <- AIC(out)
aicc[3] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[3] <- AIC(out, k = log(nrow(Exemple)))
cp[3] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_3,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[4] <- outsum$r.squared
R2aj[4] <- outsum$adj.r.squared
aic[4] <- AIC(out)
aicc[4] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[4] <- AIC(out, k = log(nrow(Exemple)))
cp[4] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_1+X_2,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[5] <- outsum$r.squared
R2aj[5] <- outsum$adj.r.squared
aic[5] <- AIC(out)
aicc[5] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[5] <- AIC(out, k = log(nrow(Exemple)))
cp[5] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_1+X_3,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[6] <- outsum$r.squared
R2aj[6] <- outsum$adj.r.squared
aic[6] <- AIC(out)
aicc[6] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[6] <- AIC(out, k = log(nrow(Exemple)))
cp[6] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_2+X_3,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[7] <- outsum$r.squared
R2aj[7] <- outsum$adj.r.squared
aic[7] <- AIC(out)
aicc[7] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[7] <- AIC(out, k = log(nrow(Exemple)))
cp[7] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

out <- lm(lnFire~X_1+X_2+X_3,data=Exemple)
k <- length(out$coefficients)
outsum <- summary(out)
R2[8] <- outsum$r.squared
R2aj[8] <- outsum$adj.r.squared
aic[8] <- AIC(out)
aicc[8] <- AIC(out)+ 2*k*(k+1)/(n-k-1)
bic[8] <- AIC(out, k = log(nrow(Exemple)))
cp[8] <- sum(out$residuals^2)/sigsqhat.big + 2 * out$rank - n

resultats <- cbind(R2,R2aj,cp, aic, aicc, bic)
dimnames(resultats) <- list(c("","1","2","3","1,2","1,3","2,3","1,2,3"), c("R2","R2aj","Cp", "AIC", "AICc", "BIC"))
resultats


