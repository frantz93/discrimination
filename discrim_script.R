# Chargement des librairies utiles au programme
library(dplyr)
library(broom)
library(oaxaca)
library(FactoMineR)
library(descr)
library(gplots)

# Lecture et visualisation de la base de donnees
load("discrimination/base.RData")
str(base)
head(base)

# Creation et formattage des variables
attach(base)
base$PROVINCE <- factor(PROV, labels = c("Terre-Neuve-et-Labrador", "Île-du-Prince-Édouard", "Nouvelle-Écosse", "Nouveau-Brunswick", 
                                         "Québec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "Colombie-Britannique"))
base$YOUNG <- factor(if_else(AGE < 3, 1, 0))
base$WOMAN <- factor(if_else(SEX == 1, 0, 1))
base$MARRIED <- factor(if_else(MARSTAT == 1, 1, 0))
base$EDUC.LEVEL <- factor(if_else(EDUC < 3, 0, if_else(EDUC < 5, 1, 2)))
base$IMMIGRANT <- factor(if_else(IMMIG != 3, 1, 0))
base$ISPAID <- factor(PAID)
base$SELF.EMP <- factor(if_else(CATWORK == 1, 0, 1))
base$ISUNEMP <- factor(UNEMP)
base$OCCUP.SEC <- factor(OCCUP)
base$PART.TIME <- factor(if_else(WTIME == 2, 1, 0))
base$YRS.TENURE <- base$TENURE/12
base$SYNDIC <- factor(if_else(UNION != 3, 1, 0))
base$LARGFIRM <- factor(if_else(FIRMSIZE == 4, 1, 0))
base$SAL <- HRLYEARN/100
base$LN.SAL <- log(base$SAL)
base$LONG.UNEMP <- factor(if_else(DURUNEMP >= 52, 1, 0))
plot(base$OCCUP.SEC, base$SAL, xlab = "Categorie professionnelle", ylab = "Salaire")
base$OCCUP.SEC <- factor(OCCUP, levels = c(7, 1, 2, 3, 4, 5, 6, 8, 9, 10))

# Analyse exploratoire
# 1- Statistiques descriptives et comparatives
baseQC <- subset.data.frame(base, PROVINCE == "Québec", select = c(19:34))
head(baseQC)
summary(baseQC)

CrossTable(baseQC$IMMIGRANT, baseQC$YOUNG, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$WOMAN, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$MARRIED, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$EDUC.LEVEL, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$ISPAID, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$SELF.EMP, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$ISUNEMP, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$OCCUP.SEC, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$PART.TIME, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$SYNDIC, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$LARGFIRM, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$LONG.UNEMP, prop.chisq = FALSE, chisq = TRUE)
CrossTable(baseQC$IMMIGRANT, baseQC$IMMIGRANT, prop.chisq = FALSE)

plotmeans(baseQC$SAL ~ baseQC$IMMIGRANT, p = 0.95, connect = FALSE, bars = TRUE, xlab = "Immigrant", ylab = "Salaire horaire")
t.test(baseQC$SAL ~ baseQC$IMMIGRANT)
plotmeans(baseQC$YRS.TENURE ~ baseQC$IMMIGRANT, p = 0.95, connect = FALSE, bars = TRUE, xlab = "Immigrant", ylab = "Expérience dans le poste (années)")
t.test(baseQC$YRS.TENURE ~ baseQC$IMMIGRANT)

summary(baseQC$SAL)
sd(baseQC$SAL, na.rm = TRUE)

summary(baseQC$YRS.TENURE)
sd(baseQC$YRS.TENURE, na.rm = TRUE)


# 2- Analyse factorielle : Analyse en composantes multiples (ACM)
varACM <- subset.data.frame(baseQC, 
                            select = c("WOMAN", "YOUNG", "MARRIED", "EDUC.LEVEL", "IMMIGRANT", "SELF.EMP", "SYNDIC", "OCCUP.SEC",
                                       "PART.TIME", "LARGFIRM", "ISPAID", "ISUNEMP", "LONG.UNEMP", "YRS.TENURE", "SAL"))
res.mca <- MCA(varACM, quali.sup = c(6:13), quanti.sup = c(14, 15), graph = FALSE)

plot(res.mca, invisible = c("ind", "quali.sup"), habillage = "quali", cex = 0.9, selectMod = "cos2 20")
plot(res.mca, invisible = c("ind", "var"), habillage = "quali", cex = 0.9, selectMod = "cos2 20")
plot(res.mca, invisible = "ind", autoLab = "y", cex = 0.8, habillage = "quali", selectMod = "cos2 10")
plot(res.mca, choix = "quanti.sup")
plot(res.mca, choix = "var", xlim = c(0, 0.5), ylim = c(0, 0.6))
plot
summary(res.mca)

cor(baseQC$SAL, baseQC$YRS.TENURE, use = "complete.obs")
cor.test(baseQC$SAL, baseQC$YRS.TENURE, use = "complete.obs")

# Regressions OLS - Salaires
baseQC.paid <- subset.data.frame(baseQC, ISPAID == "1")  #Filtrage des donnees des salariés au Quebec
summary(baseQC.paid)
View(baseQC.paid)

reg1 <- lm(LN.SAL ~ YOUNG + WOMAN + MARRIED + EDUC.LEVEL + IMMIGRANT + SELF.EMP + OCCUP.SEC + PART.TIME + YRS.TENURE + SYNDIC + LARGFIRM, data = baseQC.paid)
summary(reg1)
effects <- round(exp(reg1$coefficients)-1, 3)*100 #effects on salary
effects

reg2 <- lm(SAL ~ YOUNG + WOMAN + MARRIED + EDUC.LEVEL + IMMIGRANT + SELF.EMP + OCCUP.SEC + PART.TIME + YRS.TENURE + SYNDIC + LARGFIRM, data = baseQC.paid)
summary(reg2)


# Regression Logit - Chomage Long
baseQC.chom <- subset.data.frame(baseQC, ISUNEMP == "1")
View(baseQC.chom)
summary(baseQC.chom)

reg3 <- glm(LONG.UNEMP ~ YOUNG + WOMAN + MARRIED + EDUC.LEVEL + IMMIGRANT, family = binomial(link = "logit"), baseQC.chom)
summary(reg3)                   #Variables SELF.EMP et OCCUP.SEC ne sont pas intégrées dans la regression par manque de données
                                #Variables TENURE + SYNDIC + LARGFIRM ne sont pas d'intérêt car ne concernent pas les chômeurs

round(exp(coef(reg3)), 2)   #Calcul des ratio Exp^B


# Regression Logit - Chomage
reg4 <- glm(ISUNEMP ~ YOUNG + WOMAN + MARRIED + EDUC.LEVEL + IMMIGRANT, family = binomial(link = "logit"), baseQC)
summary(reg4)                   #Variables SELF.EMP et OCCUP.SEC ne sont pas intégrées dans la regression par manque de données
#Variables TENURE + SYNDIC + LARGFIRM ne sont pas d'intérêt car ne concernent pas les chômeurs

round(exp(coef(reg4)), 2)   #Calcul des ratio Exp^B


#Decomposition de l'inégalité salariale (méthode Oaxaca)
baseQC.paid$T.IMMIG <- with(baseQC.paid, IMMIGRANT == 1)          # Variables de type 'logical' qui serviront à la réalisation

results <- oaxaca(formula = SAL ~ YOUNG + WOMAN + MARRIED + EDUC.LEVEL + SELF.EMP + OCCUP.SEC + PART.TIME + YRS.TENURE + SYNDIC + LARGFIRM 
                  | T.IMMIG, data = baseQC.paid, R = 100)
results$n
results$y

round(results$twofold$overall[2, c(2,4)], 3)  #Decomposition bi-dimensionnelle
round(results$threefold$overall[c(1,3,5)], 3) #Decomposition tri-dimensionnelle

plot(results, decomposition = "twofold", group.weight = 1, components = c("explained"))   #graphe de la formation des ecarts expliqués par le modèle
round(results$twofold$variables[[2]][,2:3],3)   #coefficients de la formation des ecarts expliqués par le modèle

plot(results, components = c("endowments"))   #graphe de la formation des ecarts expliqués par les différences de caractéristiques
plot(results, components = c("interaction"))   #graphe de la formation des ecarts expliqués par les interactions
plot(results, components = c("coefficients")) #graphe de la formation des ecarts inexpliqués
round(results$threefold$variables, 3)  #coefficients de la formation des ecarts expliqués par les différences de caractéristiques


# Verification des differences de caracteristiques entre immigrants et non immigrants dans l'échantillon des salariés
test <- round(as.data.frame(results$x), 3)
test <- test[-1,]
test


#Decomposition de l'écart (natif - immigrant) de prob chomage long (methode Fairlie)
base$T.LONG.UNEMP <- with(base, DURUNEMP >= 52) # des regressions Oaxaca et Fairlie
summary(baseQC.chom$IMMIGRANT)

# 1- Caractéristiques des natifs (groupe A) vs les immigrants (groupe B)
X.B <- model.matrix(~ LONG.UNEMP + YOUNG + WOMAN + MARRIED + EDUC.LEVEL, data = baseQC.chom[baseQC.chom$IMMIGRANT == "1",])
X.A <- model.matrix(~ LONG.UNEMP + YOUNG + WOMAN + MARRIED + EDUC.LEVEL, data = baseQC.chom[baseQC.chom$IMMIGRANT == "0",])

prop.A <- apply(X.A, 2, mean)
prop.B <- apply(X.B, 2, mean)
ecartAB <- prop.A - prop.B        #ecart de caracteristiques entre les groupes (natifs - immigrants)

round(cbind(prop.A, prop.B, ecartAB), 3)

# 2- Regression logit dans le groupe des natifs (groupe A)
reg5 <- glm(LONG.UNEMP ~ YOUNG + WOMAN + MARRIED + EDUC.LEVEL, family = binomial(link = "logit"), data=baseQC.chom[baseQC.chom$IMMIGRANT == "0",])
summary(reg5)

# 3- On predit les probabilités a partir des estimateurs des natifs (groupe A)
baseQC.chom$pA <- predict(reg5, baseQC.chom, type = 'response')

prop.cB <- mean(baseQC.chom$pA[baseQC.chom$IMMIGRANT == "1"], na.rm = TRUE)      #probabilité que les immigrants soient 
                                  #en chômage long si leurs caractéristiques sont valorisées comme celles des natifs

expl <- prop.A[2] - prop.cB #Ecart expliqué
expl
inexpl <- prop.cB - prop.B[2] #Ecart inexpliqué
inexpl