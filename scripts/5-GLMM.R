##### Modèle GLMM WBF en fonction de la masse des balises ####

# load data
pigeon_path <- here::here()
output_activity_accdata <- paste0(pigeon_path,"/outputs/activity/acc_data/")
load(paste0(output_activity_accdata, "script3_subdatkmeans_cluster2.RData"))
load(paste0(output_activity_accdata, "dat_WBF_metadata.RData"))

library(tidyverse)

datr <- datWBF %>%
  select("indiv", "time", "WBF", "device_id", "sex", "AP", "LT", "device_type", "mass")

datkmeans_cl2$Date <- format(datkmeans_cl2$Date, "%Y-%m-%d %H:%M:%OS2", tz = "Europe/Paris")
datr$time <- format(datr$time, "%Y-%m-%d %H:%M:%OS2", tz = "Europe/Paris")

datkmeans_cl2_WBF <- datkmeans_cl2 %>%
  left_join(datr, join_by(Date == time, indiv == indiv))

datWBF <- datkmeans_cl2_WBF

# remove nas
datWBF <- datWBF %>%
  filter(!is.na(WBF))

summary(datWBF)
head(datWBF)

# variable réponse : WBF = variable quantitative continue non-bornée, loi normale en théorie
# variables explicatives : device_type = facteur fixe à 3 modalités (OT6, OT9, OT10)
# mass = covariable
# indiv = facteur aléatoire

hist(datWBF$WBF)

library(car)
qqPlot(datWBF$WBF) # pas normale
shapiro.test(datWBF$WBF) # p<0.05, deviate from normality

# comparaison de plus de 2 variances
# test de Bartlett par permutation (non-paramétrique car distrib non normale)
perm.bartlett.test(WBF~device_type, data = datWBF) # p<0.05

# comparaison de plus de 2 moyennes


library(lme4)
mnorm <- lmer(WBF ~ device_type * mass + (1|indiv), data = datWBF)
plotresid(mnorm) # catastrophique

# solutions pour indépendance entre les résidus du modèle et les valeurs prédites pas respectée
mnorm2 <- glmer(WBF ~ device_type * mass + (1|indiv), family = gaussian(link = "log"), # changer la fonction de lien
                mustart = fitted.values(mnorm), # bug affecting gaussian glm's with non-standard link : need to specify mustart
                # https://bugs.r-project.org/show_bug.cgi?id=16877#c3
                data = datWBF)
plotresid(mnorm2)
mnorm3 <- glmer(WBF ~ device_type * mass + (1|indiv), family = gaussian(link = "inverse"), 
               mustart = fitted.values(mnorm),
               data = datWBF)
plotresid(mnorm3)
mnormsqrt <- lmer(WBF ~ device_type * sqrt(mass) + (1|indiv), data = datWBF) # transformer variable explicative quanti
plotresid(mnormsqrt)
mnormlog <- lmer(WBF ~ device_type * log(mass) + (1|indiv), data = datWBF)
plotresid(mnormlog)

# solution pour équivariance des résidus pas respectée
library(MASS)
mnorm4 <- glmmPQL(WBF ~ device_type * mass, random = ~1|indiv, 
                  family = quasi(link = "log", variance = "mu"), verbose = FALSE, data = datWBF)
plotresid(mnorm4)

# solution pour normalité des résidus
mnorm5 <- lmer(sqrt(WBF) ~ device_type * mass + (1|indiv), data = datWBF)
plotresid(mnorm5)

# essayer avec WBF = quantitativé continue bornée entre 0 et 10, loi bêta en théorie
# on ramène entre 0 et 1
datWBF$WBF2 <- (datWBF$WBF-0)/(10-0)
# transformer la réponse pour exclure 0 et 1
library(RVAideMemoire)
datWBF$WBF2 <- p.beta(datWBF$WBF2)

# régression bêta mixte
library(glmmTMB)
m2 <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), family = beta_family(link = "logit"), data = datWBF)

# vérification de l'ajustement aux données
# 1. indépendance entre les résidus du modèle et les valeurs prédites
plotresid(m2) # ligne rouge assez horizontale donc ok
# 2. modélisation correcte de la variance des résidus
# dispersion verticale des points pas constante donc pas ok
m2bis <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), dispformula = ~device_type * mass, family = beta_family, data = datWBF)
m3 <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), dispformula = ~device_type, family = beta_family, data = datWBF)
m4 <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), dispformula = ~mass, family = beta_family, data = datWBF)

library(MuMIn)
AICc(m2bis, m3, m4) # m2bis a la valeur d'AICc la plus faible
plotresid(m2bis)

# test de Wald
Anova(m2bis)
# mass is not significant p>0.05
# device type is significant *** p<0.001

# comparaisons multiples pour identifier les modalités qui diffèrent dans le facteur device type

# calculer moyennes ajustées
library(emmeans)
EMM <- emmeans(m2bis, ~device_type)

# réaliser les comparaisons
library(multcomp)
cld(EMM, details = TRUE)
# pas de différence significative entre OT9 et OT10
# différences significatives entre OT6-OT10 et entre OT6-OT9 p<0.0001
# estimates positifs (1.4275 & 1.3583) donc OT6 est significativement plus élevé que OT9 et OT10
# interprétation biologique : les pigeons équipés de balises de 6g ont une fréquence de battement d'ailes plus élevée
# pourquoi ? vol moins contraint, cinématique de vol plus naturelle
# balises plus lourdes réduisent la fréquence de battement, l'effet négatif apparaît déjà à 9g, puis sature

# récupérer les moyennes ajustées pour graph
valeurs <- back.emmeans(EMM, transform = "p.beta", n = 654) # spécifiquement pour régression bêta mixte # n=nrows
valeurs

# diagramme en barres avec barres d'erreur

# prendre valeurs, les WBF ont été transformés pour être entre 0 et 1 donc je remultiplie par 10
moyennes <- c(5.634615, 2.486977, 2.359619) # dans l'ordre du graphe de gauche à droite : OT6, OT9, OT10
erreurs_inf <- c(5.290143, 2.201554, 2.086334)
erreurs_sup <- c(5.973089, 2.796031, 2.656574)
# lettres de significativité
lettres <- c("a", "b", "b")

# construire barplot
abscisses <- barplot(moyennes, main = "Effect of device mass on wingbeat frequency in homing pigeons", cex.main = 1.1, names.arg = c("6", "9", "10"), 
                    xlab = "Device mass (g)", ylab = "Wingbeat frequency (Hz)", ylim = c(0, 6.5), col = c("mistyrose", "pink", "hotpink"))
# ajouter barres d'erreur
arrows(abscisses, erreurs_inf, abscisses, erreurs_sup, code=3, angle=90, length=0.15)
# ajouter lettres de significativité
text(x = abscisses,
     y = erreurs_sup + 0.25,
     labels = lettres,
     cex = 1.2,
     font = 2)

#### essai avec OS3 pour avoir plus de données ?
####
# on ramène entre 0 et 1
datWBF$WBF2 <- (datWBF$WBF-0)/(10-0)
# transformer la réponse pour exclure 0 et 1
library(RVAideMemoire)
datWBF$WBF2 <- p.beta(datWBF$WBF2)

# régression bêta mixte
library(glmmTMB)
m2 <- glmmTMB(WBF2 ~ device_type + mass + (1|indiv), family = beta_family(link = "logit"), data = datWBF) # avec interaction

# vérification de l'ajustement aux données
# 1. indépendance entre les résidus du modèle et les valeurs prédites
plotresid(m2) # ligne rouge assez horizontale donc ok
# 2. modélisation correcte de la variance des résidus
# dispersion verticale des points pas constante donc pas ok
m2bis <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), dispformula = ~device_type * mass, family = beta_family, data = datWBF)
m3 <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), dispformula = ~device_type, family = beta_family, data = datWBF)
m4 <- glmmTMB(WBF2 ~ device_type * mass + (1|indiv), dispformula = ~mass, family = beta_family, data = datWBF)

library(MuMIn)
AICc(m2bis, m3, m4) # m2bis a la valeur d'AICc la plus faible

# test de Wald
Anova(m2bis)
# effet significatif de la masse, device_type et interaction
summary(m2bis) # estimate positif pour mass
# la fréquence de battement d'ailes augmente quand la masse du pigeon augmente

# comparaisons multiples pour identifier les modalités qui diffèrent dans le facteur device type

# calculer moyennes ajustées
library(emmeans)
EMM2 <- emmeans(m2bis, ~device_type)

# réaliser les comparaisons
library(multcomp)
cld(EMM2, details = TRUE) 
# OT6-OT10 & OT6-OT9 significatifs

# interaction entre un facteur et une covariable : pentes
pentes <- emtrends(m2bis, ~device_type, var = "mass")

cld(pentes, details = TRUE)

#### modèle OBDA OS3 ####
library(lme4)
mODBA <- lmer(ODBA.mu.3s ~ device_type + mass + (1|indiv), data = datWBF)

# vérification de l'ajustement aux données
library(RVAideMemoire)
plotresid(mODBA)
library(MASS)
modele <- glmmPQL(ODBA.mu.3s ~ device_type + mass,random=~1|indiv,family=quasi(link="identity",variance="mu"),verbose=FALSE, data = datWBF)
plotresid(modele) # bof
r.squaredGLMM(modele) # R2m = 14%, R2c = 99%
Anova(modele)
# device_type est significatif p<0.05

# comparaisons multiples pour identifier les modalités qui diffèrent dans le facteur device type

# calculer moyennes ajustées
library(emmeans)
EMM3 <- emmeans(modele, ~device_type)

# réaliser les comparaisons
library(multcomp)
cld(EMM3, details = TRUE) # différence significative OT9-OT6
# OT6 a une ODBA + faible que OT9
# OT10 pas différent statistiquement

## tester variable continue + terme quadratique
mass2 <- datWBF$mass^2

mv_quad <- glmmPQL(ODBA.mu.3s ~ mass + mass2,
                   random = ~1|indiv,
                   family = quasi(link = "identity", variance = "mu"),
                   data = datWBF)
plotresid(mv_quad)
r.squaredGLMM(mv_quad) # 9% et 99%
Anova(mv_quad) # pas significatif

##### Modèle LMM ODBA en fonction de device_type et mass ####

# variable réponse : ODBA.mu.3s = variable quantitative continue non bornée
# loi normale a priori
# variables explicatives : device_type = facteur fixe à 3 modalités
# mass = covariable
# indiv = facteur aléatoire

# construire un modèle linéaire mixte
library(lme4)
mODBA <- lmer(ODBA.mu.3s ~ device_type + mass + (1|indiv), data = datWBF)

# vérification de l'ajustement aux données
library(RVAideMemoire)
plotresid(mODBA) # linéarité vérifiée
# homoscédasticité ok
# normalité ok

# capacité explicative globale
library(MuMIn)
r.squaredGLMM(mODBA)
# device_type et mass expliquent 1% de la variance de ODBA
# device_type, mass et indiv expliquent 6% de la variance de ODBA

# test de Wald
library(car)
Anova(mODBA)
# pas significatif p>0.05

# avec interaction
mODBA2 <- lmer(ODBA.mu.3s ~ device_type * mass + (1|indiv), data = datWBF)
plotresid(mODBA2) # ok
r.squaredGLMM(mODBA2)
Anova(mODBA2) # pas significatif

#### Modèle LMM VeDBA ####
mVeDBA <- lmer(VeDBA.3s ~ device_type + mass + (1|indiv), data = datWBF)
plotresid(mVeDBA)
r.squaredGLMM(mVeDBA) # R2m = 1%, R2c = 5%
Anova(mVeDBA) # pas significatif

#### Modèle LMM VeDBA.sd.3s ####
mv2 <- lmer(VeDBA.sd.3s ~ device_type + mass + (1|indiv), data = datWBF)
plotresid(mv2) # équivariance pas respectée
# on va passer sur un GLMM
library(MASS)
mv2b <- glmmPQL(VeDBA.sd.3s ~ device_type + mass, random = ~1|indiv, family = quasi(link = "identity", variance = "mu"),verbose = FALSE, data = datWBF)
plotresid(mv2b) # c'est mieux pour l'équivariance
r.squaredGLMM(mv2b) # R2m = 52%, R2c = 97%
Anova(mv2b)
# device_type est significatif p<0.05 *

# comparaisons multiples pour identifier les modalités qui diffèrent dans le facteur device type

# calculer moyennes ajustées
library(emmeans)
EMM2 <- emmeans(mv2b, ~device_type)

# réaliser les comparaisons
library(multcomp)
cld(EMM2, details = TRUE)
# pas de différence significative car correction de Tukey = perte de puissance. OT9 - OT6 et OT10 - OT6 sont relativement proches de 0.05