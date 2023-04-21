#####################################################################################################################
# Sélection donneur sur variables receveur uniquement
#####################################################################################################################
library(ggplot2) # Pour les graphiques
library(ggsurvfit) # Graphiques de Kaplan-Meier avancés
library(ggrepel) # Arrangement automatique des labels des points superposés sur les graphes
library(RColorBrewer) # Gestion avancée des couleurs
library(plotly) # Graphiques interactifs
library(gridExtra) # Manipulation des objets graphiques
library(knitr) # Export et custimisation des tables
library(kableExtra) # Customisation avancée des tableaux exportés
library(table1) # Tables descriptives rapides
library(dplyr) # Syntaxe pipe, data management et graphiques
library(broom) # Data management
library(tidyr) # Data management
library(survival) # Analyses de survie
library(survminer) # Analyse de survie avancée
library(lmtest) # Tests de rapport de vraisemblance
library(shrink) # Shrinkage
library(rms) # Backward amélioré pour cph
library(base.rms) # Conversion coxph vers cph

# Génération de la table
tab_univ_donneur_sensi1 <- round(sapply(df[,var_donneur],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~df$ageR + df$imcR2 + df$sexeR + df$malIni2cl + df$hemodial +
                                                                                     df$anteDiab + df$anteDyslip + df$anteHTA + df$anteCardioVasc + df$anteNeo + df$cmvR + df$AcHBsR + df$tabac + x))$coefficients[14,]),4)

tab_univ_donneur_sensi1 <- t(tab_univ_donneur_sensi1)
tab_univ_donneur_sensi1
colnames(tab_univ_donneur_sensi1) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_donneur_sensi1) <- c('Donor age','Donor Height','Donor Weight','Last donor diuresis','Donor urea','Donor creatinine','Donor men',
                                'Donor after cardiac death','Donor death vascular etiology','Donor history of hypertension','Donor history of diabetes',
                                'Donor history of cardiac arrest','Donor proteinuria positive','Donor CMV serology positive','Donor BMI')



# Export en PDF

tab_univ_donneur_sensi1 %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_univ_donneur_sensi1.pdf')



# Construction du modèle enrichi | Plusieurs variables diffèrent (stratification sur le centre)

model_complet_sensi1 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du receveur
                         ageR + imcR + sexeR + malIni2cl + hemodial +
                         anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR +
                         # Variables de la greffe
                         sqrt(Tdial) + Tabm + incompABDR + before_2008 + strata(centre) +
                         # Variables du donneur
                         ageD + sexeD + tailleD + poidsD + card_death + causeDCD2cl + htaD + diabD + arretD + cmvD
                       ,data = df)
summary(model_complet_sensi1) 


#####################################################################################################################
# LASSO par étapes
#####################################################################################################################
library(glmnet)
library(tidyr)

################## Receveur

df_lasso <- drop_na(df[,c(var_receveur,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR+imcR+sexeR+malIni2cl+hemodial+anteDiab+anteDyslip+anteHTA
                  + anteCardioVasc + anteNeo + anteUro + cmvR + AcHBsR + antiClassI.jour + antiClassII.jour,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F)


# Choix de lambda

x11(); plot(mod_cv) 
lambda_rec_1 <- mod_cv$lambda[9]  # Valeur 'stringente' (plus petite valeur dans le CI du premier point)
lambda_rec_2 <- mod_cv$lambda[18] # Valeur 'moyenne' (coude)

# Modèles

lasso_rec1 <- glmnet(x,y,family='cox',lambda=lambda_rec_1) # Stringent
lasso_rec2 <- glmnet(x,y,family='cox',lambda=lambda_rec_2) # Moyen

#### Résultats

# Modèle 'moyen'

myCoefs <- coef(lasso_rec2);
myCoefs[which(myCoefs != 0 ) ] 
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

var_rec_lasso_1 <- c('ageR','hemodial','anteDiab','anteCardioVasc','anteNeo','anteDyslip','imcR','malIni2cl',
                     'cmvR','anteHTA','anteUro','AcHBsR',
                     'antiClassI.jour','antiClassII.jour','sexeR')

pen_fac_rec_1 <- c(rep(0,5),rep(99999,10)) 

# Modèle 'stringent'

myCoefs <- coef(lasso_rec1);
myCoefs[which(myCoefs != 0 ) ] 
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

var_rec_lasso_2 <- c('ageR','anteDiab','anteCardioVasc','anteDyslip','imcR','malIni2cl','hemodial',
                     'anteNeo','cmvR','anteHTA','anteUro','AcHBsR',
                     'antiClassI.jour','antiClassII.jour','sexeR')

pen_fac_rec_2 <- c(rep(0,3),rep(99999,12)) 



# Modèle 'permissif' (cross validé)

myCoefs <- coef(mod_cv, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] 
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

var_rec_lasso_3 <- c('ageR','imcR','malIni2cl','hemodial','anteDiab','anteDyslip',
                     'anteCardioVasc','anteNeo','cmvR','anteHTA','anteUro','AcHBsR',
                     'antiClassI.jour','antiClassII.jour','sexeR')

pen_fac_rec_3 <- c(rep(0,9),rep(99999,6)) 












################## Greffe

####### Modèle 'stringent'


df_lasso <- drop_na(df[,c(var_rec_lasso_1,var_greffe,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR + hemodial + anteDiab + anteCardioVasc + anteNeo + anteDyslip + imcR+malIni2cl+
                    cmvR +
                    anteHTA + anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm + incomp2cl
                  ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,pen_fac_rec_1,rep(1,4)) )


# Choix de lambda

x11(); plot(mod_cv) 
lambda_gref_1 <- mod_cv$lambda[9]  # 'Coude'

# Modèle

lasso_gref1 <- glmnet(x,y,family='cox',lambda=lambda_gref_1, penalty.factor = c(0,pen_fac_rec_1,rep(1,4)))

# Résultats

myCoefs <- coef(lasso_gref1, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] 
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

var_rec_gref_lasso_1 <- c('ageR','hemodial','anteDiab','anteCardioVasc','anteNeo','incomp2cl','anteDyslip','imcR','malIni2cl',
                     'cmvR','anteHTA','anteUro','AcHBsR',
                     'antiClassI.jour','antiClassII.jour','sexeR','Tdial','Tabm','iscHeure')

pen_fac_rec_gref_1 <- c(rep(0,6),rep(99999,13)) 



####### Modèle 'stringent'


df_lasso <- drop_na(df[,c(var_rec_lasso_2,var_greffe,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR + anteDiab + anteCardioVasc +anteDyslip + imcR+malIni2cl+hemodial+
                    anteNeo  + cmvR +
                    anteHTA + anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm + incomp2cl
                  ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,pen_fac_rec_2,rep(1,4)) )


# Choix de lambda

x11(); plot(mod_cv) 
lambda_gref_1 <- mod_cv$lambda[1]  # Valeur 'stringente' (plus petite valeur dans le CI du premier point)

# Modèle

lasso_gref1 <- glmnet(x,y,family='cox',lambda=lambda_gref_1, penalty.factor = c(0,pen_fac_rec_2,rep(1,4)))

# Résultats

myCoefs <- coef(lasso_gref1, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] 
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

var_rec_gref_lasso_2 <- c('ageR','anteDiab','anteCardioVasc','anteDyslip','imcR','malIni2cl','hemodial',
                          'anteNeo','cmvR','anteHTA','anteUro','AcHBsR',
                          'antiClassI.jour','antiClassII.jour','sexeR','incomp2cl','Tdial','Tabm','iscHeure')

pen_fac_rec_gref_2 <- c(rep(0,3),rep(99999,16)) 


####### Modèle 'permissif' cross-validé

df_lasso <- drop_na(df[,c(var_rec_lasso_3,var_greffe,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR+imcR+malIni2cl+hemodial+anteDiab+anteDyslip +
                    anteCardioVasc + anteNeo  + cmvR +
                    anteHTA + anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm + incomp2cl
                    ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,pen_fac_rec_3,rep(1,4)) )


# Résultats

myCoefs <- coef(mod_cv, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] 
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

var_rec_gref_lasso_3 <- c('ageR','imcR','malIni2cl','hemodial','anteDiab','anteDyslip',
                     'anteCardioVasc','anteNeo','cmvR','incomp2cl','anteHTA','anteUro','AcHBsR',
                     'antiClassI.jour','antiClassII.jour','sexeR','Tdial','Tabm','iscHeure')

pen_fac_rec_gref_3 <- c(rep(0,10),rep(99999,9)) 











################## Donneur

var_donneur2 <- c(var_donneur[1],var_donneur[4:16])

####### Modèle 'moyen'


df_lasso <- drop_na(df[,c(var_rec_gref_lasso_1,var_donneur2,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR + hemodial + anteDiab + anteCardioVasc + anteNeo + incomp2cl + anteDyslip + imcR+malIni2cl+
                    cmvR +
                    anteHTA + anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm + 
                    ageD + diureseD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD +
                    arretD + protuD + cmvD + imcD + drogVasoD
                  ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,pen_fac_rec_gref_1,rep(1,14)) )


# Choix de lambda

x11(); plot(mod_cv) 
lambda_don_1 <- mod_cv$lambda[10]  # Coude

# Modèle

lasso_donn1 <- glmnet(x,y,family='cox',lambda=lambda_don_1, penalty.factor = c(0,pen_fac_rec_gref_1,rep(1,14)) )

# Résultats

myCoefs <- coef(lasso_donn1, s="lambda.min");
coef_final_moyen <- myCoefs[which(myCoefs != 0 ) ] 
var_final_moyen <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 



x <- model.matrix(y ~ ageR + hemodial + anteDiab + anteCardioVasc + anteNeo + incomp2cl + 
                    ageD + ureeD + causeDCD2cl +  htaD + diabD + arretD + cmvD + imcD +
                    anteDyslip + imcR+malIni2cl+
                    cmvR + card_death +
                    anteHTA + anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm + 
                    diureseD + creatD + sexeD + 
                    protuD + drogVasoD
                  ,df_lasso)

# Cross validation mod final

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,rep(1,14),rep(Inf,19)) )


# Choix de lambda final

x11(); plot(mod_cv) 
lambda_don_1 <- mod_cv$lambda[30]  # Conserver les variables choisies sans pénalité




lasso_donn1 <- glmnet(x,y,family='cox',lambda=lambda_don_1, penalty.factor = c(0,rep(1,14),rep(Inf,19)) )

myCoefs <- coef(lasso_donn1, s="lambda.min");
coef_final_moyen <- myCoefs[which(myCoefs != 0 ) ] 
var_final_moyen <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 




####### Modèle 'stringent'


df_lasso <- drop_na(df[,c(var_rec_gref_lasso_2,var_donneur2,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR + anteDiab + anteCardioVasc +anteDyslip + imcR+malIni2cl+hemodial+
                    anteNeo  + cmvR +
                    anteHTA + anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm + incomp2cl +
                    ageD + diureseD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD +
                    arretD + protuD + cmvD + imcD
                  ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,pen_fac_rec_gref_2,rep(1,13)) )


# Choix de lambda

x11(); plot(mod_cv) 
lambda_don_1 <- mod_cv$lambda[1]  # Valeur 'stringente' (plus petite valeur dans le CI du plus bas point)

# Modèle

lasso_donn1 <- glmnet(x,y,family='cox',lambda=lambda_don_1, penalty.factor = c(0,pen_fac_rec_gref_2,rep(1,13)) )

# Résultats

myCoefs <- coef(lasso_donn1, s="lambda.min");
coef_final_stringent <- myCoefs[which(myCoefs != 0 ) ] 
var_final_stringent <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 





####### Modèle 'permissif' 

df_lasso <- drop_na(df[,c(var_rec_gref_lasso_3,var_donneur2,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR+imcR+malIni2cl+hemodial+anteDiab+anteDyslip +
                    anteCardioVasc + anteNeo  + cmvR + incomp2cl +
                    anteHTA +anteUro + AcHBsR + antiClassI.jour + antiClassII.jour + sexeR +
                    iscHeure + Tdial + Tabm +
                    ageD + diureseD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD +
                    arretD + protuD + cmvD + imcD
                  ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F, penalty.factor = c(0,pen_fac_rec_gref_3,rep(1,13)) )


# Résultats

myCoefs <- coef(mod_cv, s="lambda.min");
coef_final_3 <- myCoefs[which(myCoefs != 0 ) ] 
var_final_3 <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 



################## Tableaux PDF

# Modele stringent

tab_stringent <- data.frame(beta=coef_final_stringent)

rownames(tab_stringent) <- c('Recipient Age','History of diabetes','History of cardiovascular disease')
colnames(tab_stringent) <- c('\\beta')

tab_stringent %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/LASSO groupé/LASSO_stringent.pdf')

# Modele moyen

tab_moyen <- data.frame(beta=coef_final_moyen)

rownames(tab_moyen) <- c('Recipient Age','Hemodialysis','History of diabetes','History of cardiovascular disease',
                         'History of neoplasia','HLA incompatibilities >=4','Donor Age','Donor urea',
                         'Donor death vascular etiology','Donor history of hypertension',
                         'Donor history of diabetes','Donor history of cardiac arrest','Donor CMV serology positive',
                         'Donor BMI')
colnames(tab_moyen) <- c('\\beta')

tab_moyen %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/LASSO groupé/LASSO_moyen.pdf')

# Modele permissif

tab_permissif <- data.frame(beta=coef_final_3)

rownames(tab_permissif) <- c('Recipient Age','Recipient BMI','Relapsing initial nephropathy','Hemodialysis','History of diabetes',
                         'History of Dyslipemia','History of cardiovascular disease',
                         'History of neoplasia','Recipient CMV serology positive','HLA incompatibilities >=4',
                         'Donor Age','Donor urea',
                         'Donor death vascular etiology','Donor history of hypertension',
                         'Donor history of diabetes','Donor history of cardiac arrest','Donor CMV serology positive',
                         'Donor BMI')
colnames(tab_permissif) <- c('\\beta')

tab_permissif %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/LASSO groupé/LASSO_permissif.pdf')


#####################################################################################################################
# LASSO sur toutes les variables
#####################################################################################################################

# Construction

var_lasso <- c(var_donneur2,var_receveur,var_greffe)
df_lasso <- drop_na(df[,c(var_lasso,'TpsEvtYear','Evt')])
y <- Surv(df_lasso$TpsEvtYear,df_lasso$Evt)
x <- model.matrix(y ~ ageR+imcR+sexeR+malIni2cl+hemodial+anteDiab+anteDyslip+anteHTA+
                    anteCardioVasc + anteNeo + anteUro + cmvR + AcHBsR + antiClassI.jour + antiClassII.jour +
                    iscHeure + Tdial + Tabm + incomp2cl +
                    ageD + diureseD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD +
                    arretD + protuD + cmvD + imcD
                    ,df_lasso)

# Cross validation

mod_cv <- cv.glmnet(x=x, y=y, family='cox',nfolds=20,
                    intercept = F)


# Choix de lambda

x11(); plot(mod_cv) 
lambda_rec_1 <- mod_cv$lambda[28]  # coude
lambda_rec_2 <- mod_cv$lambda[23]  # coude


# Modèle 

lasso_rec1 <- glmnet(x,y,family='cox',lambda=lambda_rec_1) 
myCoefs <- coef(lasso_rec1);
coef_lasso_glob <- myCoefs[which(myCoefs != 0 ) ] 
var_lasso_glob <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

# Tableau PDF

tab_lasso_global <- data.frame(beta=coef_lasso_glob)

rownames(tab_lasso_global) <- c('Recipient Age','Relapsing initial nephropathy','History of diabetes','History of cardiovascular disease',
                         'History of neoplasia','HLA incompatibilities >=4','Donor Age','Donor urea','Donor creatinine',
                         'Donor death vascular etiology','Donor history of hypertension',
                         'Donor history of diabetes','Donor history of cardiac arrest','Donor CMV serology positive',
                         'Donor BMI')
colnames(tab_lasso_global) <- c('\\beta')

tab_lasso_global %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/LASSO global/LASSO_global.pdf')


# Modèle 2

lasso_rec2 <- glmnet(x,y,family='cox',lambda=lambda_rec_2) 
myCoefs <- coef(lasso_rec2);
coef_lasso_glob <- myCoefs[which(myCoefs != 0 ) ] 
var_lasso_glob <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] 

# Tableau PDF

tab_lasso_global <- data.frame(beta=coef_lasso_glob)

rownames(tab_lasso_global) <- c('Recipient Age','Relapsing initial nephropathy','History of diabetes','History of cardiovascular disease',
                                'History of neoplasia','HLA incompatibilities >=4','Donor Age','Donor urea',
                                'Donor death vascular etiology','Donor history of hypertension',
                                'Donor CMV serology positive','Donor BMI')
colnames(tab_lasso_global) <- c('\\beta')

tab_lasso_global %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Sensibilité/LASSO global/LASSO_global_12var.pdf')

#####################################################################################################################
# Backward sur l'AIC
#####################################################################################################################

#################################### Receveur

################## Sélection univariée


# Variables
var_receveur <- c(var_univ_receveur_quanti,var_univ_receveur_quali)

# Génération de la table
tab_univ_receveur <- round(sapply(df[,var_receveur],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~x))$coefficients),3)
tab_univ_receveur <- t(tab_univ_receveur)
colnames(tab_univ_receveur) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_receveur) <- c('Recipient Age','Recipient BMI','Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis',
                                 'History of diabetes','History of Dyslipidemia','History of hypertension',
                                 'History of cardiovascular disease','History of neoplasia','History of urological disease',
                                 'Positive recipient CMV serology','Positive recipient AcHBs serology','anti-HLA immunization of class I',
                                 'anti-HLA immunization of class II')


################## Modèle retenu

# Construction du modèle de base

model_receveur <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du receveur
                          ageR + imcR + sexeR + malIni2cl + hemodial +
                          anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR
                        ,data = df)
summary(model_receveur) 

################## Backward

fastbw(coxph2cph(model_receveur),rule='aic',sls=0.05)

model_receveur <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du receveur
                          ageR + hemodial +
                          anteDiab + anteCardioVasc + anteNeo
                        ,data = df)
summary(model_receveur) 

#################################### Greffe

################## Sélection univariée

# Variables
var_greffe <- c('iscHeure','Tdial','Tabm','incomp2cl')

# Génération de la table
tab_univ_greffe <- round(sapply(df[,var_greffe],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~df$ageR + df$hemodial + df$anteDiab + df$anteCardioVasc + df$anteNeo+ x))$coefficients[6,]),4)
tab_univ_greffe <- as.data.frame(t(tab_univ_greffe))
colnames(tab_univ_greffe) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_greffe) <- c('CIT','Time on dialysis','Time on waiting list','HLA AB-D-R incompatibilities')



################## Modèle retenu

# Construction du modèle de base

model_greffe <- coxph(Surv(TpsEvtYear, Evt) ~ 
                        # Variables du receveur
                        ageR + hemodial +
                        anteDiab + anteCardioVasc + anteNeo +
                        # Variables de la greffe
                        incomp2cl + iscHeure + Tdial + Tabm
                      ,data = df)
summary(model_greffe) 

################## Backward

fastbw(coxph2cph(model_greffe),rule='aic',force = 1:5,sls=0.05)

model_greffe <- coxph(Surv(TpsEvtYear, Evt) ~ 
                        # Variables du receveur
                        ageR + hemodial + 
                        anteDiab + anteCardioVasc + anteNeo +
                        # Variables de la greffe
                        incomp2cl + Tdial
                      ,data = df)
summary(model_greffe) 

#################################### Donneur


################## Sélection univariée

# Variables
var_donneur <- c(var_univ_donneur_quanti,var_univ_donneur_quali,'imcD')

# Génération de la table
tab_univ_donneur <- round(sapply(df[,var_donneur],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~df$ageR + df$hemodial +
                                                                              df$anteDiab + df$anteCardioVasc + df$anteNeo + df$incomp2cl + df$Tdial + x,ties='breslow'))$coefficients[8,]),4)

tab_univ_donneur <- t(tab_univ_donneur)
tab_univ_donneur
colnames(tab_univ_donneur) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_donneur) <- c('Donor age','Donor Height','Donor Weight','Last donor diuresis','Donor urea','Donor creatinine','Donor men',
                                'Donor after cardiac death','Donor death vascular etiology','Donor history of hypertension','Donor history of diabetes',
                                'Donor history of cardiac arrest','Donor proteinuria positive','Donor CMV serology positive','Donor treated by vasoppressor','Donor BMI')

################## Modèle retenu

# Construction du modèle de base

model_complet <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du receveur
                         ageR + hemodial +
                         anteDiab + anteCardioVasc + anteNeo +
                         # Variables de la greffe
                         incomp2cl +  Tdial +
                         # Variables du donneur
                         ageD + imcD + card_death + causeDCD2cl + htaD + diabD + arretD + cmvD
                       ,data = df)
summary(model_complet) 

################## Backward

fastbw(coxph2cph(model_complet),rule='aic',force = 1:7,sls=0.05)

model_complet <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du receveur
                         ageR + hemodial +
                         anteDiab + anteCardioVasc + anteNeo +
                         # Variables de la greffe
                         incomp2cl + Tdial +
                         # Variables du donneur
                         ageD + imcD + card_death + causeDCD2cl
                       ,data = df)
summary(model_complet) 


# Export du modèle de base en pdf

model_complet_df <- as.data.frame(tidy(model_complet))
model_complet_df$hr <- exp(model_complet_df$estimate)
model_complet_df <- model_complet_df[c(2,6,3:5)]
rownames(model_complet_df) <- c('Recipient Age','Hemodialysis',
                                'History of diabetes','History of cardiovascular disease','History of neoplasia',
                                'HLA A-B-DR incompatibilities>=4','Time on dialysis','Donor age','Donor BMI','Donor after cardiac death','Donor death vascular etiology' )
colnames(model_complet_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_complet_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/Backward alternatives/backward_aic.pdf')





#####################################################################################################################
# Backward sur toutes les variables d'un coup
#####################################################################################################################
var_univ_sensi <- c(var_receveur,var_greffe,var_donneur2)

# Génération de la table
tab_back_glob <- round(sapply(df[,var_univ_sensi],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~x))$coefficients),3)
tab_back_glob <- t(tab_back_glob)
colnames(tab_back_glob) <- c('\\beta','HR','SE(\\beta)','Z','p')


# Export en PDF

tab_back_glob %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/Backward alternatives/table_univarie_globale.pdf')

# Construction du modèle de base

model_global <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du receveur
                          ageR + imcR + sexeR + malIni2cl + hemodial +
                          anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR +
                          Tdial + incomp2cl + ageD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD +
                          diabD + arretD + cmvD + drogVasoD + imcD
                        ,data = df)
summary(model_global) 

# Backward

fastbw(coxph2cph(model_global),rule='p',sls=0.05)

model_global <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          ageR +
                          anteDiab + anteCardioVasc + incomp2cl + ageD + card_death + causeDCD2cl
                        ,data = df)
summary(model_global) 

model_glob_df <- as.data.frame(tidy(model_global))
model_glob_df$hr <- exp(model_glob_df$estimate)
model_glob_df <- model_glob_df[c(2,6,3:5)]
rownames(model_glob_df) <- c('Recipient Age',
                             'History of diabetes','History of cardiovascular disease','HLA incompatibilities >=4',
                             'Donor age','Donor after cardiac death','Donor death vascular etiology')
colnames(model_glob_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_glob_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Sensibilité/Backward alternatives/backward_globale.pdf')
 
BIC(model_global
    )





#####################################################################################################################
# Stratégie 2 avec BIC
#####################################################################################################################

#################################### Table univariée

var_univ_strat2 <- c(var_receveur,var_greffe,var_donneur2)
var_univ_strat2 <- c(var_univ_strat2[1:(length(var_univ_strat2)-2)],'tailleD','poidsD')

# Génération de la table
tab_back_glob <- round(sapply(df[,var_univ_strat2],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~x))$coefficients),3)
tab_back_glob <- t(tab_back_glob)
colnames(tab_back_glob) <- c('\\beta','HR','SE(\\beta)','Z','p')

# Export en PDF

tab_back_glob %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Univarié/table_univarie_globale.pdf')


#################################### Donneur

################## Sélection Backward

var_donneur_univ <- c('ageD','ureeD','creatD','sexeD','card_death','causeDCD2cl','htaD','diabD','arretD',
                      'cmvD','tailleD','poidsD')

model_donneur3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du donneur
                          ageD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD + 
                          arretD + cmvD + tailleD + poidsD
                        ,data = df)
summary(model_donneur3) 

# Backward

# Faire à la main

model_donneur3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du donneur
                          ageD + card_death + causeDCD2cl +
                          htaD + cmvD + tailleD + poidsD
                        ,data = df)
summary(model_donneur3) 

#################################### Greffe

################## Sélection Backward

var_greffe_univ <- c('Tdial','Tabm','incomp2cl')

model_greffe3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du donneur
                         ageD + card_death + causeDCD2cl +
                         htaD + cmvD + tailleD + poidsD +
                         # Variables de la greffe
                         Tdial + Tabm + incomp2cl
                       ,data = df)
summary(model_greffe3) 

# Backward

fastbw(coxph2cph(model_greffe3),rule='aic',sls=0.05,force=1:7)

model_greffe3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du donneur
                         ageD + card_death + causeDCD2cl +
                         htaD + cmvD + tailleD + poidsD +
                         # Variables de la greffe
                         incomp2cl + Tdial
                       ,data = df)
summary(model_greffe3) 

#################################### Receveur

################## Sélection Backward

var_rec_univ <- c('ageR','imcR','sexeR','malIni2cl','hemodial','anteDiab','anteDyslip',
                  'anteHTA','anteCardioVasc','anteNeo','cmvR','AcHBsR')

model_receveur3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                           # Variables du donneur
                           ageD + card_death + causeDCD2cl +
                           htaD + cmvD + tailleD + poidsD +
                           # Variables de la greffe
                           incomp2cl + Tdial +
                           # Variables du receveur
                           ageR + imcR + sexeR + malIni2cl + hemodial + anteDiab + anteDyslip +
                           anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR
                         ,data = df)
summary(model_receveur3) 

# Backward

fastbw(coxph2cph(model_receveur3),rule='aic',sls=0.05,force=1:9)

model_receveur3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                           # Variables du donneur
                           ageD + card_death + causeDCD2cl +
                           htaD + cmvD + tailleD + poidsD +
                           # Variables de la greffe
                           incomp2cl + Tdial + 
                           # Variables du receveur
                           ageR + anteDiab + anteCardioVasc
                         ,data = df)
summary(model_receveur3) 

#################################### Retour donneur

################## Sélection Backward

fastbw(coxph2cph(model_receveur3),rule='aic',sls=0.05,force=9:12)

model_final3 <- model_receveur3

summary(model_final3) 



#####################################################################################################################
# Stratégie 2 sans le temps sur liste
#####################################################################################################################

#################################### Table univariée

var_univ_strat2 <- c(var_receveur,var_greffe,var_donneur2)
var_univ_strat2 <- c(var_univ_strat2[1:(length(var_univ_strat2)-2)],'tailleD','poidsD')

# Génération de la table
tab_back_glob <- round(sapply(df[,var_univ_strat2],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~x))$coefficients),3)
tab_back_glob <- t(tab_back_glob)
colnames(tab_back_glob) <- c('\\beta','HR','SE(\\beta)','Z','p')

# Export en PDF

tab_back_glob %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Univarié/table_univarie_globale.pdf')


#################################### Donneur

################## Sélection Backward

var_donneur_univ <- c('ageD','ureeD','creatD','sexeD','card_death','causeDCD2cl','htaD','diabD','arretD',
                      'cmvD','tailleD','poidsD')

model_donneur2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du donneur
                          ageD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD + 
                          arretD + cmvD + tailleD + poidsD
                        ,data = df)
summary(model_donneur2) 

# Backward

fastbw(coxph2cph(model_donneur2),rule='p',sls=0.05)

model_donneur2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du donneur
                          ageD + card_death + causeDCD2cl +
                          htaD + cmvD + tailleD + poidsD
                        ,data = df)
summary(model_donneur2) 

model_donneur2_df <- as.data.frame(tidy(model_donneur2))
model_donneur2_df$hr <- exp(model_donneur2_df$estimate)
model_donneur2_df <- model_donneur2_df[c(2,6,3:5)]
rownames(model_donneur2_df) <- c('Donor age','Donor after cardiac death',
                                 'Donor death vascular etiology','Donor history of hypertension',
                                 'Positive Donor CMV serology','Donor Height','Donor Weight')
colnames(model_donneur2_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_donneur2_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Donneur/modele_final_donneur.pdf')





#################################### Greffe

################## Sélection Backward

var_greffe_univ <- c('Tdial','Tabm','incomp2cl')

model_greffe_noabm <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du donneur
                         ageD + card_death + causeDCD2cl +
                         htaD + cmvD + tailleD + poidsD +
                         # Variables de la greffe
                         Tdial + incomp2cl
                       ,data = df)
summary(model_greffe_noabm) 

# Backward

fastbw(coxph2cph(model_greffe_noabm),rule='p',sls=0.05,force=1:7)

model_greffe_noabm <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du donneur
                         ageD + card_death + causeDCD2cl +
                         htaD + cmvD + tailleD + poidsD +
                         # Variables de la greffe
                         incomp2cl + Tdial
                       ,data = df)
summary(model_greffe_noabm) 


model_greffe2_df <- as.data.frame(tidy(model_greffe2))
model_greffe2_df$hr <- exp(model_greffe2_df$estimate)
model_greffe2_df <- model_greffe2_df[c(2,6,3:5)]
rownames(model_greffe2_df) <- c('Donor age','Donor after cardiac death',
                                'Donor death vascular etiology','Donor history of hypertension',
                                'Positive Donor CMV serology','Donor Height','Donor Weight','HLA incompatibilities >=4')
colnames(model_greffe2_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_greffe2_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Greffe/modele_final_greffe.pdf')






#################################### Receveur

################## Sélection Backward

var_rec_univ <- c('ageR','imcR','sexeR','malIni2cl','hemodial','anteDiab','anteDyslip',
                  'anteHTA','anteCardioVasc','anteNeo','cmvR','AcHBsR')

model_rec_noabm <- coxph(Surv(TpsEvtYear, Evt) ~ 
                           # Variables du donneur
                           ageD + card_death + causeDCD2cl +
                           htaD + cmvD + tailleD + poidsD +
                           # Variables de la greffe
                           incomp2cl + Tdial + 
                           # Variables du receveur
                           ageR + imcR + sexeR + malIni2cl + hemodial + anteDiab + anteDyslip +
                           anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR
                         ,data = df)
summary(model_rec_noabm) 

# Backward

fastbw(coxph2cph(model_rec_noabm),rule='p',sls=0.05,force=1:9)

model_rec_noabm <- coxph(Surv(TpsEvtYear, Evt) ~ 
                           # Variables du donneur
                           ageD + card_death + causeDCD2cl +
                           htaD + cmvD + tailleD + poidsD +
                           # Variables de la greffe
                           incomp2cl + Tdial +
                           # Variables du receveur
                           ageR + anteDiab + anteCardioVasc 
                         ,data = df)
summary(model_rec_noabm) 
BIC(model_rec_noabm)

model_rec_noabm <- coxph(Surv(TpsEvtYear, Evt) ~ 
                           # Variables du donneur
                           ageD + card_death + causeDCD2cl +
                           htaD + cmvD + imcD +
                           # Variables de la greffe
                           incomp2cl + Tdial +
                           # Variables du receveur
                           ageR + anteDiab + anteCardioVasc
                         ,data = df)
summary(model_rec_noabm) 
BIC(model_rec_noabm) # On garde avec l'IMC

model_receveur2_df <- as.data.frame(tidy(model_receveur2))
model_receveur2_df$hr <- exp(model_receveur2_df$estimate)
model_receveur2_df <- model_receveur2_df[c(2,6,3:5)]
rownames(model_receveur2_df) <- c('Donor age','Donor after cardiac death',
                                  'Donor death vascular etiology','Donor history of hypertension',
                                  'Positive Donor CMV serology','Donor BMI','HLA incompatibilities >=4',
                                  'Recipient age','History of diabetes','History of cardiovascular disease',
                                  'Hemodialysis')
colnames(model_receveur2_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_receveur2_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Receveur/modele_final_receveur.pdf')





#################################### Retour donneur

################## Sélection Backward

fastbw(coxph2cph(model_rec_noabm),rule='p',sls=0.05,force=8:11)

model_final_noabm <- coxph(Surv(TpsEvtYear, Evt) ~ 
                        # Variables du donneur
                        ageD + card_death + causeDCD2cl +
                        cmvD + imcD +
                        # Variables de la greffe
                        incomp2cl + Tdial +
                        # Variables du receveur
                        ageR + anteDiab + anteCardioVasc
                      ,data = df)
summary(model_final_noabm) 

model_final2_df <- as.data.frame(tidy(model_final2))
model_final2_df$hr <- exp(model_final2_df$estimate)
model_final2_df <- model_final2_df[c(2,6,3:5)]
rownames(model_final2_df) <- c('Donor age','Donor after cardiac death',
                               'Donor death vascular etiology',
                               'Positive Donor CMV serology','Donor BMI','HLA incompatibilities >=4',
                               'Recipient age','History of diabetes','History of cardiovascular disease',
                               'Hemodialysis')
colnames(model_final2_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_final2_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/modele_final_sans_interactions.pdf')


#################################### Interactions var signif

# Liste des candidates
inter_signif2 <- c('incomp2cl:ageD','incomp2cl:imcD',
                   'ageD:ageR','ageR:imcD',
                   'hemodial:ageD','hemodial:imcD','anteDiab:ageD','anteDiab:imcD',
                   'anteCardioVasc:ageD','anteCardioVasc:imcD',
                   'anteNeo:ageD','anteNeo:imcD',
                   'incomp2cl:card_death',
                   'ageR:causeDCD2cl','imcR:card_death','imcR:causeDCD2cl',
                   'hemodial:card_death','hemodial:causeDCD2cl','anteDiab:causeDCD2cl',
                   'anteCardioVasc:causeDCD2cl','anteCardioVasc:card_death','anteNeo:causeDCD2cl',
                   'cmvD:hemodial','cmvD:anteDiab','cmvD:ageR','cmvD:imcR')

# Fonction d'ajout d'interaction au modele

ajout_candidat<-function(modele,candidat)
{
  mod_text <- modele$call %>% deparse() %>% paste(collapse="")
  pos <- regexpr("~",mod_text)
  mod2_text <- paste0(substr(mod_text,1,pos),candidat,"+",substr(mod_text,pos+1,nchar(mod_text)))
  
  out <- tryCatch(mod2_text %>% str2lang %>% eval(), warning=function(w) {return(NA)})
  return(out)
}

# Table univariée
tab_inter_signif2<- round(sapply(inter_signif2,function(x) summary(ajout_candidat(model_complet,x))$coefficients[11,]),3)


tab_inter_signif2 <- t(tab_inter_signif2)
tab_inter_signif2
colnames(tab_inter_signif2) <- c('\\beta','HR','SE(\\beta)','Z','p')

################## Backward

# Construction du modèle enrichi

model_complet_inter3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc + 
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + imcD:ageR + card_death:incomp2cl + causeDCD2cl:ageR + 
                                causeDCD2cl:anteDiab + cmvD:hemodial + cmvD:hemodial + cmvD:anteDiab +
                                cmvD:ageR
                              ,data = df)
summary(model_complet_inter3) 


# Backward

fastbw(coxph2cph(model_complet_inter3),rule='p',force = 1:10,sls=0.05)

model_complet_inter3 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc + anteNeo +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR 
                              ,data = df)
summary(model_complet_inter3) 



#################################### Interactions var signif

# Liste des candidates
inter_notsignif2 <- c('ageD:Tdial','ageD:Tabm','imcD:Tdial','imcD:Tabm','ageD:imcR','imcD:imcR',
                      'ageD:sexeR','ageD:malIni2cl','ageD:anteDyslip','ageD:anteHTA','ageD:cmvR',
                      'ageD:AcHBsR','ageD:antiClassI.jour','ageD:antiClassII.jour',
                      'imcD:sexeR','imcD:malIni2cl','imcD:anteDyslip','imcD:anteHTA','imcD:cmvR',
                      'imcD:antiClassI.jour','imcD:antiClassII.jour',
                      'card_death:Tdial','causeDCD2cl:Tdial','card_death:sexeR','malIni2cl:causeDCD2cl',
                      'causeDCD2cl:anteDyslip','causeDCD2cl:anteHTA','card_death:anteHTA','cmvR:causeDCD2cl',
                      'AcHBsR:causeDCD2cl','antiClassI.jour:card_death','antiClassII.jour:card_death',
                      'antiClassII.jour:causeDCD2cl','cmvD:sexeR','cmvD:malIni2cl','cmvD:anteHTA',
                      'cmvD:anteNeo','cmvD:cmvR','cmvD:Tdial','cmvD:Tabm')

# Fonction d'ajout d'interaction au modele

ajout_candidat<-function(modele,candidat)
{
  mod_text <- modele$call %>% deparse() %>% paste(collapse="")
  pos <- regexpr("~",mod_text)
  mod2_text <- paste0(substr(mod_text,1,pos),candidat,"+",substr(mod_text,pos+1,nchar(mod_text)))
  
  out <- tryCatch(mod2_text %>% str2lang %>% eval(), warning=function(w) {return(NA)})
  return(out)
}

# Table univariée
tab_inter_notsignif2<- round(sapply(inter_notsignif2,function(x) summary(ajout_candidat(model_complet,x))$coefficients[11,]),3)


tab_inter_notsignif2 <- t(tab_inter_notsignif2)
tab_inter_notsignif2
colnames(tab_inter_signif2) <- c('\\beta','HR','SE(\\beta)','Z','p')

################## Backward

# Construction du modèle enrichi

model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tdial + ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + imcD:malIni2cl + card_death:Tdial + causeDCD2cl:Tdial +
                                malIni2cl:causeDCD2cl + causeDCD2cl:anteHTA +cmvD:malIni2cl + cmvD:anteHTA + 
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 




#### Backward manuelle 

# -Age:Tdial
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + imcD:malIni2cl + card_death:Tdial + causeDCD2cl:Tdial +
                                malIni2cl:causeDCD2cl + causeDCD2cl:anteHTA +cmvD:malIni2cl + cmvD:anteHTA + 
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 

# -cmvD:malIni
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + imcD:malIni2cl + card_death:Tdial + causeDCD2cl:Tdial +
                                malIni2cl:causeDCD2cl + causeDCD2cl:anteHTA + cmvD:anteHTA + 
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 

# -causeDC:malIni
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + card_death:Tdial + causeDCD2cl:Tdial +
                                causeDCD2cl:anteHTA + cmvD:anteHTA + 
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 


# -card_death:Tdial
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + causeDCD2cl:Tdial +
                                causeDCD2cl:anteHTA + cmvD:anteHTA + 
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 

# -causeDCD:anteHTA
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + causeDCD2cl:Tdial +
                                cmvD:anteHTA + 
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 

# -cmvD:anteHTA
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + causeDCD2cl:Tdial +
                                cmvD:anteNeo + cmvD:cmvR
                              ,data = df)
summary(model_complet_inter4) 


# -cmvD:cmvR
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + causeDCD2cl:Tdial +
                                cmvD:anteNeo 
                              ,data = df)
summary(model_complet_inter4) 


# -cmvD:anteNeo
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                ageD:malIni2cl + causeDCD2cl:Tdial
                              ,data = df)
summary(model_complet_inter4) 

# -ageD:malIni
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm +
                                causeDCD2cl:Tdial
                              ,data = df)
summary(model_complet_inter4) 

# -causeDC:Tdial
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tdial + imcD:Tabm
                              ,data = df)
summary(model_complet_inter4) 

# -imcD:Tdial
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm + imcD:Tabm
                              ,data = df)
summary(model_complet_inter4) 

# -imcD:Tabm
model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD +
                                # Interactions des variables du modèle
                                ageD:ageR + 
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm
                              ,data = df)
summary(model_complet_inter4) 

################## Modele retenu

model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD + 
                                # Interactions des variables du modèle
                                ageD:ageR  +
                                # Interactions avec var R/G hors modèle
                                ageD:Tabm
                              ,data = df)
summary(model_complet_inter4) 
BIC(model_complet_inter4) 

# Tabm vs Tdial ?

model_complet_inter4 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                # Variables du receveur
                                ageR + hemodial +
                                anteDiab + anteCardioVasc +
                                # Variables de la greffe
                                incomp2cl + 
                                # Variables du donneur
                                ageD + imcD + card_death + causeDCD2cl + cmvD + 
                                # Interactions des variables du modèle
                                ageD:ageR  +
                                # Interactions avec var R/G hors modèle
                                ageD:Tdial
                              ,data = df)
summary(model_complet_inter4) 
BIC(model_complet_inter4) # On garde Tdial






# Export du modèle de base en pdf

model_complet_inter4_df <- as.data.frame(tidy(model_complet_inter4))
model_complet_inter4_df$hr <- exp(model_complet_inter4_df$estimate)
model_complet_inter4_df <- model_complet_inter4_df[c(2,6,3:5)]
rownames(model_complet_inter4_df) <- c('Recipient age','Hemodialysis',
                                       'History of diabetes','History of cardiovascular disease',
                                       'HLA A-B-DR incompatibilities>=4','Donor age','Donor BMI','Donor after cardiac death','Donor death vascular etiology',
                                       'Donor CMV serology positive','Recipient age:Donor age','Donor age:Time on dialysis')
colnames(model_complet_inter4_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_complet_inter4_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2//modele_final_avec_interactions.pdf')





