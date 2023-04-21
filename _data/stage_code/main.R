#####################################################################################################################
# IMPORT
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
library(tidyr) # Data management
library(survival) # Analyses de survie
library(survminer) # Analyse de survie avancée
library(lmtest) # Tests de rapport de vraisemblance
library(shrink) # Shrinkage
library(StepReg) # Stepwise cox
library(rms) # Backward amélioré pour cph
library(base.rms) # Conversion coxph vers cph
library(glmnet) # Régression LASSO
library(tidyverse) # Data management courbes ROC 
library(survivalROC) # Courbes ROC temps dépendante
library(survival.calib) # Calibration pour modèles de survie
library(riskRegression) # Prédiction des risques

## Paramètres

# Centrage des titres GGplot
theme_update(plot.title = element_text(hjust = 0.5))

## Import

df <- read.csv2("~/Documents/Stage/Data/baseScoreD.csv", stringsAsFactors=TRUE)
df_vaso <- read.csv2("~/Documents/Stage/Data/baseScoreD_2023.04.csv", stringsAsFactors=TRUE)
df_vaso <- df_vaso[,c('clef','drogVasoD')]
View(df)

## Conversion en facteurs

df$hemodial <- df$techEpu3cl==3
df$perito <- df$techEpu3cl==2
df$preemptive <- df$techEpu3cl==1
df$card_death <- df$typeD3cl==2
df$deplet <- df$trtIndu3cl==2
df$before_2008 <- df$yearG<=2008

names_fac <- c('centre','sexeR','imcR2cl','imcR3cl','malIni2cl','malIni5cl','techEpu','techEpu2cl',
               'techEpu3cl','machPerf','nbdial2cl','greffe1.2cl','greffe2.2cl','greffe3.2cl','greffe4.2cl',
               'anteDiab','anteDyslip','anteHTA','anteVascu','anteCard','anteCardioVasc','anteNeo','anteUro',
               'anteGrossess','cmvR','ebvR','hcvR','AgHBsR','AcHBsR','hivR','cmvD','ebvD','hcvD','AgHBsD','AcHBsD','hivD',
               'sexeD','typeD','typeD3cl','imcD2cl','imcD3cl','causeDCD2cl','htaD','diabD','dyslipD','arretD',
               'creatD2cl','protuD','hematurieD','incomp2cl','incompABO','trtIndu3cl','CNI','CSA','tacro','mTOR',
               'sirolimus','everolimus','apparentCNI','antiprolif','MMF','MPA','AZA','trtcort','antiClassI.jour','antiClassII.jour',
               'DSA.jour','hemodial','perito','preemptive','card_death','deplet','before_2008')

df[,names_fac] <- lapply(df[,names_fac] , factor)
str(df)

## Split validation/apprentissage

set.seed(14062000)
app <- sample(1:7266,replace=F,size=7266*2/3)
df_valid <- df[-app,] # Echantillon de validation
df_descr <- df # dataframe pour le tableau descriptif
df_descr$is_valid <- (df$clef %in% df_valid$clef)*1 # colonne indicatrice de l'échantillon de validation
df_descr$is_valid <- factor(df_descr$is_valid,labels = c("Apprentissage",'Validation'))
df <- df[app,] # Echantillon d'apprentissage
df <- merge(df,df_vaso,by='clef',all.x=T,all.y=F)
df_valid <- merge(df_valid,df_vaso,by='clef',all.x=T,all.y=F)
set.seed(NULL)

df$drogVasoD <- factor(df$drogVasoD)
df_valid$drogVasoD <- factor(df_valid$drogVasoD)

df$nantes <- df$centre == 'divat nantes'
df$montpellier <- df$centre == 'divat montpellier'
df$nice <- df$centre == 'divat nice'
df$nancy <- df$centre == 'divat nancy'
df$lyon <- df$centre == 'divat lyon'
df$sainte <- df$centre == 'divat saint etienne'

#####################################################################################################################
# VERIFICATION DE COHERENCE ET CRITERES D'INCLUSION
#####################################################################################################################

### Criteres d'inclusion - OK

# Centres: Lyon, Montpellier, Nancy, Nantes, Nice, St-Etienne - OK
table(df$centre)

# Type de greffe: Rein - OK
table(df$typeG)

# Date de greffe 01/01/2000 - 31/12/2022 - OK
summary(df$yearG)

# Première greffe uniquement - OK
table(df$rangG)

# Receveur adulte - OK
summary(df$ageR)

# Donneur cadavérique - OK
table(df$typeD)



### Cohérence

# Modalités prévues pour les var catégorielles



# Valeurs cohérentes pour les var continues


#####################################################################################################################
# DESCRIPTION DE L'EXTRACTION
#####################################################################################################################

# Patients par centres
table(df$Evt,df$centre)

# Distribution de l'âge receveur
par(mfrow=c(2,1))
hist(df$ageR,xlab='Äge (en années)',ylab='Effectif',main='Âge du receveur',col='salmon4')

# Distribution de l'âge donneur
hist(df[df$yearG <= 2008,]$ageD,xlab='Äge (en années)',ylab='Effectif',main='Âge du donneur pré-2008',col='lightblue4',xlim=c(0,100))


hist(df[df$yearG > 2008,]$ageD,xlab='Äge (en années)',ylab='Effectif',main='Âge du donneur post-2008',col='lightblue4',xlim=c(0,100))
par(mfrow=c(1,1))

# Distribution du critère ECD
table(df$ecdscd)/nrow(df[!is.na(df$ecdscd),])

# Année de greffe
hist(df$yearG,xlab='Ännée de greffe',ylab='Effectif',main='Ânnées de greffe',col='salmon4',breaks=20)


#####################################################################################################################
# TABLE DESCRIPTIVE
#####################################################################################################################

################## Préparation

## Table mise en forme

df$tabac <- df$anteTabac %in% c('Oui, non sevre','Oui, sevre')
df_valid$tabac <- df_valid$anteTabac %in% c('Oui, non sevre','Oui, sevre')

# Liste des variables et label

var_list_descr_quali <- c('sexeR','malIni2cl','preemptive','perito','hemodial',
                          'nbdial2cl','tabac',
                          'anteDiab','anteDyslip','anteHTA','anteCardioVasc','anteNeo','anteUro',
                          'cmvR','AcHBsR','cmvD',
                          'sexeD','card_death','causeDCD2cl','htaD','diabD','arretD',
                          'protuD','deplet','antiClassI.jour','antiClassII.jour','before_2008','drogVasoD')

var_list_descr_quanti <- c('ageR','imcR','Tabm','Tdial','iscHeure','ageD','tailleD','poidsD',
                           'diureseD','ureeD','creatD','incompABDR')
var_list_descr <- c(var_list_descr_quanti,var_list_descr_quali)


var_label_descr_quanti <- c('Recipient age (years)','Recipient BMI kg/m²','Time on waiting list (days)','Time on dialysis (days)','CIT (hours)',
                            'Donor age (years)','Donor height (cm)','Donor weight (kg)','Last donor diuresis (mL)','Donor urea (mmol/L)','Donor creatinine (µmol/L)','HLA A-B-DR incompatibilities')


var_label_descr_quali <- c('Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis',
                           'Post-transplant dialysis','History of smoking',
                           'History of diabetes','History of Dyslipidemia','History of hypertension',
                           'History of cardiovascular disease','History of neoplasia','History of urological disease',
                           'Positive recipient CMV serology','Positive recipient AcHBs serology',
                           'Positive donor CMV serology',
                           'Donor men','Donor after cardiac death','Donor death vascular etiology','Donor history of hypertension',
                           'Donor history of diabetes','Donor history of cardiac arrest',
                           'Donor proteinuria positive','Depleting induction therapy',
                           'Pre-transplantation anti-HLA immunization of class I',
                           'Pre-transplantation anti-HLA immunization of class II','Transplanted before 2008','Donor treated by vasopressor')


# fonction 1er et 2e quartiles

quart25 <- function(y) quantile(y,c(0.25),na.rm=T) 
quart75 <- function(y) quantile(y,c(0.75),na.rm=T) 

################## Table Quantitative

# Liste de valeurs descriptives avant mise en forme QUANTITATIVE

tab_quanti_unform <- data.frame(Variable=var_label_descr_quanti,
                                Missing_learn=sapply(df[,var_list_descr_quanti], function(y) sum(length(which(is.na(y))))),
                                mean_learn=round(sapply(df[,var_list_descr_quanti],mean,na.rm=T),1),
                                sd_learn=round(sapply(df[,var_list_descr_quanti],sd,na.rm=T),1),
                                q1_learn=round(sapply(df[,var_list_descr_quanti], quart25 ),1),
                                q3_learn=round(sapply(df[,var_list_descr_quanti], quart75 ),1),
                                Missing_valid=sapply(df_valid[,var_list_descr_quanti], function(y) sum(length(which(is.na(y))))),
                                mean_valid=round(sapply(df_valid[,var_list_descr_quanti],mean,na.rm=T),1),
                                sd_valid=round(sapply(df_valid[,var_list_descr_quanti],sd,na.rm=T),1),
                                q1_valid=round(sapply(df_valid[,var_list_descr_quanti], quart25 ),1),
                                q3_valid=round(sapply(df_valid[,var_list_descr_quanti], quart75 ),1)
) 
tab_quanti_unform

# Mise en forme QUANTITATIVE

tab_quanti_unform$meansd_learn <- with(tab_quanti_unform, paste(mean_learn,sd_learn, sep=" ± "))
tab_quanti_unform$meansd_valid <- with(tab_quanti_unform, paste(mean_valid,sd_valid, sep=" ± "))

tab_quanti_unform$q1_learn <- with(tab_quanti_unform, paste('(',q1_learn, sep=""))
tab_quanti_unform$q3_learn <- with(tab_quanti_unform, paste(q3_learn,')', sep=""))
tab_quanti_unform$q1q3_learn <- with(tab_quanti_unform, paste(q1_learn,q3_learn, sep="-"))

tab_quanti_unform$q1_valid <- with(tab_quanti_unform, paste('(',q1_valid, sep=""))
tab_quanti_unform$q3_valid <- with(tab_quanti_unform, paste(q3_valid,')', sep=""))
tab_quanti_unform$q1q3_valid <- with(tab_quanti_unform, paste(q1_valid,q3_valid, sep="-"))

tab_quanti_unform$descript_learn <- with(tab_quanti_unform, paste(meansd_learn,q1q3_learn, sep="<br>"))
tab_quanti_unform$descript_valid <- with(tab_quanti_unform, paste(meansd_valid,q1q3_valid, sep="<br>"))


# Table mise en forme QUANTITATIVE

tab_quanti <- tab_quanti_unform[,c('Missing_learn','descript_learn','Missing_valid','descript_valid')]
tab_quanti
tab_quanti <- rbind(c('','','',''),tab_quanti) # Ligne vide pour les titres dans la table







################## Table Qualittative

# Liste de valeurs descriptives avant mise en forme QUALITATIVE

tab_quali_unform <- data.frame(Variable=var_label_descr_quali,
                               Missing_learn=sapply(df[,var_list_descr_quali], function(y) sum(length(which(is.na(y))))),
                               N_learn=sapply(df[,var_list_descr_quali], function(y) sum(length(which(y==TRUE | y==1 | y=='ECD')))),
                               Percent_learn=round(sapply(df[,var_list_descr_quali], function(y) sum(length(which(y==TRUE | y==1 | y=='ECD')))/nrow(df)),4)*100,
                               Missing_valid=sapply(df_valid[,var_list_descr_quali], function(y) sum(length(which(is.na(y))))),
                               N_valid=sapply(df_valid[,var_list_descr_quali], function(y) sum(length(which(y==TRUE | y==1 | y=='ECD')))),
                               Percent_valid=round(sapply(df_valid[,var_list_descr_quali], function(y) sum(length(which(y==TRUE | y==1 | y=='ECD')))/nrow(df_valid)),4)*100
) 
tab_quali_unform

# Mise en forme QUALITATIVE
tab_quali_unform$Percent_learn <- with(tab_quali_unform, paste('(',Percent_learn,')', sep=""))
tab_quali_unform$Percent_valid <- with(tab_quali_unform, paste('(',Percent_valid,')', sep=""))

tab_quali_unform$descript_learn <- with(tab_quali_unform, paste(N_learn,Percent_learn, sep="<br>"))
tab_quali_unform$descript_valid <- with(tab_quali_unform, paste(N_valid,Percent_valid, sep="<br>"))

# Table mise en forme QUANTITATIVE

tab_quali <- tab_quali_unform[,c('Missing_learn','descript_learn','Missing_valid','descript_valid')]
tab_quali
tab_quali <- rbind(c('','','',''),tab_quali) # Ligne vide pour les titres dans la table

################## Table complète

table_descr <- rbind(tab_quanti,tab_quali)
table_descr

################# Sortie PDF

knit_table <- function(df){
  if (is_html_output()) {
    df %>%
      kable("html", escape = F,col.names=c('Variable','Missing (learning sample)','Learning sample (N=4844)','Missing (validation sample)','Validation sample (N=2778)')) %>%
      kable_styling()
  } else {
    df <- data.frame(lapply(df, function(x) {gsub("<br>", "\n", x)}), stringsAsFactors = F)
    
    df %>%  
      mutate_all(linebreak) %>%
      kable("latex", booktabs = T, escape = F,col.names=c('','Missing (learning sample)','Learning sample (N=4844)','Missing (validation sample)','Validation sample (N=2778)'))  %>%
      row_spec(1,bold=T) %>%
      row_spec(14,bold=T) %>%
      kable_styling('striped')
  }
}

table_descr2 <- table_descr
rownames(table_descr2) <- c('Quantitative characteristics - mean ± SD (Q1-Q3)',var_label_descr_quanti,"Categorical characteristics - N (\\%)",var_label_descr_quali)
table_descr2$Variable <- rownames(table_descr2)
table_descr2 <- table_descr2[,c(5,1:4)]
table_descr_knit <- knit_table(table_descr2)

table_descr_knit <- gsub("[r]", "[l]", table_descr_knit,fixed=T)
table_descr_knit <- gsub("[c]", "[l]", table_descr_knit,fixed=T)
table_descr_knit

table_descr_knit <- table_descr_knit %>% 
  kable_styling(latex_options="scale_down")

save_kable(table_descr_knit,file='/home/corentin/Documents/Stage/Projet R/Tables/Descriptif/tab_descr.pdf')

################# Autres manipulations

nrow(df[df$sexeR==0 & is.na(df$anteGrossess),])/nrow(df[df$sexeR==0,]) # Proportion de NA en antécédent de grossesse chez les femmes, échantillon d'apprentissage
nrow(df_valid[df_valid$sexeR==0 & is.na(df_valid$anteGrossess),])/nrow(df_valid[df_valid$sexeR==0,]) # Proportion de NA en antécédent de grossesse chez les femmes, échantillon de validation

sum(is.na(df$TpsEvt)) # Aucun temps d'évènement manquant
sum(is.na(df$Evt)) # Aucune indicatrice d'évènement manquante

hist_uree <- plot_ly(x = df$ureeD, type = "histogram") # Ecart type suspect sur urée => outliers ?
hist_uree
nrow(df[df$ureeD>200 & !is.na(df$ureeD),])
nrow(df[df$ureeD<25 & !is.na(df$ureeD),])

nrow(df[df$tacro==1 & df$yearG>2008,])/nrow(df[df$yearG>2008,]) # Tacrolimus après 2008

table(df$centre)
table(df_valid$centre)

#####################################################################################################################
# DESCRIPTION DE LA SURVIE
#####################################################################################################################

################## Data management
## Temps de survie en année
df$TpsEvtYear <- df$TpsEvt/365.25
df_valid$TpsEvtYear <- df_valid$TpsEvt/365.25

################## Kaplan-Meier global

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df)

# Points
options(max.print = 120000)
sink(file = "surv.txt") # Sortie de l'output en texte
summary(Obj.Surv)
sink(file=NULL)
timelist <- c(5,10,15,20)
probsurv <- c(0.782,0.558,0.393,0.256)
dftt <- data.frame(timelist=timelist,probsurv=probsurv,label=c("78.2%","55.8%","39.3%",'25.6%'))


# Graphique
pglob <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                    risk.table.y.text.col = T,
                    risk.table.y.text = FALSE,
                    linetype = c("solid"),
                    risk.table = T,
                    title = "Courbe de survie globale",
                    risk.table.title = 'Effectif à risque',
                    xlab = "Temps post-greffe (Années)",
                    ylab = "Probabilité de survie",
                    xlim = c(0, as.integer(max(df$TpsEvtYear))+1),
                    ylim = c(0,1),
                    censor = F,
                    surv.scale = "default",
                    fontsize = 3,
                    tables.theme = theme_light(),
                    break.time.by = 1,
                    risk.table.height=0.3,
                    conf.int = T,
                    legend.labs=c('Global'))


ppglob <- pglob$plot + geom_point(aes(x=timelist, y=probsurv), data = dftt) + geom_text_repel(aes(x=timelist, y=probsurv,label=label),vjust= -2, data = dftt) +
  theme(plot.title = element_text(hjust = 0.5))
ptglob <- pglob$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppglob,ptglob),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))




################## Kaplan-Meier par centre

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ centre, data = df)

# Points
options(max.print = 120000)
sink(file = "surv.txt") # Sortie de l'output en texte
summary(Obj.Surv)
sink(file = NULL)
timelist <- c(5,10,5,10,5,10,5,10,5)
probsurv <- c(0.7603,0.3310,0.810,0.603,0.773,0.598,0.768,0.536,0.6477)
dftt <- data.frame(timelist=timelist,probsurv=probsurv,label=c('76.0%','33.1%','81.0%','60.3%','77.3%','59.8%','76.8%','53.6%','64.8%'))

# Graphique
pcentre <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                      risk.table.y.text.col = T,
                      risk.table.y.text = FALSE,
                      linetype = c("solid", "solid","solid","solid","solid","solid"),
                      risk.table = T,
                      title = "Courbe de survie",
                      risk.table.title = 'Effectif à risque',
                      xlab = "Temps post-greffe (Années)",
                      ylab = "Probabilité de survie",
                      xlim = c(0, as.integer(max(df$TpsEvtYear))+1),
                      ylim = c(0,1),
                      censor = F,
                      surv.scale = "default",
                      fontsize = 3,
                      tables.theme = theme_light(),
                      break.time.by = 1,
                      risk.table.height=0.3,
                      conf.int = F,
                      legend.labs=c('Lyon','Montpellier','Nancy','Nantes','Nice','St-Etienne'))

ppcentre <- pcentre$plot + geom_point(aes(x=timelist, y=probsurv), data = dftt) + geom_text_repel(aes(x=timelist, y=probsurv,label=label),vjust= -2, data = dftt) +
  theme(plot.title = element_text(hjust = 0.5))
ptcentre <- pcentre$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre,ptcentre),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))


################## Kaplan-Meier tronqués

## St-Etienne

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df[df$sainte==1,])

# Graphique
pcentre1 <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                      risk.table.y.text.col = T,
                      risk.table.y.text = FALSE,
                      linetype = c("solid"),
                      risk.table = T,
                      title = "Courbe de survie",
                      risk.table.title = 'Effectif à risque',
                      xlab = "Temps post-greffe (Années)",
                      ylab = "Probabilité de survie",
                      xlim = c(0,1),
                      ylim = c(0,1),
                      censor = F,
                      surv.scale = "default",
                      fontsize = 3,
                      tables.theme = theme_light(),
                      break.time.by = 0.2,
                      risk.table.height=0.3,
                      conf.int = F,
                      legend.labs=c('St-Etienne'))

ppcentre1 <- pcentre1$plot + 
  theme(plot.title = element_text(hjust = 0.5))
ptcentre1 <- pcentre1$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre1,ptcentre1),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))


## Nice

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df[df$nice==1,])

# Graphique
pcentre2 <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                       risk.table.y.text.col = T,
                       risk.table.y.text = FALSE,
                       linetype = c("solid"),
                       risk.table = T,
                       title = "Courbe de survie",
                       risk.table.title = 'Effectif à risque',
                       xlab = "Temps post-greffe (Années)",
                       ylab = "Probabilité de survie",
                       xlim = c(0,4),
                       ylim = c(0,1),
                       censor = F,
                       surv.scale = "default",
                       fontsize = 3,
                       tables.theme = theme_light(),
                       break.time.by = 0.5,
                       risk.table.height=0.3,
                       conf.int = F,
                       legend.labs=c('Nice'))

ppcentre2 <- pcentre2$plot + 
  theme(plot.title = element_text(hjust = 0.5))
ptcentre2 <- pcentre2$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre2,ptcentre2),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))


## Nantes

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df[df$nantes==1,])

# Graphique
pcentre3 <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                       risk.table.y.text.col = T,
                       risk.table.y.text = FALSE,
                       linetype = c("solid"),
                       risk.table = T,
                       title = "Courbe de survie",
                       risk.table.title = 'Effectif à risque',
                       xlab = "Temps post-greffe (Années)",
                       ylab = "Probabilité de survie",
                       xlim = c(0,19.5),
                       ylim = c(0,1),
                       censor = F,
                       surv.scale = "default",
                       fontsize = 3,
                       tables.theme = theme_light(),
                       break.time.by = 1,
                       risk.table.height=0.3,
                       conf.int = F,
                       legend.labs=c('Nantes'))

ppcentre3 <- pcentre3$plot + 
  theme(plot.title = element_text(hjust = 0.5))
ptcentre3 <- pcentre3$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre3,ptcentre3),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))


## Nancy

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df[df$nancy==1,])

# Graphique
pcentre4 <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                       risk.table.y.text.col = T,
                       risk.table.y.text = FALSE,
                       linetype = c("solid"),
                       risk.table = T,
                       title = "Courbe de survie",
                       risk.table.title = 'Effectif à risque',
                       xlab = "Temps post-greffe (Années)",
                       ylab = "Probabilité de survie",
                       xlim = c(0,19.5),
                       ylim = c(0,1),
                       censor = F,
                       surv.scale = "default",
                       fontsize = 3,
                       tables.theme = theme_light(),
                       break.time.by = 1,
                       risk.table.height=0.3,
                       conf.int = F,
                       legend.labs=c('Nancy'))

ppcentre4 <- pcentre4$plot + 
  theme(plot.title = element_text(hjust = 0.5))
ptcentre4 <- pcentre4$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre4,ptcentre4),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))


## Montpellier

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df[df$montpellier==1,])

# Graphique
pcentre5 <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                       risk.table.y.text.col = T,
                       risk.table.y.text = FALSE,
                       linetype = c("solid"),
                       risk.table = T,
                       title = "Courbe de survie",
                       risk.table.title = 'Effectif à risque',
                       xlab = "Temps post-greffe (Années)",
                       ylab = "Probabilité de survie",
                       xlim = c(0,19.5),
                       ylim = c(0,1),
                       censor = F,
                       surv.scale = "default",
                       fontsize = 3,
                       tables.theme = theme_light(),
                       break.time.by = 1,
                       risk.table.height=0.3,
                       conf.int = F,
                       legend.labs=c('Montpellier'))

ppcentre5 <- pcentre5$plot + 
  theme(plot.title = element_text(hjust = 0.5))
ptcentre5 <- pcentre5$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre5,ptcentre5),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))


## Lyon

# Objet survie
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df[df$lyon==1,])

# Graphique
pcentre6 <- ggsurvplot(fit = Obj.Surv, data = df, pval = F, 
                       risk.table.y.text.col = T,
                       risk.table.y.text = FALSE,
                       linetype = c("solid"),
                       risk.table = T,
                       title = "Courbe de survie",
                       risk.table.title = 'Effectif à risque',
                       xlab = "Temps post-greffe (Années)",
                       ylab = "Probabilité de survie",
                       xlim = c(0,7.5),
                       ylim = c(0,1),
                       censor = F,
                       surv.scale = "default",
                       fontsize = 3,
                       tables.theme = theme_light(),
                       break.time.by = 1,
                       risk.table.height=0.3,
                       conf.int = F,
                       legend.labs=c('Lyon'))

ppcentre6 <- pcentre6$plot + 
  theme(plot.title = element_text(hjust = 0.5))
ptcentre6 <- pcentre6$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(ppcentre6,ptcentre6),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))








################## Indicateurs descriptifs

##### Nombre d'évènements

# Apprentissage
nb.RD<-sum(df$Retour==1) # 977 retours en dialyse
nb.DC<-sum(df$Deces==1) # 1010 décès
nb.DCFct<-sum(df$DecesFct==1) # 757 décès avec greffon fonctionnel
nb.Cens<-sum(df$Evt==0) # 3110 censures

nb.RD; nb.DC; nb.Cens; nb.DCFct

nb.DCFct/nb.DC # 75% de décès avec greffon fonctionnel

# Validation
nb.RDv<-sum(df_valid$Retour==1) # 510 retours en dialyse
nb.DCv<-sum(df_valid$Deces==1) # 507 décès
nb.DCFctv<-sum(df_valid$DecesFct==1) # 375 décès avec greffon fonctionnel
nb.Censv<-sum(df_valid$Evt==0) # 1893 censures

nb.RDv; nb.DCv; nb.Censv; nb.DCFctv

nb.DCFctv/nb.DCv # 74% de décès avec greffon fonctionnel

##### Personnes-années

# Apprentissage
PersAnn <- round(sum(df$TpsEvtYear),0) 
PersAnn

# Validation
PersAnnv <- round(sum(df_valid$TpsEvtYear),0) 
PersAnnv

##### Incidence en personne-année

# Apprentissage
nb.RD/PersAnn; nb.DC/PersAnn; nb.DCFct/PersAnn # Deces/ RD par personne-année

# Validation
nb.RDv/PersAnnv; nb.DCv/PersAnnv; nb.DCFctv/PersAnnv # Deces/ RD par personne-année

##### Follow-up médian

# Apprentissage
Obj.Surv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df) # Objet survie
sink(file = "surv.txt") # Sortie de l'output en texte
summary(Obj.Surv) # 11.73 ans
sink(file = NULL)

med_follow_up <- 11.74812

# Validation
Obj.Survv<-survfit(Surv(TpsEvtYear, Evt) ~ 1, data = df_valid) # Objet survie
sink(file = "surv.txt") # Sortie de l'output en texte
summary(Obj.Survv)
sink(file = NULL)

med_follow_upv <- 11.96441


##### Probas de survie

## Apprentissage
sink(file = "surv.txt") # Sortie de l'output en texte
summary(Obj.Surv) 
sink(file = NULL)
# 5 ans: 78% [77%-79%] 
# 10 ans: 56% [54%-58%] 
# 15 ans: 39% [37%-42%] 
# 20 ans: 26% [23%-29%] 

## Validation
sink(file = "surv.txt") # Sortie de l'output en texte
summary(Obj.Survv) 
sink(file = NULL)
# 5 ans: 79% [77%-80%] 
# 10 ans: 58% [56%-61%] 
# 15 ans: 40% [37%-43%] 
# 20 ans: 25% [21%-29%] 



#####################################################################################################################
# SCREENING UNIVARIE DES INTERACTIONS
#####################################################################################################################

################## Listes variables

# Quantitatif
var_univ_donneur_quanti <- c('ageD','tailleD','poidsD','diureseD','ureeD','creatD')

var_univ_receveur_quanti <- c('ageR','imcR')

var_univ_greffe_quanti <- c('iscHeure','incompABDR','Tdial','Tabm')


# Qualitatif
var_univ_donneur_quali <- c('sexeD','card_death','causeDCD2cl','htaD','diabD','arretD','protuD','cmvD')

var_univ_receveur_quali <- c('sexeR','malIni2cl','preemptive','perito','hemodial','anteDiab','anteDyslip',
                             'anteHTA','anteCardioVasc','anteNeo','anteUro','cmvR','AcHBsR',
                             'antiClassI.jour','antiClassII.jour')



################## Quanti-Quanti

# Donneur - Receveur
dr_univ_quanti <- round(sapply(df[,var_univ_donneur_quanti],function(x) sapply(df[,var_univ_receveur_quanti],function(y) summary(lm(x~y))$coefficients[8])),3)
rownames(dr_univ_quanti) <- c('Recipient age','Recipient BMI')
colnames(dr_univ_quanti) <- c('Donor age','Donor height','Donor weight','Last donor diuresis','Donor urea','Donor creatinine')

# Donneur - greffe
dg_univ_quanti <- round(sapply(df[,var_univ_donneur_quanti],function(x) sapply(df[,var_univ_greffe_quanti],function(y) summary(lm(x~y))$coefficients[8])),3)
colnames(dg_univ_quanti) <- c('Donor age','Donor height','Donor weight','Last donor diuresis','Donor urea','Donor creatinine')
rownames(dg_univ_quanti) <- c('CIT','HLA A-B-DR incompatibilities','Time on dialysis','Time on waiting list')


# Sauvegarde en PDF
dr_univ_quanti %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_qanti_DR.pdf')

dg_univ_quanti %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_qanti_DG.pdf')



################## Quanti-Quali

# Donneur Quanti - Receveur Quali
dr_univ_quanti_quali <- round(sapply(df[,var_univ_donneur_quanti],function(x) sapply(df[,var_univ_receveur_quali],function(y) summary(lm(x~y))$coefficients[8])),3)
colnames(dr_univ_quanti_quali) <- c('Donor age','Donor height','Donor weight','Last donor diuresis','Donor urea','Donor creatinine')
rownames(dr_univ_quanti_quali) <- c('Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis',
                                    'History of diabetes','History of Dyslipidemia','History of hypertension',
                                    'History of cardiovascular disease','History of neoplasia','History of urological disease',
                                    'Positive recipient CMV serology','Positive recipient AcHBs serology','Pre-transplantation anti-HLA immunization of class I',
                                    'Pre-transplantation anti-HLA immunization of class II')

# Donneur Quali - Receveur Quanti
rd_univ_quanti_quali <- round(sapply(df[,var_univ_receveur_quanti],function(x) sapply(df[,var_univ_donneur_quali],function(y) summary(lm(x~y))$coefficients[8])),3)
colnames(rd_univ_quanti_quali) <- c('Recipient age','Recipient BMI')
rownames(rd_univ_quanti_quali) <- c('Donor men','Donor after cardiac death','Donor death vascular etiology',
                                    'Donor history of hypertension','Donor history of diabetes',
                                    'Donor history of cardiac arrest','Donor proteinuria positive','Donor CMV serology positive')

# Donneur Quali - Greffe Quanti
gd_univ_quanti_quali <- round(sapply(df[,var_univ_greffe_quanti],function(x) sapply(df[,var_univ_donneur_quali],function(y) summary(lm(x~y))$coefficients[8])),3)
colnames(gd_univ_quanti_quali) <- c('CIT','HLA A-B-DR incompatibilities','Time on dialysis','Time on waiting list')
rownames(gd_univ_quanti_quali) <- c('Donor men','Donor after cardiac death','Donor death vascular etiology',
                                    'Donor history of hypertension','Donor history of diabetes',
                                    'Donor history of cardiac arrest','Donor proteinuria positive','Donor CMV serology positive')


# Sauvegarde en PDF
dr_univ_quanti_quali %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_qanti_quali_DR.pdf')

rd_univ_quanti_quali %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_qanti_quali_RD.pdf')

gd_univ_quanti_quali %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_qanti_quali_GD.pdf')



################## Quali-Quali

# Donneur - Receveur
dr_univ_quali <- round(sapply(df[,var_univ_donneur_quali],function(x) sapply(df[,var_univ_receveur_quali],function(y) chisq.test(table(x,y))$p.value)),3)
colnames(dr_univ_quali) <- c('Donor men','Donor after cardiac death','Donor death vascular etiology',
                             'Donor history of hypertension','Donor history of diabetes',
                             'Donor history of cardiac arrest','Donor proteinuria positive','Donor CMV serology positive')
rownames(dr_univ_quali) <- c('Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis',
                             'History of diabetes','History of Dyslipidemia','History of hypertension',
                             'History of cardiovascular disease','History of neoplasia','History of urological disease',
                             'Positive recipient CMV serology','Positive recipient AcHBs serology','Pre-transplantation anti-HLA immunization of class I',
                             'Pre-transplantation anti-HLA immunization of class II')


# Sauvegarde en PDF
dr_univ_quali %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_quali_DR.pdf')


#####################################################################################################################
# SELECTION VARIABLES DONNEUR
#####################################################################################################################

#################################### Table univariée

var_donneur <- c(var_univ_donneur_quanti,var_univ_donneur_quali,'imcD')
var_greffe <- c('iscHeure','Tdial','Tabm','incomp2cl')
var_receveur <- c(var_univ_receveur_quanti,var_univ_receveur_quali)

var_all <- c(var_receveur,var_greffe,var_donneur)


# Génération de la table
tab_back_glob <- round(sapply(df[,var_all],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~x))$coefficients),3)
tab_back_glob <- t(tab_back_glob)
colnames(tab_back_glob) <- c('\\beta','HR','SE(\\beta)','Z','p')

# Export en PDF

tab_back_glob %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Univarié/table_univarie_globale.pdf')


#################################### Hypothèses

## Fonction log-log
log.minus.log<-function(y) { 
  log(-log(y)) 
}


################## Log-linéarité

## Age donneur -- OK
q25 <- quantile(df$ageD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$ageD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~ageD,data=df[df$ageD>q25 & df$ageD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$ageD>q25 & df$ageD<q975 & !is.na(df$ageD),]$ageD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Age Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite

## Taille Donneur -- OK
q25 <- quantile(df$tailleD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$tailleD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~tailleD,data=df[df$tailleD>q25 & df$tailleD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$tailleD>q25 & df$tailleD<q975 & !is.na(df$tailleD),]$tailleD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Taille Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite


## Poids Donneur -- OK
q25 <- quantile(df$poidsD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$poidsD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~poidsD,data=df[df$poidsD>q25 & df$poidsD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$poidsD>q25 & df$poidsD<q975 & !is.na(df$poidsD),]$poidsD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Poids Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite

## IMC -- Transfo
q25 <- quantile(df$imcD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$imcD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~imcD,data=df[df$imcD>q25 & df$imcD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$imcD>q25 & df$imcD<q975 & !is.na(df$imcD),]$imcD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "IMC Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite




## Diurese -- OK
q25 <- quantile(df$diureseD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$diureseD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~diureseD,data=df[df$diureseD>q25 & df$diureseD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$diureseD>q25 & df$diureseD<q975 & !is.na(df$diureseD),]$diureseD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Diurese Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite




## Uree -- OK
q25 <- quantile(df$ureeD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$ureeD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~ureeD,data=df[df$ureeD>q25 & df$ureeD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$ureeD>q25 & df$ureeD<q975 & !is.na(df$ureeD),]$ureeD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Diurese Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite





## Creat -- OK
q25 <- quantile(df$creatD,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$creatD,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~creatD,data=df[df$creatD>q25 & df$creatD<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$creatD>q25 & df$creatD<q975 & !is.na(df$creatD),]$creatD, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Diurese Donneur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite


################## Proportionnalité des risques

## résidus de Schoenfeld 
par(mfrow=c(4,4))

list_plot_schoen <- lapply(var_donneur, function(x) {plot(main=x,cox.zph(coxph(Surv(df$TpsEvtYear,df$Evt)~df[,x]) ),col=c(1,2,2,2)) 
  abline(h=0,col=4)})

# Saint-etienne card_death ?
plot(main='card_death',cox.zph(coxph(Surv(df[df$sainte==0,]$TpsEvtYear,df[df$sainte==0,]$Evt)~df[df$sainte==0,'card_death']) ))


## plots log-log QUALI

par(mfrow=c(3,3))

list_plot_loglog <- lapply(var_donneur[7:14], function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                               xlab="Temps post  sortie (jours)", mark.time=FALSE))

par(mfrow=c(1,1))



#################################### Modèle de base

model_donneur <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du donneur
                          ageD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD + 
                          arretD + cmvD + imcD
                        ,data = df)
summary(model_donneur)


#################################### Stepwise

var_donneur_univ <- c('ageD','ureeD','creatD','sexeD','card_death','causeDCD2cl','htaD','diabD','arretD',
                      'cmvD','tailleD','poidsD')

stepdat <- na.omit(df[,c(var_donneur_univ,'TpsEvtYear','Evt')])

stepwiseCox(Surv(stepdat$TpsEvtYear, stepdat$Evt) ~ 
              # Variables du donneur
              ageD + ureeD + creatD + sexeD + card_death + causeDCD2cl + htaD + diabD + 
              arretD + cmvD + tailleD + poidsD,
            selection='backward',
            select='SL',
            sls=0.05,
            sle=0.05,
            data=stepdat)

######### Modèle retenu

model_donneur <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du donneur
                          ageD + card_death + causeDCD2cl +
                          cmvD + tailleD + poidsD
                        ,data = stepdat)
summary(model_donneur) 

model_donneur_df <- as.data.frame(tidy(model_donneur))
model_donneur_df$hr <- exp(model_donneur_df$estimate)
model_donneur_df <- model_donneur_df[c(2,6,3:5)]
rownames(model_donneur_df) <- c('Donor age','Donor after cardiac death',
                                 'Donor death vascular etiology',
                                 'Positive Donor CMV serology','Donor height','Donor weight')
colnames(model_donneur_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_donneur_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Donneur/modele_final_donneur_step.pdf')





#####################################################################################################################
# SELECTION VARIABLES GREFFE
#####################################################################################################################

################## Log-linéarité

## CIT -- OK
q25 <- quantile(df$iscHeure,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$iscHeure,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~iscHeure,data=df[df$iscHeure>q25 & df$iscHeure<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$iscHeure>q25 & df$iscHeure<q975 & !is.na(df$iscHeure),]$iscHeure, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "CIT - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite


## Temps liste -- OK
q25 <- quantile(df$Tabm,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$Tabm,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~Tabm,data=df[df$Tabm>q25 & df$Tabm<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$Tabm>q25 & df$Tabm<q975 & !is.na(df$Tabm),]$Tabm, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Temps sur liste - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite

## Temps dialyse -- OK
q25 <- quantile(df$Tdial,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$Tdial,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~Tdial,data=df[df$Tdial>q25 & df$Tdial<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$Tdial>q25 & df$Tdial<q975 & !is.na(df$Tdial),]$Tdial, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Temps en dialyse - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite


################## Proportionnalité des risques

## résidus de Schoenfeld 
par(mfrow=c(2,3))

list_plot_schoen <- lapply(var_greffe, function(x) {plot(main=x,cox.zph(coxph(Surv(df$TpsEvtYear,df$Evt)~df[,x]) ),col=c(1,2,2,2)) 
  abline(h=0,col=4)})


## plots log-log QUALI

par(mfrow=c(1,1))

list_plot_loglog <- lapply(c('incomp2cl'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                            xlab="Temps post  sortie (jours)", mark.time=FALSE))


#################################### Stepwise

var_greffe_univ <- c('Tdial','Tabm','incomp2cl')
var_donneur_univ2 <- c('ageD','card_death','causeDCD2cl','cmvD','tailleD','poidsD')

stepdat <- na.omit(df[,c(var_donneur_univ2,var_greffe_univ,'TpsEvtYear','Evt')])
stepwiseCox(Surv(stepdat$TpsEvtYear, stepdat$Evt) ~ 
              # Variables du donneur
              ageD + card_death + causeDCD2cl + cmvD + tailleD + poidsD +
              # Variables greffe
              Tdial + incomp2cl + Tabm,
            selection='backward',
            sls=0.05,
            sle=0.05,
            include=c('ageD','card_death','causeDCD2cl','cmvD','tailleD','poidsD'),
            data=stepdat)


#################################### Modèle retenu

model_greffe <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du donneur
                         ageD + card_death + causeDCD2cl +
                         cmvD + tailleD + poidsD +
                         # Variables de la greffe
                         incomp2cl + Tdial
                       ,data = stepdat)
summary(model_greffe) 

model_greffe_df <- as.data.frame(tidy(model_greffe))
model_greffe_df$hr <- exp(model_greffe_df$estimate)
model_greffe_df <- model_greffe_df[c(2,6,3:5)]
rownames(model_greffe_df) <- c('Donor age','Donor after cardiac death',
                                'Donor death vascular etiology',
                                'Positive Donor CMV serology','Donor height',
                                'Donor weight','HLA incompatibilities >=4','Time on dialysis')
colnames(model_greffe_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_greffe_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Greffe/modele_final_greffe_step.pdf')




#####################################################################################################################
# SELECTION VARIABLES RECEVEUR
#####################################################################################################################


################## Log-linéarité

## Age receveur -- OK

q25 <- quantile(df$ageR,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$ageR,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~ageR,data=df[df$ageR>q25 & df$ageR<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$ageR>q25 & df$ageR<q975 & !is.na(df$ageR),]$ageR, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Age receveur - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite


## IMC receveur -- OK
q25 <- quantile(df$imcR,c(0,0.025,1),na.rm=T)[2]
q975 <- quantile(df$imcR,c(0,0.975,1),na.rm=T)[2]

mart_res_null <- residuals(coxph(Surv(TpsEvtYear,Evt)~imcR,data=df[df$imcR>q25 & df$imcR<q975,]),type="martingale")

ggplot(mapping = aes(x = df[df$imcR>q25 & df$imcR<q975 & !is.na(df$imcR),]$imcR, y = mart_res_null)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "IMC - Log linéaire") +
  xlab('') +
  ylab('') +
  theme_bw() + theme(legend.key = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8))# Log linéarité parfaite

################## Proportionnalité des risques

## Fonction log-log
log.minus.log<-function(y) { 
  log(-log(y)) 
}


## plots log-log QUALI

par(mfrow=c(4,4))

list_plot_loglog <- lapply(c(var_univ_receveur_quali), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                                        xlab="Temps post  sortie (jours)", mark.time=FALSE))


## résidus de Schoenfeld 
par(mfrow=c(5,4))

list_plot_schoen <- lapply(var_receveur, function(x) {plot(main=x,cox.zph(coxph(Surv(df$TpsEvtYear,df$Evt)~df[,x]) ),col=c(1,2,2,2)) 
  abline(h=0,col=4)})

#################################### Stepwise

var_greffe_univ2 <- c('Tdial','incomp2cl')
var_donneur_univ2 <- c('ageD','card_death','causeDCD2cl','cmvD','tailleD','poidsD')
var_rec_univ <- c('ageR','imcR','sexeR','malIni2cl','hemodial','anteDiab','anteDyslip',
                  'anteHTA','anteCardioVasc','anteNeo','cmvR','AcHBsR')


stepdat <- na.omit(df[,c(var_donneur_univ2,var_greffe_univ2,var_rec_univ,'TpsEvtYear','Evt')])
stepwiseCox(Surv(stepdat$TpsEvtYear, stepdat$Evt) ~ 
              # Variables du donneur
              ageD + card_death + causeDCD2cl + cmvD + tailleD + poidsD +
              # Variables greffe
              Tdial + incomp2cl +
              # Variables du receveur
              ageR + imcR + sexeR + malIni2cl + hemodial + anteDiab + anteDyslip + anteHTA + 
              anteCardioVasc + anteNeo + cmvR + AcHBsR,
            selection='backward',
            sls=0.05,
            sle=0.05,
            include=c('ageD','card_death','causeDCD2cl','cmvD','tailleD','poidsD','Tdial','incomp2cl'),
            data=stepdat)


#################################### Modèle retenu

model_receveur <- coxph(Surv(TpsEvtYear, Evt) ~ 
                        # Variables du donneur
                        ageD + card_death + causeDCD2cl +
                        cmvD + tailleD + poidsD +
                        # Variables de la greffe
                        incomp2cl + Tdial +
                        # Variables du receveur
                        ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl
                      ,data = stepdat)
summary(model_receveur) 

model_receveur_df <- as.data.frame(tidy(model_receveur))
model_receveur_df$hr <- exp(model_receveur_df$estimate)
model_receveur_df <- model_receveur_df[c(2,6,3:5)]
rownames(model_receveur_df) <- c('Donor age','Donor after cardiac death',
                               'Donor death vascular etiology',
                               'Positive Donor CMV serology','Donor height',
                               'Donor weight','HLA incompatibilities >=4','Time on dialysis',
                               'Recipient age','History of diabetes','History of cardiovascular disease',
                               'Hemodialysis','History of neoplasia','Relapsing initial nephropathy')
colnames(model_receveur_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_receveur_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2/Receveur/modele_final_receveur_step.pdf')


#####################################################################################################################
# INTERACTIONS ENTRE VARIABLES SIGNIFICATIVES
#####################################################################################################################


####################################  Liste des candidates
inter_signif2 <- c('incomp2cl:ageD',
                   'ageD:ageR',
                   'hemodial:ageD','anteDiab:ageD',
                   'anteCardioVasc:ageD',
                   'anteNeo:ageD',
                   'malIni2cl:ageD',
                   'ageD:Tdial',
                   'incomp2cl:card_death',
                   'hemodial:card_death',
                   'anteCardioVasc:card_death',
                   'ageR:causeDCD2cl',
                   'hemodial:causeDCD2cl','anteDiab:causeDCD2cl',
                   'anteCardioVasc:causeDCD2cl','anteNeo:causeDCD2cl',
                   'malIni2cl:causeDCD2cl',
                   'cmvD:hemodial','cmvD:anteDiab','cmvD:ageR',
                   'tailleD:hemodial','tailleD:anteCardioVasc','tailleD:anteNeo','tailleD:ageR',
                   'poidsD:hemodial','poidsD:malIni2cl','poidsD:ageR'
)

####################################  Fonction d'ajout d'interaction au modele

ajout_candidat<-function(modele,candidat)
{
  mod_text <- modele$call %>% deparse() %>% paste(collapse="")
  pos <- regexpr("~",mod_text)
  mod2_text <- paste0(substr(mod_text,1,pos),candidat,"+",substr(mod_text,pos+1,nchar(mod_text)))
  
  out <- tryCatch(mod2_text %>% str2lang %>% eval(), warning=function(w) {return(NA)})
  return(out)
}

#################################### Table univariée
tab_inter_signif<- round(sapply(inter_signif2,function(x) summary(ajout_candidat(model_receveur,x))$coefficients[15,]),3)


tab_inter_signif <- t(tab_inter_signif)
colnames(tab_inter_signif) <- c('\\beta','HR','SE(\\beta)','Z','p')
tab_inter_signif

#################################### Stepwise

stepdat <- na.omit(df[,c('ageD','card_death','causeDCD2cl','cmvD','tailleD','poidsD','Tdial','incomp2cl',
                         'ageR','anteDiab','anteCardioVasc','hemodial','anteNeo','malIni2cl','TpsEvtYear','Evt')])
modback <- coxph(Surv(TpsEvtYear, Evt) ~ 
                   # Variables du donneur
                   ageD + card_death + causeDCD2cl +
                   cmvD + tailleD + poidsD +
                   # Variables de la greffe
                   incomp2cl + Tdial +
                   # Variables du receveur
                   ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                   # Interactions internes
                   ageD:ageR + malIni2cl:ageD + incomp2cl:hemodial + incomp2cl:card_death + 
                     ageR:causeDCD2cl + anteDiab:causeDCD2cl + cmvD:hemodial,
                 data=stepdat)

fastbw(coxph2cph(modback),rule='p',sls=0.05,force = 1:14)

#################################### Modèle retenu

model_complet_intersig <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + causeDCD2cl +
                                  cmvD + tailleD + poidsD +
                                  # Variables de la greffe
                                  incomp2cl + Tdial +
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df)

summary(model_complet_intersig)

#####################################################################################################################
# INTERACTIONS AVEC VARIABLES EXTERNES
#####################################################################################################################

#################################### Liste des candidates

inter_nosignif2 <- c('ageD:sexeR','ageD:anteDyslip','ageD:anteHTA','ageD:cmvR','ageD:AcHBsR',
                   'ageD:imcR','ageD:antiClassI.jour','ageD:antiClassII.jour',
                   'card_death:sexeR','card_death:anteHTA','card_death:antiClassI.jour','card_death:antiClassII.jour',
                   'card_death:imcR',
                   'causeDCD2cl:anteDyslip','causeDCD2cl:anteHTA','causeDCD2cl:cmvR','causeDCD2cl:AcHBsR',
                   'causeDCD2cl:antiClassII.jour','causeDCD2cl:imcR',
                   'cmvD:imcR','cmvD:cmvR',
                   'tailleD:sexeR','tailleD:anteDyslip','tailleD:antiClassI.jour','tailleD:cmvR',
                   'tailleD:imcR',
                   'poidsD:imcR','poidsD:sexeR','poidsD:anteDyslip','poidsD:antiClassI.jour',
                   'poidsD:antiClassII.jour'
)

#################################### Table univariée
tab_inter_nosignif<- round(sapply(inter_nosignif2,function(x) summary(ajout_candidat(model_complet_intersig,x))$coefficients[15,]),3)


tab_inter_nosignif <- t(tab_inter_nosignif)
colnames(tab_inter_nosignif) <- c('\\beta','HR','SE(\\beta)','Z','p')
tab_inter_nosignif

#################################### Stepwise

model_complet_inter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + causeDCD2cl +
                                  cmvD + tailleD + poidsD +
                                  # Variables de la greffe
                                  incomp2cl + Tdial +
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl +
                                  # Interactions externes
                                  card_death:imcR + causeDCD2cl:imcR + cmvD:cmvR +
                                  tailleD:imcR + poidsD:imcR,
                                data=df)

summary(model_complet_inter)

# -tailleD:imcR

model_complet_inter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                               # Variables du donneur
                               ageD + card_death + causeDCD2cl +
                               cmvD + tailleD + poidsD +
                               # Variables de la greffe
                               incomp2cl + Tdial +
                               # Variables du receveur
                               ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                               # Interactions internes
                               ageD:ageR + ageR:causeDCD2cl +
                               # Interactions externes
                               card_death:imcR + causeDCD2cl:imcR + cmvD:cmvR +
                               poidsD:imcR,
                             data=df)

summary(model_complet_inter)


# -causeDC:imcR

model_complet_inter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                               # Variables du donneur
                               ageD + card_death + causeDCD2cl +
                               cmvD + tailleD + poidsD +
                               # Variables de la greffe
                               incomp2cl + Tdial +
                               # Variables du receveur
                               ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                               # Interactions internes
                               ageD:ageR + ageR:causeDCD2cl +
                               # Interactions externes
                               card_death:imcR + cmvD:cmvR +
                               poidsD:imcR,
                             data=df)

summary(model_complet_inter)


# -cmvD:cmvR

model_complet_inter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                               # Variables du donneur
                               ageD + card_death + causeDCD2cl +
                               cmvD + tailleD + poidsD +
                               # Variables de la greffe
                               incomp2cl + Tdial +
                               # Variables du receveur
                               ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                               # Interactions internes
                               ageD:ageR + ageR:causeDCD2cl +
                               # Interactions externes
                               card_death:imcR + 
                               poidsD:imcR,
                             data=df)

summary(model_complet_inter)


# -card_death:imcR

model_complet_inter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                               # Variables du donneur
                               ageD + card_death + causeDCD2cl +
                               cmvD + tailleD + poidsD +
                               # Variables de la greffe
                               incomp2cl + Tdial +
                               # Variables du receveur
                               ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                               # Interactions internes
                               ageD:ageR + ageR:causeDCD2cl +
                               # Interactions externes
                               poidsD:imcR,
                             data=df)

summary(model_complet_inter)


#################################### Modèle retenu

model_complet_inter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                               # Variables du donneur
                               ageD + card_death + causeDCD2cl +
                               cmvD + tailleD + poidsD +
                               # Variables de la greffe
                               incomp2cl + Tdial +
                               # Variables du receveur
                               ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                               # Interactions internes
                               ageD:ageR + ageR:causeDCD2cl,
                             data=df)

summary(model_complet_inter)

#####################################################################################################################
# REINJECTION DE VARIABLES REJETEES
#####################################################################################################################

#################################### Variables candidates

var_rej <- c('diureseD','ureeD','creatD','sexeD','htaD','diabD','arretD','protuD','iscHeure',
             'imcR','sexeR','anteDyslip','anteHTA','anteUro','cmvR','AcHBsR','antiClassI.jour','antiClassII.jour')
tab_rej<- round(sapply(var_rej,function(x) summary(ajout_candidat(model_complet_inter,x))$coefficients[1,]),3)
tab_rej <- t(tab_rej)
colnames(tab_rej) <- c('\\beta','HR','SE(\\beta)','Z','p')
tab_rej

#################################### Modèle de base

model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                               # Variables du donneur
                               ageD + card_death + causeDCD2cl +
                               cmvD + tailleD + poidsD +
                               # Variables de la greffe
                               incomp2cl + Tdial +
                               # Variables du receveur
                               ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                               # Interactions internes
                               ageD:ageR + ageR:causeDCD2cl +
                               # Variables rejetées
                               creatD + sexeD + protuD + iscHeure + imcR + antiClassII.jour,
                             data=df)

summary(model_complet_rej)

#################################### Backward

# -protuD
model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD +
                             # Variables de la greffe
                             incomp2cl + Tdial +
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl +
                             # Variables rejetées
                             creatD + sexeD + iscHeure + imcR + antiClassII.jour,
                           data=df)

summary(model_complet_rej)

#-imcR
model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD +
                             # Variables de la greffe
                             incomp2cl + Tdial +
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl +
                             # Variables rejetées
                             creatD + sexeD + iscHeure + antiClassII.jour,
                           data=df)

summary(model_complet_rej)

#-sexeD
model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD +
                             # Variables de la greffe
                             incomp2cl + Tdial +
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl +
                             # Variables rejetées
                             creatD + iscHeure + antiClassII.jour,
                           data=df)

summary(model_complet_rej)

#-anticlass
model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD +
                             # Variables de la greffe
                             incomp2cl + Tdial +
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl +
                             # Variables rejetées
                             creatD + iscHeure,
                           data=df)

summary(model_complet_rej)


#################################### Modèle retenu

model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)

summary(model_complet_rej)


#####################################################################################################################
# REMISE EN CAUSE VARIABLES
#####################################################################################################################

#################################### Modèle de base

model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)

summary(model_complet_rej)

#################################### Backward

fastbw(coxph2cph(model_complet_rej),rule='p',sls=0.05,force = 16:18)


###### Effet confondant anteNeo ?

model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + anteNeo + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)

model_complet_rej2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + malIni2cl +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)

coef(model_complet_rej)[1:7]/coef(model_complet_rej2)[1:7] # Pas de var de plus de 10%, on supprime

###### Effet confondant malIni ?

model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + malIni2cl + 
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)

model_complet_rej2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                              # Variables du donneur
                              ageD + card_death + causeDCD2cl +
                              cmvD + tailleD + poidsD + creatD + 
                              # Variables de la greffe
                              incomp2cl + Tdial + iscHeure + 
                              # Variables du receveur
                              ageR + anteDiab + anteCardioVasc + hemodial + 
                              # Interactions internes
                              ageD:ageR + ageR:causeDCD2cl,
                            data=df)

coef(model_complet_rej)[1:7]/coef(model_complet_rej2)[1:7] # Pas de var de plus de 10%, on supprime



#################################### Modèle retenu

model_complet_rej <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)

#####################################################################################################################
# NOUVELLES INTERACTIONS INTERNES
#####################################################################################################################

####################################  Liste des candidates
inter_signif2 <- c('incomp2cl:ageD',
                   'hemodial:ageD','anteDiab:ageD',
                   'anteCardioVasc:ageD',
                   'ageD:Tdial',
                   'incomp2cl:card_death',
                   'hemodial:card_death',
                   'anteCardioVasc:card_death',
                   'hemodial:causeDCD2cl','anteDiab:causeDCD2cl',
                   'anteCardioVasc:causeDCD2cl',
                   'cmvD:hemodial','cmvD:anteDiab','cmvD:ageR',
                   'tailleD:hemodial','tailleD:anteCardioVasc','tailleD:ageR',
                   'poidsD:hemodial','poidsD:ageR',
                   'creatD:hemodial','creatD:ageR'
)

#################################### Table univariée
tab_inter_signif<- round(sapply(inter_signif2,function(x) summary(ajout_candidat(model_complet_rej,x))$coefficients[15,]),3)

tab_inter_signif <- t(tab_inter_signif)
colnames(tab_inter_signif) <- c('\\beta','HR','SE(\\beta)','Z','p')
tab_inter_signif

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + iscHeure + 
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl + anteDiab:ageD + 
                             incomp2cl:card_death + hemodial:card_death +
                             cmvD:hemodial + tailleD:anteCardioVasc + tailleD:ageR +
                             creatD:ageR,
                           data=df)
summary(model_complet_reinter)

# -tailleD:anteCard

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl + anteDiab:ageD + 
                                 incomp2cl:card_death + hemodial:card_death +
                                 cmvD:hemodial + tailleD:ageR +
                                 creatD:ageR,
                               data=df)
summary(model_complet_reinter)

# -tailleD:ageR

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl + anteDiab:ageD + 
                                 incomp2cl:card_death + hemodial:card_death +
                                 cmvD:hemodial + 
                                 creatD:ageR,
                               data=df)
summary(model_complet_reinter)

# -anteDiab:ageD

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl + 
                                 incomp2cl:card_death + hemodial:card_death +
                                 cmvD:hemodial + 
                                 creatD:ageR,
                               data=df)
summary(model_complet_reinter)

# -hemodial:card_death

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl + 
                                 incomp2cl:card_death + 
                                 cmvD:hemodial + 
                                 creatD:ageR,
                               data=df)
summary(model_complet_reinter)

# -cmvD:hemodial

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl + 
                                 incomp2cl:card_death + 
                                 creatD:ageR,
                               data=df)
summary(model_complet_reinter)

# -creatD:ageR

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl + 
                                 incomp2cl:card_death,
                               data=df)
summary(model_complet_reinter)

# -incomp2cl:card_death

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)
summary(model_complet_reinter)





#####################################################################################################################
# MODELE FINAL
#####################################################################################################################

####################################  Confusion incomp2cl ?

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)

coef(model_complet_reinter)[c(1:7,9,10)]/coef(model_complet_reinter2)[1:9] # confusion, on garde


####################################  Confusion causeDC ?

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + 
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  incomp2cl + Tdial + iscHeure + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df)

coef(model_complet_reinter)[c(1:2,4:10)]/coef(model_complet_reinter2)[1:9] # confusion, on garde

####################################  IMC ou taille/poids ?

model_final <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)
summary(model_final)
BIC(model_final)


#################################### Nouvelle possibilité d'ajout ?

var_rej <- c('diureseD','ureeD','sexeD','htaD','diabD','arretD','protuD',
             'imcR','sexeR','anteDyslip','anteHTA','anteUro','cmvR','AcHBsR','antiClassI.jour','antiClassII.jour')
tab_rej2<- round(sapply(var_rej,function(x) summary(ajout_candidat(model_final,x))$coefficients[1,]),3)
tab_rej2 <- t(tab_rej2)
colnames(tab_rej2) <- c('\\beta','HR','SE(\\beta)','Z','p')
tab_rej2


#################################### Export du modèle de base en pdf

model_final_df <- as.data.frame(tidy(model_final))
model_final_df$hr <- exp(model_final_df$estimate)
model_final_df <- model_final_df[c(2,6,3:5)]
rownames(model_final_df) <- c('Donor Age','Donor after cardiac death','Donor death vascular etiology','Positive donor CMV serology','Donor height',
                                          'Donor weight','Donor creatinine','HLA incompatibilities>=4','Time on dialysis','Cold ischemia time','Recipient Age',
                                          'History of diabetes','History of cardiovascular disease','Hemodialysis','Recipient age:Donor age',
                                          'Donor death vascular etiology:Recipient age')
colnames(model_final_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_final_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2//modele_final_avec_interactions_step.pdf')



#####################################################################################################################
# CENTRE / PERIODE
#####################################################################################################################

################## Centre

model_finalb <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + iscHeure + 
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial +
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df,ties = 'breslow')
summary(model_finalb)
AIC(model_finalb)

model_final_centre <- coxph(Surv(TpsEvtYear, Evt) ~ 
                        # Variables du donneur
                        ageD + card_death + causeDCD2cl +
                        cmvD + tailleD + poidsD + creatD + 
                        # Variables de la greffe
                        incomp2cl + Tdial + iscHeure + 
                        # Variables du receveur
                        ageR + anteDiab + anteCardioVasc + hemodial +
                        # Interactions internes
                        ageD:ageR + ageR:causeDCD2cl + frailty(centre,'gamma'),
                      data=df,ties = 'breslow')
summary(model_final_centre)
AIC(model_final_centre)
AIC(model_final_centre);AIC(model_finalb) # Effet centre non significatif

################## Période

table(df$yearG,df$tacro) # 50-50 en 2006, 75-25 en 2011, saut en 2012
table(df$yearG,df$machPerf) # 2016 (début de forte croissance vers 2012)

###### Tacrolimus

#### 2006
df$before_2006 <- df$yearG<=2006

list_plot_loglog <- lapply(c('before_2006'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))


m2006 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2006,
              data=df,ties = 'breslow')
AIC(m2006);AIC(model_finalb) # Effet 2006 non significatif

#### 2007
df$before_2007 <- df$yearG<=2007 

list_plot_loglog <- lapply(c('before_2008'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))


m2007 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2007,
              data=df,ties = 'breslow')
AIC(m2007);AIC(model_finalb) # Effet 2007 non significatif

#### 2008

list_plot_loglog <- lapply(c('before_2008'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))


m2008 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2008,
              data=df,ties = 'breslow')
AIC(m2008);AIC(model_finalb) # Effet 2008 non significatif


#### 2009
df$before_2009 <- df$yearG<=2009

list_plot_loglog <- lapply(c('before_2009'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))


m2009 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2009,
              data=df,ties = 'breslow')
AIC(m2009);AIC(model_finalb) # Effet 2009 non significatif



#### 2010
df$before_2010 <- df$yearG<=2010

list_plot_loglog <- lapply(c('before_2010'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))


m2010 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2010,
              data=df,ties = 'breslow')
AIC(m2010);AIC(model_finalb) # Effet 2010 non significatif




#### 2011
df$before_2011 <- df$yearG<=2011

list_plot_loglog <- lapply(c('before_2011'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))

m2011 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2011,
              data=df,ties = 'breslow')
AIC(m2011);AIC(model_finalb) # Effet 2011 non significatif

###### Machine Perf

#### 2016
df$before_2015 <- df$yearG<=2015

list_plot_loglog <- lapply(c('before_2015'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))

m2015 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2015,
              data=df,ties = 'breslow')
AIC(m2015);AIC(model_finalb) # Effet 2016 non significatif

#### 2016
df$before_2016 <- df$yearG<=2016

list_plot_loglog <- lapply(c('before_2016'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))

m2016 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2016,
              data=df,ties = 'breslow')
AIC(m2016);AIC(model_finalb) # Effet 2016 non significatif

#### 2017
df$before_2017 <- df$yearG<=2017

list_plot_loglog <- lapply(c('before_2017'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                              xlab="Temps post  sortie (jours)", mark.time=FALSE))

m2017 <-coxph(Surv(TpsEvtYear, Evt) ~ 
                # Variables du donneur
                ageD + card_death + causeDCD2cl +
                cmvD + tailleD + poidsD + creatD + 
                # Variables de la greffe
                incomp2cl + Tdial + iscHeure + 
                # Variables du receveur
                ageR + anteDiab + anteCardioVasc + hemodial +
                # Interactions internes
                ageD:ageR + ageR:causeDCD2cl + before_2017,
              data=df,ties = 'breslow')
AIC(m2017);AIC(model_finalb) # Effet 2017 non significatif


#####################################################################################################################
# MODELE FINAL AVEC EFFET PERIODE
#####################################################################################################################

####################################  Confusion iscHeure ?

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + before_2012 + 
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)
summary(model_complet_reinter)

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + causeDCD2cl +
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  incomp2cl + Tdial + before_2012 + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df[!is.na(df$iscHeure),])
summary(model_complet_reinter2)

AIC(model_complet_reinter2);AIC(model_complet_reinter) # Augmentation de l'AIC, on garde

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + causeDCD2cl +
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  incomp2cl + Tdial + before_2012 + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df)
coef(model_complet_reinter)[c(1:9)]/coef(model_complet_reinter2)[1:9] # confusion, moyenne partout, on garde



####################################  Confusion incomp2cl ?

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + before_2012 +
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)
summary(model_complet_reinter)

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + causeDCD2cl +
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  Tdial + iscHeure + before_2012 + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df[!is.na(df$incomp2cl),])
summary(model_complet_reinter2)
AIC(model_complet_reinter2);AIC(model_complet_reinter) # Augmentation de l'AIC, on garde

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death + causeDCD2cl +
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  Tdial + iscHeure + before_2012 + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df)
summary(model_complet_reinter2)
coef(model_complet_reinter)[c(1:7,9,10)]/coef(model_complet_reinter2)[1:9] # confusion, on garde

####################################  Confusion causeDC ?

model_complet_reinter <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du donneur
                                 ageD + card_death + causeDCD2cl +
                                 cmvD + tailleD + poidsD + creatD + 
                                 # Variables de la greffe
                                 incomp2cl + Tdial + iscHeure + before_2012 +
                                 # Variables du receveur
                                 ageR + anteDiab + anteCardioVasc + hemodial +
                                 # Interactions internes
                                 ageD:ageR + ageR:causeDCD2cl,
                               data=df)
summary(model_complet_reinter)

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death +
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  Tdial + iscHeure + before_2012 + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df[!is.na(df$causeDCD2cl),])
summary(model_complet_reinter2)
AIC(model_complet_reinter2);AIC(model_complet_reinter) # Augmentation de l'AIC, on garde

model_complet_reinter2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                  # Variables du donneur
                                  ageD + card_death +
                                  cmvD + tailleD + poidsD + creatD + 
                                  # Variables de la greffe
                                  Tdial + iscHeure + before_2012 + 
                                  # Variables du receveur
                                  ageR + anteDiab + anteCardioVasc + hemodial +
                                  # Interactions internes
                                  ageD:ageR + ageR:causeDCD2cl,
                                data=df)
summary(model_complet_reinter2)
coef(model_complet_reinter)[c(1:2,4:10)]/coef(model_complet_reinter2)[1:9] # confusion, on garde


####################################  Modèle retenu

model_final <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + iscHeure + before_2012 +
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df)
summary(model_final)
AIC(model_final)


#################################### Nouvelle possibilité d'ajout à alpha=0.2 ?

var_rej <- c('diureseD','ureeD','sexeD','htaD','diabD','arretD','protuD',
             'imcR','sexeR','anteDyslip','anteHTA','anteUro','cmvR','AcHBsR','antiClassI.jour','antiClassII.jour')
tab_rej2<- round(sapply(var_rej,function(x) summary(ajout_candidat(model_final,x))$coefficients[1,]),3)
tab_rej2 <- t(tab_rej2)
colnames(tab_rej2) <- c('\\beta','HR','SE(\\beta)','Z','p')
tab_rej2

#################################### imcR en ajustement ?

model_final <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + iscHeure + before_2012 +
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial + 
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df[!is.na(df$imcR),])
summary(model_final)

model_final2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + iscHeure + before_2012 +
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df)
summary(model_final2)

coef(model_final)[c(1:9)]/coef(model_final2)[1:9] # ajustement, on garde

model_final <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + iscHeure + before_2012 +
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df)
summary(model_final)

#################################### Supprimer iscHeure ?

model_final <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + iscHeure + before_2012 +
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df)
summary(model_final)

model_final2 <- coxph(Surv(TpsEvtYear, Evt) ~ 
                        # Variables du donneur
                        ageD + card_death + causeDCD2cl +
                        cmvD + tailleD + poidsD + creatD + 
                        # Variables de la greffe
                        incomp2cl + Tdial + before_2012 +
                        # Variables du receveur
                        ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                        # Interactions internes
                        ageD:ageR + ageR:causeDCD2cl,
                      data=df[!is.na(df$iscHeure),])
summary(model_final2)

coef(model_final)[c(1:9)]/coef(model_final2)[1:9] 
AIC(model_final);AIC(model_final2) # Supprime iscHeure


####################################  Modèle retenu

model_final <- coxph(Surv(TpsEvtYear, Evt) ~ 
                       # Variables du donneur
                       ageD + card_death + causeDCD2cl +
                       cmvD + tailleD + poidsD + creatD + 
                       # Variables de la greffe
                       incomp2cl + Tdial + before_2012 +
                       # Variables du receveur
                       ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                       # Interactions internes
                       ageD:ageR + ageR:causeDCD2cl,
                     data=df)
summary(model_final)


#################################### Export du modèle de base en pdf

model_final_df <- as.data.frame(tidy(model_final))
model_final_df$hr <- exp(model_final_df$estimate)
model_final_df <- model_final_df[c(2,6,3:5)]
rownames(model_final_df) <- c('Donor Age','Donor after cardiac death','Donor death vascular etiology','Positive donor CMV serology','Donor height',
                              'Donor weight','Donor creatinine','HLA incompatibilities>=4','Time on dialysis','Transplanted before 2012','Recipient Age',
                              'History of diabetes','History of cardiovascular disease','Hemodialysis','Recipient BMI','Recipient age:Donor age',
                              'Donor death vascular etiology:Recipient age')
colnames(model_final_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_final_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/tables_graphes/Stratégie 2//modele_final_avec_interactions_step.pdf')







#####################################################################################################################
# VALIDATION
#####################################################################################################################

####################################  Courbe ROC temps-dépendante

df$lp <- predict(model_final,type='lp',newdata = df)
survivalROC_helper <- function(t) {
  survivalROC(Stime        = df$TpsEvtYear,
              status       = df$Evt,
              marker       = df$lp,
              predict.time = t,
              method       = "NNE",
              span = 0.25 * nrow(df)^(-0.20))
}


survivalROC_data <- data_frame(t = 1:10) %>%
  mutate(survivalROC = map(t, survivalROC_helper),
         ## Extract scalar AUC
         auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
         ## Put cut off dependent values in a data_frame
         df_survivalROC = map(survivalROC, function(obj) {
           as_data_frame(obj[c("cut.values","TP","FP")])
         })) %>%
  dplyr::select(-survivalROC) %>%
  unnest() %>%
  arrange(t, FP, TP)

survivalROC_data %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = survivalROC_data %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())


#################################### Calibration

##### Test de Nam-D'Agostino (calibration)

model_final_calib <- coxph(Surv(TpsEvtYear, Evt) ~ 
                             # Variables du donneur
                             ageD + card_death + causeDCD2cl +
                             cmvD + tailleD + poidsD + creatD + 
                             # Variables de la greffe
                             incomp2cl + Tdial + before_2012 +
                             # Variables du receveur
                             ageR + anteDiab + anteCardioVasc + hemodial + imcR +
                             # Interactions internes
                             ageD:ageR + ageR:causeDCD2cl,
                           data=df)
summary(model_final_calib)
