#####################################################################################################################
# IMPORT
#####################################################################################################################
library(ggplot2)
library(dplyr)
library(rcartocolor)
library(RColorBrewer)
library(ggrepel)
library(plotly)
library(knitr)
library(gridExtra)
library(ggsurvfit)
library(survival)
library(survminer)
library(table1)
library(questionr)
library(ggcorrplot)
library(kableExtra)
library(broom)
library(tidyr)
library(lmtest)

## Paramètres

# Centrage des titres GGplot
theme_update(plot.title = element_text(hjust = 0.5))




## Import

df <- read.csv2("~/Documents/Stage/Data/baseScoreD.csv", stringsAsFactors=TRUE)
df_origin <- df
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
set.seed(NULL)

#####################################################################################################################
# VERIFICATION DE COHERENCE ET CRITERES D'INCLUSION
#####################################################################################################################

### Criteres d'inclusion - OK

# Centres: Lyon, Montpellier, Nancy, Nantes, Nice, St-Etienne - OK
table(df_origin$centre)

# Type de greffe: Rein - OK
table(df_origin$typeG)

# Date de greffe 01/01/2000 - 31/12/2022 - OK
summary(df_origin$yearG)

# Première greffe uniquement - OK
table(df_origin$rangG)

# Receveur adulte - OK
summary(df_origin$ageR)

# Donneur cadavérique - OK
table(df_origin$typeD)



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
# ANALYSE DESCRIPTIVE
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
                          'protuD','deplet','antiClassI.jour','antiClassII.jour','before_2008')

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
                           'Pre-transplantation anti-HLA immunization of class II','Transplanted before 2008')


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


## Casser le data frame d'origine en 2 pour séparer la table pour l'impression


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
# REGRESSION DONNEUR / RECEVEUR
#####################################################################################################################

################## Listes variables

# Quantitatif
var_univ_donneur_quanti <- c('ageD','tailleD','poidsD','diureseD','ureeD','creatD')
  
var_univ_receveur_quanti <- c('ageR','imcR')

var_univ_greffe_quanti <- c('incompABDR','Tdial','Tabm')


# Qualitatif
var_univ_donneur_quali <- c('sexeD','card_death','causeDCD2cl','htaD','diabD','arretD','protuD','cmvD')

var_univ_receveur_quali <- c('sexeR','malIni2cl','preemptive','perito','hemodial','tabac','anteDiab','anteDyslip',
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
rownames(dg_univ_quanti) <- c('HLA A-B-DR incompatibilities','Time on dialysis','Time on waiting list')


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
rownames(dr_univ_quanti_quali) <- c('Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis','History of smoking',
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
colnames(gd_univ_quanti_quali) <- c('HLA A-B-DR incompatibilities','Time on dialysis','Time on waiting list')
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
rownames(dr_univ_quali) <- c('Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis','History of smoking',
                                    'History of diabetes','History of Dyslipidemia','History of hypertension',
                                    'History of cardiovascular disease','History of neoplasia','History of urological disease',
                                    'Positive recipient CMV serology','Positive recipient AcHBs serology','Pre-transplantation anti-HLA immunization of class I',
                                    'Pre-transplantation anti-HLA immunization of class II')


# Sauvegarde en PDF
dr_univ_quali %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Donneur-Receveur/tab_univ_quali_DR.pdf')


#####################################################################################################################
# SELECTION VARIABLES RECEVEUR
#####################################################################################################################

################## Sélection univariée


# Variables
var_receveur <- c(var_univ_receveur_quanti,var_univ_receveur_quali)

# Génération de la table
tab_univ_receveur <- round(sapply(df[,var_receveur],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~x))$coefficients),3)
tab_univ_receveur <- t(tab_univ_receveur)
colnames(tab_univ_receveur) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_receveur) <- c('Recipient Age','Recipient BMI','Recipient men','Relapsing initial nephropathy','Preemptive transplantation','Peritoneal dialysis','Hemodialysis','History of smoking',
                                 'History of diabetes','History of Dyslipidemia','History of hypertension',
                                 'History of cardiovascular disease','History of neoplasia','History of urological disease',
                                 'Positive recipient CMV serology','Positive recipient AcHBs serology','anti-HLA immunization of class I',
                                 'anti-HLA immunization of class II')



# Export en PDF

tab_univ_receveur %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_univ_receveur.pdf')



# Construction du modèle de base

model_receveur <- coxph(Surv(TpsEvtYear, Evt) ~ 
             # Variables du receveur
             ageR + imcR + sexeR + malIni2cl + techEpu3cl +
             anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac
            ,data = df)
summary(model_receveur) 




################## Log-linéarité

## Age receveur -- Passer au carré ?
df$ageR2 <- df$ageR**2
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~ageR+log(ageR)+sqrt(ageR)+ageR2,data=df)
ggcoxfunctional(mart_res_null, data = df)


## IMC receveur -- OK
df$imcR2 <- df$imcR**2
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~imcR+log(imcR)+sqrt(imcR)+imcR2,data=df[!is.na(df$imcR),])
ggcoxfunctional(mart_res_null, data = df[!is.na(df$imcR),])


################## Proportionnalité des risques

## Fonction log-log
log.minus.log<-function(y) { 
  log(-log(y)) 
}


## plots log-log QUALI

par(mfrow=c(4,4))

list_plot_loglog <- lapply(c(var_univ_receveur_quali,'tabac'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                                          xlab="Temps post  sortie (jours)", mark.time=FALSE))


## résidus de Schoenfeld 
par(mfrow=c(5,4))

list_plot_schoen <- lapply(var_receveur, function(x) plot(main=x,cox.zph(coxph(Surv(df$TpsEvtYear,df$Evt)~df[,x]) )))




################## Modèle retenu

# Construction du modèle de base

model_receveur <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du receveur
                          ageR + imcR2 + sexeR + malIni2cl + techEpu3cl +
                          anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac
                        ,data = df)
summary(model_receveur) 

# Export du modèle de base en pdf

model_receveur_df <- as.data.frame(tidy(model_receveur))
model_receveur_df$hr <- exp(model_receveur_df$estimate)
model_receveur_df <- model_receveur_df[c(2,6,3:5)]
rownames(model_receveur_df) <- c('Recipient Age','Recipient BMI (squared)','Recipient men','Relapsing initial nephropathy','Peritoneal dialysis','Hemodialysis',
                                 'History of diabetes','History of Dyslipidemia','History of hypertension',
                                 'History of cardiovascular disease','History of neoplasia',
                                 'Positive recipient CMV serology','Positive recipient AcHBs serology','History of smoking')
colnames(model_receveur_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_receveur_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_mod_receveur.pdf')



#####################################################################################################################
# SELECTION VARIABLES GREFFE
#####################################################################################################################

################## Sélection univariée

# Variables
var_greffe <- c('iscHeure','Tdial','Tabm','incompABDR','before_2008')

# Génération de la table
tab_univ_greffe <- round(sapply(df[,var_greffe],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~df$ageR + df$imcR2 + df$sexeR + df$malIni2cl + df$techEpu3cl +
                                                                              df$anteDiab + df$anteDyslip + df$anteHTA + df$anteCardioVasc + df$anteNeo + df$cmvR + df$AcHBsR + df$tabac + x))$coefficients[15,]),4)

# Centre ?
mcentre <- coxph(Surv(df$TpsEvtYear,df$Evt)~df$ageR + df$imcR2 + df$sexeR + df$malIni2cl + df$techEpu3cl +
        df$anteDiab + df$anteDyslip + df$anteHTA + df$anteCardioVasc + df$anteNeo + df$cmvR + df$AcHBsR + df$tabac + df$centre)
lrtest(mcentre,model_receveur)
pcentre <- as.numeric(lrtest(mcentre,model_receveur)[2,5])

# Mise en forme
tab_univ_greffe <- as.data.frame(t(tab_univ_greffe))
tab_univ_greffe <- rbind(tab_univ_greffe, c(rep('',4),round(pcentre,4)))
colnames(tab_univ_greffe) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_greffe) <- c('CIT','Time on dialysis','Time on waiting list','HLA AB-D-R incompatibilities','Transplanted before 2008','Centre')


# Export en PDF

tab_univ_greffe %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_univ_greffe.pdf')



# Construction du modèle enrichi

model_receveur_greffe <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du receveur
                          ageR + imcR + sexeR + malIni2cl + hemodial +
                          anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac +
                          # Variables de la greffe
                          Tabm + Tdial + incompABDR + before_2008 + centre
                        ,data = df)
summary(model_receveur_greffe) 


################## Log-linéarité

## Temps dialyse -- Passer à la racine carrée
df$Tdial2 <- df$Tdial**2
df$Tdiallog <- log(1+df$Tdial)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~Tdial+sqrt(Tdial)+Tdial2+Tdiallog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$Tdial),])


## Temps liste -- OK
df$Tabm2 <- df$Tabm**2
df$Tabmlog <- log(1+df$Tabm)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~Tabm+sqrt(Tabm)+Tabm2+Tabmlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$Tabm),])

## incompABDR -- OK
df$ABDR2 <- df$incompABDR**2
df$ABDRlog <- log(1+df$incompABDR)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~incompABDR+sqrt(incompABDR)+ABDR2+ABDRlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$incompABDR),])

## CIT -- log ?
df$CIT2 <- df$iscHeure**2
df$CITlog <- log(df$iscHeure)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~iscHeure+sqrt(iscHeure)+CIT2+CITlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$iscHeure),])
coxph(Surv(TpsEvtYear,Evt)~CITlog,data=df) # pas signif


################## Proportionnalité des risques

## résidus de Schoenfeld 
par(mfrow=c(2,3))

list_plot_schoen <- lapply(c(var_univ_greffe_quanti,'before_2008','centre'), function(x) plot(main=x,cox.zph(coxph(Surv(df$TpsEvtYear,df$Evt)~df[,x]) )))


## plots log-log QUALI

par(mfrow=c(1,2))

list_plot_loglog <- lapply(c('before_2008','centre'), function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                                     xlab="Temps post  sortie (jours)", mark.time=FALSE))
par(mfrow=c(1,1))


################## Modèle retenu

# Construction du modèle de base

model_greffe <- coxph(Surv(TpsEvtYear, Evt) ~ 
                          # Variables du receveur
                          ageR + imcR2 + sexeR + malIni2cl + hemodial #3cl non convergent
                          + anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac +
                          # Variables de la greffe 
                          sqrt(Tdial) + Tabm + incompABDR + before_2008 + strata(centre)
                        ,data = df)
summary(model_greffe) 

# Export du modèle de base en pdf

model_greffe_df <- as.data.frame(tidy(model_greffe))
model_greffe_df$hr <- exp(model_greffe_df$estimate)
model_greffe_df <- model_greffe_df[c(2,6,3:5)]
rownames(model_greffe_df) <- c('Recipient Age','Recipient BMI (squared)','Recipient men','Relapsing initial nephropathy','Hemodialysis',
                                 'History of diabetes','History of Dyslipidemia','History of hypertension',
                                 'History of cardiovascular disease','History of neoplasia',
                                 'Positive recipient CMV serology','Positive recipient AcHBs serology','History of smoking','Time on dialysis (sqrt)',
                                 'Time on waiting list','HLA A-B-DR incompatibilities','Transplanted before 2008')
colnames(model_greffe_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_greffe_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_mod_greffe.pdf')







#####################################################################################################################
# SELECTION VARIABLES DONNEUR
#####################################################################################################################

################## Sélection univariée

# Variables
var_donneur <- c(var_univ_donneur_quanti,var_univ_donneur_quali,'imcD')

# Génération de la table
tab_univ_donneur <- round(sapply(df[,var_donneur],function(x) summary(coxph(Surv(df$TpsEvtYear,df$Evt)~df$ageR + df$imcR2 + df$sexeR + df$malIni2cl + df$hemodial +
                                                                            df$anteDiab + df$anteDyslip + df$anteHTA + df$anteCardioVasc + df$anteNeo + df$cmvR + df$AcHBsR + df$Tabm + df$incompABDR + df$before_2008 +sqrt(df$Tdial) + df$tabac + strata(df$centre) + x))$coefficients[18,]),4)

tab_univ_donneur <- t(tab_univ_donneur)
tab_univ_donneur
colnames(tab_univ_donneur) <- c('\\beta','HR','SE(\\beta)','Z','p')
rownames(tab_univ_donneur) <- c('Donor age','Donor Height','Donor Weight','Last donor diuresis','Donor urea','Donor creatinine','Donor men',
                                'Donor after cardiac death','Donor death vascular etiology','Donor history of hypertension','Donor history of diabetes',
                                'Donor history of cardiac arrest','Donor proteinuria positive','Donor CMV serology positive','Donor BMI')



# Export en PDF

tab_univ_donneur %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_univ_donneur.pdf')



# Construction du modèle enrichi

model_complet <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                 # Variables du receveur
                                 ageR + imcR + sexeR + malIni2cl + hemodial +
                                 anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac +
                                 # Variables de la greffe
                                 sqrt(Tdial) + Tabm + incompABDR + before_2008 + strata(centre)
                                 # Variables du donneur
                                 ageD + tailleD + sexeD + causeDCD2cl + htaD + diabD + arretD + cmvD
                               ,data = df)
summary(model_complet) 


################## Log-linéarité

## Age donneur -- Passer au carré
df$ageD2 <- df$ageD**2
df$ageDlog <- log(1+df$ageD)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~ageD+sqrt(ageD)+ageD2+ageDlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$ageD),])


## Taille Donneur -- Non LL
df$tailleD2 <- df$tailleD**2
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~tailleD+sqrt(tailleD)+tailleD2+log(tailleD),data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$tailleD),])

## Poids Donneur -- Non LL
df$poidsD2 <- df$poidsD**2
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~poidsD+sqrt(poidsD)+poidsD2+log(poidsD),data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$poidsD),])

## IMC -- OK
df$imcD2 <- df$imcD**2
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~imcD+sqrt(imcD)+imcD2+log(imcD),data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$imcD),])

### => Garder l'imc

## Diurese -- passer au log
df$diureseD2 <- df$diureseD**2
df$diureseDlog <- log(df$diureseD+1)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~diureseD+sqrt(diureseD)+diureseD2+diureseDlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$diureseD),])
# Nouveau test univarié - AJOUT AU MODELE
coxph(Surv(TpsEvtYear, Evt) ~ ageR + imcR + sexeR + malIni2cl + hemodial + anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + Tabm + Tdial + incompABDR + before_2008 + diureseDlog + strata(centre),data = df)


## Uree -- passer au log
df$ureeD2 <- df$ureeD**2
df$ureeDlog <- log(df$ureeD+1)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~ureeD+sqrt(ureeD)+ureeD2+ureeDlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$ureeD),])
# Nouveau test univarié
coxph(Surv(TpsEvtYear, Evt) ~ ageR + strata(centre) + imcR + sexeR + malIni2cl + hemodial + anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + Tabm + Tdial + incompABDR + before_2008 + ureeDlog,data = df)

## Creat -- OK
df$creatD2 <- df$creatD**2
df$creatDlog <- log(1+df$creatD)
mart_res_null <- coxph(Surv(TpsEvtYear,Evt)~creatD+sqrt(creatD)+creatD2+creatDlog,data=df)
ggcoxfunctional(mart_res_null, data = df[!is.na(df$creatD),])


################## Proportionnalité des risques

## résidus de Schoenfeld 
par(mfrow=c(4,4))

list_plot_schoen <- lapply(c(var_donneur), function(x) plot(main=x,cox.zph(coxph(Surv(df$TpsEvtYear,df$Evt)~df[,x]) )))


## plots log-log QUALI

par(mfrow=c(3,3))

list_plot_loglog <- lapply(var_donneur[7:14], function(x) plot(main=x,survfit(Surv(TpsEvtYear,Evt)~df[,x],data=df) , fun=log.minus.log, ylab="log(-log(S(Temps)))", col=1:2, lty=1:2, 
                                                           xlab="Temps post  sortie (jours)", mark.time=FALSE))


par(mfrow=c(1,1))

################## Modèle retenu

# Construction du modèle de base

model_complet_taillepoids <- coxph(Surv(TpsEvtYear, Evt) ~ 
                         # Variables du receveur
                         ageR + imcR + sexeR + malIni2cl + hemodial +
                         anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac +
                         # Variables de la greffe
                         sqrt(Tdial) + Tabm + incompABDR + before_2008 + strata(centre) +
                         # Variables du donneur
                         ageD2 + tailleD + sexeD + causeDCD2cl + htaD + diabD + arretD + cmvD + diureseDlog
                       ,data = df)
summary(model_complet_taillepoids) 
model_complet_taillepoids$loglik

model_complet_imc <- coxph(Surv(TpsEvtYear, Evt) ~ 
                                     # Variables du receveur
                                     ageR + imcR + sexeR + malIni2cl + hemodial +
                                     anteDiab + anteDyslip + anteHTA + anteCardioVasc + anteNeo + cmvR + AcHBsR + tabac +
                                     # Variables de la greffe
                                     sqrt(Tdial) + Tabm + incompABDR + before_2008 + strata(centre) +
                                     # Variables du donneur
                                     ageD2 + imcD + sexeD + causeDCD2cl + htaD + diabD + arretD + cmvD + diureseDlog
                                   ,data = df)
summary(model_complet_imc) 
model_complet_imc$loglik # On conserve l'imc

model_complet <- model_complet_imc


# Export du modèle de base en pdf

model_complet_df <- as.data.frame(tidy(model_complet))
model_complet_df$hr <- exp(model_complet_df$estimate)
model_complet_df <- model_complet_df[c(2,6,3:5)]
rownames(model_complet_df) <- c('Recipient Age','Recipient BMI (squared)','Recipient men','Relapsing initial nephropathy','Hemodialysis','History of smoking',
                               'History of diabetes','History of Dyslipidemia','History of hypertension',
                               'History of cardiovascular disease','History of neoplasia',
                               'Positive recipient CMV serology','Positive recipient AcHBs serology','Time on dialysis (sqrt)','Time on waiting list',
                               'HLA A-B-DR incompatibilities','Transplanted before 2008','Donor age (squared)','Donor BMI','Donor men','Donor death vascular etiology','Donor history of hypertension','Donor history of diabetes',
                               'Donor history of cardiac arrest','Donor CMV serology positive','Last donor diuresis')
colnames(model_complet_df) <- c('\\beta','HR','SE(\\beta)','statistic','p')

model_complet_df %>%
  kable("latex", booktabs = T, escape = F) %>%
  save_kable(file='/home/corentin/Documents/Stage/Projet R/Tables/Univarié/Cox/cox_mod_complet.pdf')



