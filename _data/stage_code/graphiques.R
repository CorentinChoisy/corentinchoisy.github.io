# EXEMPLE
#####################################################################################################################
library(ggplot2)
library(ggrepel)
library(survival)
library(survminer)
library(plotly)
library(ggsurvfit)
library(gridExtra)
dff <- read.csv("~/Documents/Stage/StageR/Exo/Code UE3/Base.Projet.csv", sep=";")
dff <- dff[-which(dff$SUBJID==3865),]
dff <- dff[dff$DUREE_VENTILATION_MECANIQUE>=1,] # Intubés plus de 24h
dff <- dff[dff$Age>=18,] # Uniquement les adultes
dff$deces <- !is.na(dff$delai_deces)
Obj.Surv<-survfit(Surv(delai_der_nouvelle, deces) ~ 1, data = dff)
timelist <- c(750,1500,2250)
probsurv <- c(0.905,0.865,0.83)
dftt <- data.frame(timelist=timelist,probsurv=probsurv,label=c('90.5%','86.5%','83%'))
p1 <- ggsurvplot(fit = Obj.Surv, data = dff, pval = F, 
                 risk.table.y.text.col = T,
                 risk.table.y.text = FALSE,
                 linetype = c("solid", "solid","solid"),
                 risk.table = T,
                 title = "Courbe de survie",
                 risk.table.title = 'Effectif à risque',
                 xlab = "Temps post sortie réanimation - 7 jours (Jours)",
                 ylab = "Probabilité de survie",
                 xlim = c(0, 3100),
                 ylim = c(0,1),
                 censor = F,
                 surv.scale = "default",
                 fontsize = 3,
                 tables.theme = theme_light(),
                 break.time.by = 750,
                 risk.table.height=0.3,
                 conf.int = T,
                 legend.labs=c('All')) 

pp1 <- p1$plot + geom_point(aes(x=timelist, y=probsurv), data = dftt) + geom_text(aes(x=timelist, y=probsurv,label=label),vjust= -1.5, data = dftt) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10))

pt1 <- p1$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))

grid.arrange(
  grobs = list(pp1,pt1),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2)))


# Plot breaké

Obj.Surv<-survfit(Surv(delai_der_nouvelle, deces) ~ 1, data = dff)
p2 <- ggsurvplot(fit = Obj.Surv, data = dff, pval = F, 
                 risk.table.y.text.col = T,
                 risk.table.y.text = FALSE,
                 linetype = c("solid", "solid","solid"),
                 risk.table = T,
                 title = "Courbe de survie",
                 risk.table.title = 'Effectif à risque',
                 xlab = "Temps post sortie réanimation - 7 jours (Jours)",
                 ylab = "Probabilité de survie",
                 xlim = c(0, 3100),
                 ylim = c(0.6,1),
                 censor = F,
                 surv.scale = "default",
                 fontsize = 3,
                 tables.theme = theme_light(),
                 break.time.by = 750,
                 risk.table.height=0.3,
                 conf.int = F,
                 legend.labs=c('All'))

pp2 <- p2$plot + geom_point(aes(x=timelist, y=probsurv), data = dftt) + geom_text(aes(x=timelist, y=probsurv,label=label),vjust= -1.5, data = dftt) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0.6, 1), 
                     labels = c(0, 0.7, 0.8,0.9, 1)) +
  coord_cartesian(clip = "off", xlim = c(0, 3100)) +
  annotation_custom(grid::linesGrob(),
                    xmin = -90, xmax = -90, ymin = 0.57, ymax = 1) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA)),
                    xmin = -120, xmax = 0, ymin = 0.64, ymax = 0.67) +
  annotation_custom(grid::linesGrob(),
                    xmin = -150, xmax = -30, ymin = 0.66, ymax = 0.68) +
  annotation_custom(grid::linesGrob(),
                    xmin = -150, xmax = -30, ymin = 0.63, ymax = 0.65) +
  theme(axis.line.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10))
pt2 <- p2$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(pp2,pt2),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2)))





# Plot par groupes de motif d'admission
Obj.Surv<-survfit(Surv(delai_der_nouvelle, deces) ~ Motif_adm, data = dff)


summary(Obj.Surv)
timelist <- c(750,1500,2250,750,1500,2250,750,1500,2250)
probsurv <- c(0.920,0.898,0.861,0.857,0.799,0.762,0.949,0.907,0.898)
dftt <- data.frame(timelist=timelist,probsurv=probsurv,label=c('92%','89.8%','85.5%','86.1%','79.9%','76.2%','94.9%','90.7%','89.8%'))

p3 <- ggsurvplot(fit = Obj.Surv, data = dff, pval = F, 
                 risk.table.y.text.col = T,
                 risk.table.y.text = FALSE,
                 linetype = c("solid", "solid","solid"),
                 risk.table = T,
                 title = "Courbe de survie",
                 risk.table.title = 'Effectif à risque',
                 xlab = "Temps post sortie réanimation - 7 jours (Jours)",
                 ylab = "Probabilité de survie",
                 xlim = c(0, 3100),
                 ylim = c(0.6,1),
                 censor = F,
                 surv.scale = "default",
                 fontsize = 3,
                 tables.theme = theme_light(),
                 break.time.by = 750,
                 risk.table.height=0.3,
                 conf.int = F,
                 legend.labs=c('TC','CB','Trauma'))

pp3 <- p3$plot + geom_point(aes(x=timelist, y=probsurv), data = dftt) + geom_text_repel(aes(x=timelist, y=probsurv,label=label),vjust= -1, data = dftt) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0.6, 1), 
                     labels = c(0, 0.7, 0.8,0.9, 1)) +
  coord_cartesian(clip = "off", xlim = c(0, 3100)) +
  annotation_custom(grid::linesGrob(),
                    xmin = -90, xmax = -90, ymin = 0.57, ymax = 1) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA)),
                    xmin = -120, xmax = 0, ymin = 0.64, ymax = 0.67) +
  annotation_custom(grid::linesGrob(),
                    xmin = -150, xmax = -30, ymin = 0.66, ymax = 0.68) +
  annotation_custom(grid::linesGrob(),
                    xmin = -150, xmax = -30, ymin = 0.63, ymax = 0.65) +
  theme(axis.line.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10))
pt3 <- p3$table + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 10))
grid.arrange(
  grobs = list(pp3,pt3),
  layout_matrix = rbind(c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(1, 1),
                        c(2, 2),
                        c(2, 2)))
