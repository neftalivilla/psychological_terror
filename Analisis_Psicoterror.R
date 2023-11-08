setwd("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/VARIOS/Victor/Victor Psicoterror")
####Library#####

library(tidyverse)
library(haven)
library(readxl)
library(na.tools)
library(ggthemes)
library(ggalt)
library(ggpubr)
library(janitor)
library(epiR)
library(fmsb)
library(ggplotify)
library(cowplot)

####Read dataset#####
base<-read_sav("Psicoterror_final.sav")
base<-janitor::clean_names(base)

####Prevalence by Type and Year#####

#Psicoterror 
ncas <- table(base$lyman_p80)[2]; npop <- sum(!is.na(base$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Type of Residency
#Medicina Interna
ncas <- table(base[base$tipo_especialidad==1,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$tipo_especialidad==1,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

table(base$tipo_especialidad,base$lyman_p80)
prop.table(table(base$tipo_especialidad,base$lyman_p80),1)*100
#Cirugia
ncas <- table(base[base$tipo_especialidad==2,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$tipo_especialidad==2,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Imagen, Audiologia, Patologia
ncas <- table(base[base$tipo_especialidad==3,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$tipo_especialidad==3,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Psiquatria
ncas <- table(base[base$tipo_especialidad==4,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$tipo_especialidad==4,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig1.df<-rbind(prev1,prev2,prev3,prev4)
prev.fig1.df$group<-c("Internal Medicine","Surgery", "Radiology, Audiology and Pathology", "Psychiatry")

#Year
#1 Year
ncas <- table(base[base$gradodeespecialidad==1,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$gradodeespecialidad==1,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#2 Year
ncas <- table(base[base$gradodeespecialidad==2,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$gradodeespecialidad==2,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#3 Year
ncas <- table(base[base$gradodeespecialidad==3,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$gradodeespecialidad==3,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#4 Year
ncas <- table(base[base$gradodeespecialidad==4,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$gradodeespecialidad==4,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#5 Year
ncas <- table(base[base$gradodeespecialidad==5,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$gradodeespecialidad==5,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig2.df<-rbind(prev1,prev2,prev3,prev4,prev5)
prev.fig2.df$group<-c("1st", "2nd", "3rd","4th", "5th")

####Prevalence by Type and year stratified by Sex####

#Mujeres
ncas <- table(base[base$sexo==0,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$sexo==0,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Hombre 
ncas <- table(base[base$sexo==1,]$lyman_p80)[2]; npop <- sum(!is.na(base[base$sexo==1,]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig1.df.sex<-rbind(prev1,prev2)
prev.fig1.df.sex$group<-c("Women", "Men")

#Type of Residency
#Women
#Medicina Interna
ncas <- table(base[(base$sexo==0 & base$tipo_especialidad==1),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$tipo_especialidad==1),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Cirugia
ncas <- table(base[(base$sexo==0 & base$tipo_especialidad==2),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$tipo_especialidad==2),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Imagen, Audiologia, Patologia
ncas <- table(base[(base$sexo==0 & base$tipo_especialidad==3),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$tipo_especialidad==3),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Psiquatria
ncas <- table(base[(base$sexo==0 & base$tipo_especialidad==4),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$tipo_especialidad==4),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig2.df.sex.1<-rbind(prev1,prev2,prev3,prev4)
prev.fig2.df.sex.1$group<-c("Internal Medicine","Surgery", "Radiology, \nAudiology and \nPathology", "Psychiatry")
prev.fig2.df.sex.1$class<-c("Women")
#Men
#Medicina Interna
ncas <- table(base[(base$sexo==1 & base$tipo_especialidad==1),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$tipo_especialidad==1),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Cirugia
ncas <- table(base[(base$sexo==1 & base$tipo_especialidad==2),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$tipo_especialidad==2),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Imagen, Audiologia, Patologia
ncas <- table(base[(base$sexo==1 & base$tipo_especialidad==3),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$tipo_especialidad==3),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#Psiquatria
ncas <- table(base[(base$sexo==1 & base$tipo_especialidad==4),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$tipo_especialidad==4),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig2.df.sex.2<-rbind(prev1,prev2,prev3,prev4)
prev.fig2.df.sex.2$group<-c("Internal Medicine","Surgery", "Radiology, \nAudiology and \nPathology", "Psychiatry")
prev.fig2.df.sex.2$class<-c("Men")
prev.fig2.df.sex<-rbind(prev.fig2.df.sex.1,prev.fig2.df.sex.2)

#Women
#Year
#1 Year
ncas <- table(base[(base$sexo==0 & base$gradodeespecialidad==1),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$gradodeespecialidad==1),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#2 Year
ncas <- table(base[(base$sexo==0 & base$gradodeespecialidad==2),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$gradodeespecialidad==2),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#3 Year
ncas <- table(base[(base$sexo==0 & base$gradodeespecialidad==3),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$gradodeespecialidad==3),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#4 Year
ncas <- table(base[(base$sexo==0 & base$gradodeespecialidad==4),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$gradodeespecialidad==4),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#5 Year
ncas <- table(base[(base$sexo==0 & base$gradodeespecialidad==5),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==0 & base$gradodeespecialidad==5),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig3.df.sex.1<-rbind(prev1,prev2,prev3,prev4,prev5)
prev.fig3.df.sex.1$group<-c("1st", "2nd", "3rd","4th", "5th")
prev.fig3.df.sex.1$class<-c("Women")

#Men
#Year
#1 Year
ncas <- table(base[(base$sexo==1 & base$gradodeespecialidad==1),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$gradodeespecialidad==1),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#2 Year
ncas <- table(base[(base$sexo==1 & base$gradodeespecialidad==2),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$gradodeespecialidad==2),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#3 Year
ncas <- table(base[(base$sexo==1 & base$gradodeespecialidad==3),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$gradodeespecialidad==3),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#4 Year
ncas <- table(base[(base$sexo==1 & base$gradodeespecialidad==4),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$gradodeespecialidad==4),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev4<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

#5 Year
ncas <- table(base[(base$sexo==1 & base$gradodeespecialidad==5),]$lyman_p80)[2]; npop <- sum(!is.na(base[(base$sexo==1 & base$gradodeespecialidad==5),]$lyman_p80))
tmp <- as.matrix(cbind(ncas, npop))
prev5<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 500, design = 1, 
                      conf.level = 0.95) * 100,2)

prev.fig3.df.sex.2<-rbind(prev1,prev2,prev3,prev4,prev5)
prev.fig3.df.sex.2$group<-c("1st", "2nd", "3rd","4th", "5th")
prev.fig3.df.sex.2$class<-c("Men")
prev.fig3.df.sex<-rbind(prev.fig3.df.sex.1,prev.fig3.df.sex.2)

####Figure 1#####
prev.fig1.df$group<-factor(prev.fig1.df$group,levels = c("Internal Medicine","Surgery",
                                                         "Radiology, Audiology and Pathology",
                                                         "Psychiatry"))
Figure1A<-ggplot(prev.fig1.df, aes(x=group, y=est, fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width = 0.75) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_classic()+
  labs(fill="Medical Residency")+
  xlab("")+
  ylab("Prevalence, (%)")+
  ggsci::scale_fill_jama()+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 15),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(0,100))+ 
  theme(legend.position = "top")

leg<-ggpubr::get_legend(Figure1A)
leg<-as_ggplot(leg)

Figure1B<-ggplot(prev.fig2.df, aes(x=group, y=est, fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width = 0.75) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_classic()+
  labs(fill="Year of Residency")+
  xlab("")+
  ylab("Prevalence, (%)")+
  ggsci::scale_fill_npg()+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 25),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(0,100))+ 
  theme(legend.position = "top")

leg.2<-ggpubr::get_legend(Figure1B)
leg.2<-as_ggplot(leg.2)

prev.fig2.df.sex$group<-factor(prev.fig2.df.sex$group,levels = c("Internal Medicine","Surgery",
                                                         "Radiology, \nAudiology and \nPathology",
                                                         "Psychiatry"))
Figure1C<-ggplot(prev.fig2.df.sex, aes(x=class, y=est, fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width = 0.85) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_classic()+
  facet_wrap(~group,nrow = 1)+
  labs(fill="Medical Residency")+
  xlab("")+
  ylab("Prevalence, (%)")+
  ggsci::scale_fill_jama()+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 30),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(0,100))+ 
  theme(legend.position = "top")

Figure1D<-ggplot(prev.fig3.df.sex, aes(x=class, y=est, fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(),width = 0.85) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_classic()+
  facet_wrap(~group,nrow = 1)+
  labs(fill="Year of Residency")+
  xlab("")+
  ylab("Prevalence, (%)")+
  ggsci::scale_fill_npg()+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 45),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(0,100))+ 
  theme(legend.position = "top")

Figure1A<-ggarrange(Figure1A,Figure1C,ncol = 1,nrow = 2,labels = c("A","C"),common.legend = T)
Figure1B<-ggarrange(Figure1B,Figure1D,ncol = 1,nrow = 2,labels = c("B","D"),common.legend = T)
Figure1<-ggarrange(Figure1A,Figure1B,ncol = 2,nrow = 1)
#ggsave(Figure1,filename = "Figure1.pdf", width = 55, height = 30,units=c("cm"),dpi = 450,limitsize = FALSE)


####Figure 2#####

Fig2A.df<-base %>%filter(lyman_p80==1) %>% group_by(tipo_especialidad) %>%
  dplyr::select(desprestigio_laboral,entorpecimiento_progreso,
                incomunicacion,intimidacion_encubierta,intimidacion_manifiesta,
                desprestigio_personal)%>%
  na.omit()%>%
  summarise(Des_Lab = median(desprestigio_laboral),
            Ent_Pro = median(entorpecimiento_progreso),
            Incom = median(incomunicacion),
            Int_Encu = median(intimidacion_encubierta),
            Int_Mani = median(intimidacion_manifiesta),
            Dep_Pers = median(desprestigio_personal))%>%
  as.data.frame()%>%
  dplyr::select(-1)%>%
  `colnames<-`(c("Laboral Stigma","Inappropriate Tasks","Inappropriate\nCommunication","Uncovered Bullying","Manifested Bullying","Personal Stigma"))%>%
  `rownames<-`(c("Internal Medicine",
                 "Surgery",
                 "Radiology, Audiology and Pathology",
                 "Psychiatry")) 
Fig2A.df<- rbind(rep(1.5,6) , rep(0,6) , Fig2A.df)
Fig2A.df<-as.data.frame(Fig2A.df)

Figure2A<-as.ggplot(~fmsb::radarchart(Fig2A.df,axistype=1, seg=2,maxmin=T,
                                      pcol=c("#3a4e56","#e1a86a","#31a0d8", "#ab4844"), plwd=4 , plty=1,cex.main=1.5,
                                      cglcol="grey30", cglty=1, axislabcol="gray35",caxislabels=seq(0, 1.5, 0.5), cglwd=0.8,
                                      vlcex=1.65,title = ""))+
  theme(plot.margin=unit(c(-1.25,-0.75,-2.5,-2.25),"cm"))

Figure2A<-plot_grid(Figure2A, leg, ncol = 1, rel_heights = c(10, 1))

#Año de Residencia

Fig2B.df<-base%>%filter(lyman_p80==1) %>% group_by(gradodeespecialidad) %>%
  dplyr::select(desprestigio_laboral,entorpecimiento_progreso,
                incomunicacion,intimidacion_encubierta,intimidacion_manifiesta,
                desprestigio_personal)%>%
  na.omit()%>%
  summarise(Des_Lab = median(desprestigio_laboral),
            Ent_Pro = median(entorpecimiento_progreso),
            Incom = median(incomunicacion),
            Int_Encu = median(intimidacion_encubierta),
            Int_Mani = median(intimidacion_manifiesta),
            Dep_Pers = median(desprestigio_personal))%>%
  as.data.frame()%>%
  dplyr::select(-1)%>%
  `colnames<-`(c("Laboral Stigma","Inappropriate Tasks","Inappropriate\nCommunication","Uncovered Bullying","Manifested Bullying","Personal Stigma"))%>%
  `rownames<-`(c("1st", "2nd", "3rd","4th", "5th")) 
Fig2B.df<- rbind(rep(2.5,6) , rep(0,6) , Fig2B.df)
Fig2B.df<-as.data.frame(Fig2B.df)

Figure2B<-as.ggplot(~fmsb::radarchart(Fig2B.df,axistype=1, seg=2,maxmin=T,
                                      pcol=c("#E64B35FF","#4DBBD5FF","#00A087FF", "#3C5488FF","#F39B7FFF"), plwd=4 , plty=1,cex.main=1.5,
                                      cglcol="grey30", cglty=1, axislabcol="gray35",caxislabels=seq(0, 2.5, 1), cglwd=0.8,
                                      vlcex=1.65,title = ""))+
  theme(plot.margin=unit(c(-1.25,-0.75,-2.5,-2.25),"cm"))

Figure2B<-plot_grid(Figure2B, leg.2, ncol = 1, rel_heights = c(10, 1))
Figure2<-ggarrange(Figure2A,Figure2B,ncol = 2,nrow = 1,labels = c("A","B"))
#ggsave(Figure2,filename = "Figure2.pdf", width = 40, height = 20,units=c("cm"),dpi = 450,limitsize = FALSE)


# Fig2C.df<-base %>%filter(lyman_p80==1) %>% group_by(tipo_especialidad) %>%
#   dplyr::select(pf,rp,bp,gh,vt,sf,re,mh)%>%
#   na.omit()%>%
#   summarise(Fun_Fisica = median(pf),
#             Pro_Fisicos = median(rp),
#             Dolor = median(bp),
#             Salud_Gen = median(gh),
#             Vitalidad = median(vt),
#             Fx_Social = median(sf),
#             Prob_Emo = median(re),
#             Sal_Mental = median(mh))%>%
#   as.data.frame()%>%
#   dplyr::select(-1)%>%
#   `colnames<-`(c("Physical Function",
#                  "Physical Problems",
#                  "Body Pain",
#                  "General Health Perception",
#                  "Vitality",
#                  "Social Functioning",
#                  "Social Limitations",
#                  "Mental health"))%>%
#   `rownames<-`(c("Internal Medicine",
#                  "Surgery",
#                  "Radiology, Audiology and Pathology",
#                  "Psychiatry")) 
# Fig2C.df<- rbind(rep(100,6) , rep(0,6) , Fig2C.df)
# Fig2C.df<-as.data.frame(Fig2C.df)
# 
# Figure2C<-as.ggplot(~fmsb::radarchart(Fig2C.df,axistype=1, seg=2,maxmin=T,
#                                       pcol=c("#3a4e56","#e1a86a","#31a0d8", "#ab4844"), plwd=4 , plty=1,cex.main=1.5,
#                                       cglcol="grey30", cglty=1, axislabcol="gray35",caxislabels=seq(0, 100, 50), cglwd=0.8,
#                                       vlcex=1.65,title = ""))+
#   theme(plot.margin=unit(c(-1.25,-0.75,-2.5,-2.25),"cm"))

#####Tabla 1#####

#Stratification by Physcological Terror
columns <- c('Parameter',"Without Physiological Terror (n=281)","With Physiological Terror (n=68)")

#Edad
num1<-c(paste(round(mean(base[base$lyman_p80==0,]$edad,na.rm = T),2),
              paste0('(',"±",round(sd(base[base$lyman_p80==0,]$edad,na.rm = T),2),")")))

num2<-c(paste(round(mean(base[base$lyman_p80==1,]$edad,na.rm = T),2),
              paste0('(',"±",round(sd(base[base$lyman_p80==1,]$edad,na.rm = T),2),")")))

Edad<-`names<-`(data.frame(matrix(c("Age (years)",num1,num2),ncol = 3)),columns)

## Male Female

sexo <- table(base$sexo,base$lyman_p80,useNA = "always")[c(2,5)]
sexoprop <- round(prop.table(table(base$sexo,base$lyman_p80,useNA = "always"),2),4)[c(2,5)]*100
Sexo<-`names<-`(data.frame("Men (%)",
                           matrix(c(paste(sexo,paste0('(',sexoprop,')'))),ncol = 2)),
                columns)
stats::chisq.test(table(base$sexo,base$lyman_p80),correct = T,simulate.p.value = T)


#Medical Residency
medical <- table(base$tipo_especialidad,base$lyman_p80,useNA = "always")[c(1:4,6:9)]
medicalprop <- round(prop.table(table(base$tipo_especialidad,base$lyman_p80,useNA = "always"),2),4)[c(1:4,6:9)]*100
Tipo_Resi<-`names<-`(data.frame(c("Internal Medicine (%)","Surgery (%)", "Radiology, Audiology and Pathology (%)", "Psychiatry (%)"),
                           matrix(c(paste(medical,paste0('(',medicalprop,')'))),ncol = 2)),
                columns)

#Year Residency
year <- table(base$gradodeespecialidad,base$lyman_p80,useNA = "always")[c(1:5,7:11)]
year.prop <- round(prop.table(table(base$gradodeespecialidad,base$lyman_p80,useNA = "always"),2),4)[c(1:5,7:11)]*100
Año_Resi<-`names<-`(data.frame(c("1st (%)","2nd (%)","3rd (%)","4th (%)","5th (%)"),
                                matrix(c(paste(year,paste0('(',year.prop,')'))),ncol = 2)),
                     columns)

#Salud Fisica
num1<-c(paste(round(median(base[base$lyman_p80==0,]$pcs_sp,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==0,]$pcs_sp,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==0,]$pcs_sp,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$lyman_p80==1,]$pcs_sp,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==1,]$pcs_sp,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==1,]$pcs_sp,na.rm = T,probs = c(0.75)),2),')')))

SF_Fisico<-`names<-`(data.frame(matrix(c("Physical Quality of Life (Pts)",num1,num2),ncol = 3)),columns)

#Salud Mental
num1<-c(paste(round(median(base[base$lyman_p80==0,]$mcs_sp,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==0,]$mcs_sp,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==0,]$mcs_sp,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$lyman_p80==1,]$mcs_sp,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==1,]$mcs_sp,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==1,]$mcs_sp,na.rm = T,probs = c(0.75)),2),')')))

SF_Mental<-`names<-`(data.frame(matrix(c("Mental Quality of Life (Pts)",num1,num2),ncol = 3)),columns)

#Ansiedad
num1<-c(paste(round(median(base[base$lyman_p80==0,]$bai,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==0,]$bai,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==0,]$bai,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$lyman_p80==1,]$bai,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==1,]$bai,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==1,]$bai,na.rm = T,probs = c(0.75)),2),')')))

Ansiedad<-`names<-`(data.frame(matrix(c("BAI-Anxiety Levels (Pts)",num1,num2),ncol = 3)),columns)

#Ansiedad Cat
ans <- table(base$bai_cat,base$lyman_p80,useNA = "always")[c(1:4,6:9)]
ans.prop <- round(prop.table(table(base$bai_cat,base$lyman_p80,useNA = "always"),2),4)[c(1:4,6:9)]*100
Ansiedad_CAT<-`names<-`(data.frame(c("Minimal-Anxiety (%)","Mild-Anxiety (%)","Moderate-Anxiety (%)","Severe-Anxiety (%)"),
                               matrix(c(paste(ans,paste0('(',ans.prop,')'))),ncol = 2)),
                    columns)

#Depresion

num1<-c(paste(round(median(base[base$lyman_p80==0,]$beck_suma,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==0,]$beck_suma,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==0,]$beck_suma,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$lyman_p80==1,]$beck_suma,na.rm = T ),2),
              paste0('(',round(quantile(base[base$lyman_p80==1,]$beck_suma,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$lyman_p80==1,]$beck_suma,na.rm = T,probs = c(0.75)),2),')')))

Depresion<-`names<-`(data.frame(matrix(c("BDI-Depression Levels (Pts)",num1,num2),ncol = 3)),columns)

#Depresion Cat
depre <- table(base$beck_cat,base$lyman_p80,useNA = "always")[c(1:4,6:9)]
depre.prop <- round(prop.table(table(base$beck_cat,base$lyman_p80,useNA = "always"),2),4)[c(1:4,6:9)]*100
Depresion_CAT<-`names<-`(data.frame(c("Minimal-Depression (%)","Mild-Depression (%)","Moderate-Depression (%)","Severe-Depression (%)"),
                                   matrix(c(paste(depre,paste0('(',depre.prop,')'))),ncol = 2)),
                        columns)

Table1<-rbind(Edad,Sexo,Tipo_Resi,Año_Resi,SF_Fisico,SF_Mental,Ansiedad,Ansiedad_CAT,Depresion,Depresion_CAT)
Table1_Flex<-flextable::align(flextable::flextable(Table1,cwidth=4),align="center",part="all")%>%flextable::autofit()
#flextable::save_as_docx(Table1_Flex,path="Table_1.docx")

wilcox.test(base$lyman~base$lyman_p80)
chisq.test(table(base$beck_cat==1,base$lyman_p80))
chisq.test(table(base$beck_cat==2,base$lyman_p80))
chisq.test(table(base$beck_cat==3,base$lyman_p80))
chisq.test(table(base$beck_cat==4,base$lyman_p80))


base$beck_cat