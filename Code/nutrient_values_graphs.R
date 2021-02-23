library(ggplot2)
library(GGally)
library(CCA)
library(readr)
library(vegan)
library(Rmisc)
library(multcomp)
library(emmeans)
library(purrr)
library(lme4)
library(nlme)
library(Hmisc)
library(ggcorrplot)
library(ggpubr)
library(grid) 
library(tidyverse)



Compiled_Data_2018<-read_csv("nutrient_values.csv", 
                             col_types = cols(crop = col_factor(levels = c("C","T")), 
                                              mgmttype = col_factor(levels = c("OMT","LMT", "CMT")), 
                                              timepoint = col_factor(levels = c("TP1","TP2", "TP3", "TP4")),
                                              depth_upper = col_factor(levels = c("0","15", "60")))
                             
)

Compiled_Data_2018$totaln<-Compiled_Data_2018$nitrate+Compiled_Data_2018$ammonium
Compiled_Data_2018$totaln_kg_ha<-Compiled_Data_2018$nitrate_kg_ha+Compiled_Data_2018$ammonium_kg_ha


#code for graphs - 60-100 cm
Compiled_Data_20182<-Compiled_Data_2018%>%
  filter(depth_lower=="100")

doc_sum2<-Compiled_Data_20182%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_doc=sum(doc_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_doc), se=sd(sum_doc)/sqrt(n()))

totaln_sum2<-Compiled_Data_20182%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_n=sum(totaln_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_n), se=sd(sum_n)/sqrt(n()))

phosphorus_sum2<-Compiled_Data_20182%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_p=sum(phosphorus_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_p), se=sd(sum_p)/sqrt(n()))

sulfur_sum2<-Compiled_Data_20182%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_s=sum(sulfur_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_s), se=sd(sum_s)/sqrt(n()))

doc_60100<-ggplot(data=doc_sum2,
                  aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "DOC (kg/ha)") +
  theme(legend.position="none")+
  #coord_cartesian(ylim = c(0, 1000)) +
  #ggtitle("DOC Sum")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
doc_60100

totaln_60100<-ggplot(data=totaln_sum2,
                     aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "Total N \n (NO3+NH4) (kg/ha)") +
  # coord_cartesian(ylim = c(0, 150)) +
  theme(legend.position="none")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
totaln_60100

phosphorus_60100<-ggplot(data=phosphorus_sum2,
                         aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "Phosphorus (kg/ha)") +
  coord_cartesian(ylim = c(0, 40)) +
  theme(legend.position="none")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
phosphorus_60100

sulfur_60100<-ggplot(data=sulfur_sum2,
                     aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "Sulfur (kg/ha)") +
  coord_cartesian(ylim = c(0, 42)) +
  theme(axis.text.y = element_text(color = "grey20", size = 9,vjust = 1, face = "plain"))+
  theme(legend.position="none")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
sulfur_60100



figure2 <- ggarrange(doc_60100, totaln_60100, phosphorus_60100, sulfur_60100,
                     ncol = 2, nrow = 2)
figure2


#code for graphs 0-15 cm

Compiled_Data_20183<-Compiled_Data_2018%>%
  filter(depth_lower=="15")

doc_sum3<-Compiled_Data_20183%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_doc=sum(doc_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_doc), se=sd(sum_doc)/sqrt(n()))

totaln_sum3<-Compiled_Data_20183%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_n=sum(totaln_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_n), se=sd(sum_n)/sqrt(n()))

phosphorus_sum3<-Compiled_Data_20183%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_p=sum(phosphorus_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_p), se=sd(sum_p)/sqrt(n()))

sulfur_sum3<-Compiled_Data_20183%>%
  group_by(plot, timepoint, mgmttype)%>%
  dplyr::summarise(sum_s=sum(sulfur_kg_ha, na.rm=TRUE))%>%
  group_by(timepoint, mgmttype)%>%
  dplyr::summarise(avg=mean(sum_s), se=sd(sum_s)/sqrt(n()))



doc_015<-ggplot(data=doc_sum3,
                aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "DOC from 0-15 cm \n (kg/ha)") +
  theme(legend.position="none")+
  coord_cartesian(ylim = c(0, 300)) +
  #ggtitle("DOC Sum")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
doc_015

totaln_015<-ggplot(data=totaln_sum3,
                   aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "Total N from 0-15 cm \n (NO3+NH4) (kg/ha)") +
  coord_cartesian(ylim = c(0, 53)) +
  theme(legend.position="none")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
totaln_015

phosphorus_015<-ggplot(data=phosphorus_sum3,
                       aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "Phosphorus from 0-15 cm \n  (kg/ha)") +
  coord_cartesian(ylim = c(0, 410)) +
  #ggtitle("DOC Sum")+
  theme(legend.position="none")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
phosphorus_015

sulfur_015<-ggplot(data=sulfur_sum3,
                   aes(x=timepoint, y=avg, colour=mgmttype, group = mgmttype)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1, lwd=1) +
  geom_line(lwd=1.5)+ labs(x=NULL, y = "Sulfur from 0-15 cm \n  (kg/ha)") +
  coord_cartesian(ylim = c(0, 40)) +
  #ggtitle("DOC Sum")+
  theme(axis.text.y = element_text(color = "grey20", size = 9,vjust = 1, face = "plain"))+
  theme(legend.position="none")+
  scale_color_manual(labels=c("Organic", "Mixed", "Conventional"), values=c("#00BA38",  "#619CFF","#F8766D" ))+
  scale_x_discrete(labels=c("TP1" = "Feb", "TP2" = "Jun",
                            "TP3" = "Aug", "TP4" = "Feb"))
sulfur_015



figure4 <- ggarrange(doc_015, totaln_015, phosphorus_015, sulfur_015,
                     ncol = 2, nrow = 2)
figure4



#nutrient stats code - isolate each depth and test separately
Stats<-Compiled_Data_2018%>%
  filter(depth_upper=="60")

doc_mod<- aov(doc ~ mgmttype+timepoint, data = Stats)
anova(doc_mod)
summary(doc_mod)
TukeyHSD(doc_mod)

totaln_mod<- aov(totaln ~ mgmttype+timepoint, data = Stats)
anova(totaln_mod)
summary(totaln_mod)
TukeyHSD(totaln_mod)

phosphorus_mod<- aov(phosphorus ~ mgmttype+timepoint, data = Stats)
anova(phosphorus_mod)
summary(phosphorus_mod)
TukeyHSD(phosphorus_mod)

sulfur_mod<- aov(sulfur ~ mgmttype+timepoint, data = Stats)
anova(sulfur_mod)
summary(sulfur_mod)
TukeyHSD(sulfur_mod)

specific_variety_effects <- emmeans(totaln_mod,pairwise~mgmttype)
summary(specific_variety_effects$contrasts,infer=T)

