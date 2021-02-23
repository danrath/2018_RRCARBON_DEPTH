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
library(dplyr)


library(readr)
c_linegraph <- read_csv("historic_c_n_data.csv", 
                        col_types = cols(depth_upper = col_factor(levels = c("60", 
                                                                             "15", "0")), mgmttype = col_factor(levels = c("CMT", 
                                                                                                                           "LMT", "OMT"))))
View(c_linegraph)

#generate kg/ha values
c_linegraph$c_kgha_93=c_linegraph$bulkD_93*c_linegraph$depth_fraction*10000*(c_linegraph$percC_93/100)
c_linegraph$c_kgha_03=c_linegraph$bulkD_07*c_linegraph$depth_fraction*10000*(c_linegraph$percC_03/100)
c_linegraph$c_kgha_12=c_linegraph$bulkD_12*c_linegraph$depth_fraction*10000*(c_linegraph$percC_12/100)
c_linegraph$c_kgha_18=c_linegraph$bulkD_12*c_linegraph$depth_fraction*10000*(c_linegraph$percC_18/100)

#summarize kg/ha values
c_linegraph2<-c_linegraph%>%
  group_by(plot, mgmttype)%>%
  summarize(sum=sum(deltaC_93_18_kgha))

#generate delta C values for entire profile
deltaC_93_18=aggregate_data(c_linegraph, deltaC_93_18_kgha, mgmttype, depth_upper)
deltaC_wholeprofile=aggregate_data(c_linegraph2, sum, mgmttype)
deltaC_wholeprofile$depth_upper<-"whole"
deltaC_93_18_whole <- rbind(deltaC_wholeprofile, deltaC_93_18)
deltaC_93_18_whole$depth_upper <- factor(deltaC_93_18_whole$depth_upper, levels = c("whole", "60", 
                                                                                    "15", "0"))

#generate graph
deltac_9318 <- ggplot(deltaC_93_18_whole, aes(x=depth_upper, y=avg, fill=mgmttype), na.rm=TRUE)+   ##making a bar plot with SE as error bars and top of bar is mean mass of nitrate per system
  geom_bar(stat="identity",width = .8,  position=position_dodge())+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),  #use SE for error bars 
                width=0.1, color="black", size=0.5,                    # Width of the error bars
                position=position_dodge(0.8))+
  coord_flip()+
  labs(y=expression(Change~In~Total~C~from~1993~to~2018~(Mg/ha)),x="Depth (cm)")+
  theme(#panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(size = (14), hjust = 0.5),
    axis.text = element_text(size=12, color = "black"),
    axis.title.x = element_text(size=12, color = "black"),
    axis.title.y = element_text(size=12, color = "black"),
    #panel.background = element_blank(), 
    axis.line = element_line(colour = "black"), legend.title = element_blank())

deltac_93_18_graph <- deltac_9318+ scale_fill_manual(values=c("#F8766D", "#619CFF","#00BA38"), labels = c("CONV", "CONV+WCC", "ORG"), guide = guide_legend(reverse=TRUE)) +
  geom_hline(yintercept = 0,size=0.5) +
  scale_x_discrete (labels=c("0"="0-15", "15"="15-60", "60"="60-100", "whole"="Whole Profile"))

#statistical test code
c_linegraph1<-c_linegraph%>%
  subset(depth_upper=="0")

c_linegraph2<-c_linegraph%>%
  group_by(plot, mgmttype)%>%
  summarize(sum=sum(deltaC_93_18_kgha))

c_mod<- lm(deltaC_93_18_kgha ~ mgmttype, data = c_linegraph1)
anova(c_mod)
summary(emmeans(c_mod,~mgmttype),infer = c(T,F), level=.95)
specific_variety_effects <- emmeans(c_mod,pairwise~mgmttype)
summary(specific_variety_effects$contrasts,infer=T)
summary(c_mod)
TukeyHSD(c_mod)