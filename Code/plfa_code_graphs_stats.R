library(dplyr)
library(ggplot2)
library(GGally)
library(readr)
library(vegan)
library(emmeans)
library(tidyverse)
library(lme4)
library(nlme)
library(Hmisc)
library(grid)

plot_boxplot <- function(graphvar, yvar, xvar, fillvar=NULL, title=NULL) {
  
  ggplot(data=graphvar, aes_string(x = xvar, y = yvar, fill=xvar)) +
    geom_boxplot(position = position_dodge(0.9))+ 
    facet_wrap(fillvar)+
    theme(axis.title = element_text())+
    theme(plot.title = element_text(hjust = 0.5))+
    #color order - green (omtD), blue (OMTF), weird purple (LMT), brown (CMT)
    scale_fill_manual(values=c("#00BA38", "#619CFF", "#F8766D" ))
}

plot_linegraph <-function(graphvar, yvar, xvar, fillvar=NULL, title=NULL) {
  ggplot(data=graphvar, aes_string(x=xvar, y=yvar, colour=fillvar, group = fillvar)) +
    geom_line(lwd=2) +
    theme(axis.title = element_text())+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle(paste0('Timepoint ', title))+
    #color order - green (omtD), blue (OMTF), weird purple (LMT), brown (CMT)
    scale_fill_manual(values=c("#00BA38", "#619CFF", "#F8766D" ))
}

aggregate_data <- function(x, num_var, ...){
  group_var <- quos(...)
  num_var <- enquo(num_var)
  
  x %>%
    group_by(!!!group_var) %>%
    summarize(avg = mean(!!num_var,na.rm=TRUE), n = sum(!is.na(!!num_var)), 
              sd = sd(!!num_var, na.rm=TRUE), se = sd/sqrt(n), sum=sum(!!num_var, na.rm=TRUE))
}

#import data file
library(readr)
PLFA_2018 <- read_csv("PLFA_data.csv",col_types = cols(crop = col_factor(levels = c("T", "C")), depth_lower = col_factor(levels = c("15","60", "100")), depth_upper = col_factor(levels = c("0","15", "60")), mgmttype = col_factor(levels = c("OMT","LMT", "CMT")), timepoint = col_factor(levels = c("TP1","TP2", "TP3", "TP4"))))
View(PLFA_2018)

#replace all blank spaces with 0
PLFA_2018[is.na(PLFA_2018)] <- 0
PLFA_2018_nozero=PLFA_2018

#calculate ratios used in the paper
PLFA_2018_nozero$KMScyclo2<-PLFA_2018_nozero$`099: 19:0 cyclo w7c`/PLFA_2018_nozero$`081: 18:1 w7c`

PLFA_2018_nozero$monounsat=PLFA_2018_nozero$`052: 16:1 w5c`+PLFA_2018_nozero$`050: 16:1 w7c`+PLFA_2018_nozero$`079: 18:1 w9c`+PLFA_2018_nozero$`081: 18:1 w7c`

PLFA_2018_nozero$sat =PLFA_2018_nozero$`011: 12:0`+PLFA_2018_nozero$`026: 14:0`+PLFA_2018_nozero$`041: 15:0`+PLFA_2018_nozero$`055: 16:0`+PLFA_2018_nozero$`071: 17:0`+PLFA_2018_nozero$`112:20:00`

PLFA_2018_nozero$Sat_Unsat=PLFA_2018_nozero$sat/PLFA_2018_nozero$monounsat

#code used to generate graphs
levels(PLFA_2018_nozero$depth_upper)<-c("0-15cm", "15-60cm", "60-100 cm")

timepoint_list=split(PLFA_2018_nozero, f=PLFA_2018_nozero$timepoint)

#automate summary graphs - designate the response variables used to make the graphs
variables=names(PLFA_2018_nozero)[c(9,10,63:67)]
variables=set_names(variables, variables)
all_graphs_boxplot=map(timepoint_list,~map(variables, plot_boxplot, graphvar=.x, xvar="mgmttype", fillvar= "depth_upper", title= (.x[1,]) ))

all_graphs_boxplot$TP2$mic_c
#color order - green (omt), blue (LMT), red (CMT)

sat_plot=all_graphs_boxplot$TP2$Sat_Unsat+
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(), axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = "Saturated:Unsaturated Ratio") +
  theme(axis.title = element_text(size=9, vjust=1))+
  theme(legend.position="none")+
  geom_jitter(position=position_jitter(0.2))+
  theme(axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+
  #coord_cartesian(ylim =c(0,4))+
  ggtitle(label=element_blank())

grampos_plot=all_graphs_boxplot$TP2$Grampos_Gramneg+
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(), axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = "Gram+:Gram- Ratio") +
  theme(legend.position="none")+
  geom_jitter(position=position_jitter(0.2))+
  theme(axis.title = element_text(size=9, vjust=1))+
  theme(axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+
  ggtitle(label=element_blank())

cyclo_plot=all_graphs_boxplot$TP2$KMScyclo2+
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(), axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = "Cyclo 19:pre Ratio") +
  theme(axis.title = element_text(size=9, vjust=1))+
  theme(legend.position="none")+
  geom_jitter(position=position_jitter(0.2))+
  theme(axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+
  #coord_cartesian(ylim =c(0,4))+
  ggtitle(label=element_blank())

mic_c=all_graphs_boxplot$TP2$mic_c+
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(), axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y =expression(paste( "Microbial Carbon (kg/ha)"))) +
  theme(axis.title = element_text(size=9, vjust=1))+
  theme(legend.position="none")+
  geom_jitter(position=position_jitter(0.2))+
  theme(axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+
  #coord_cartesian(ylim =c(0,4))+
  ggtitle(label=element_blank())

mic_c

PLFA_figure <- ggarrange(mic_c, sat_plot, grampos_plot, cyclo_plot,
                    ncol = 2, nrow = 2)
PLFA_figure


#significance testing
PLFA_Stats<-PLFA_2018_nozero%>%
  filter(depth_upper=="0-15cm" & timepoint=="TP2")

#repeated this set of code for each indicator
Groups_mod<- lm(mic_c ~ mgmttype, data = PLFA_Stats)

TP2_L = emmeans(Groups_mod,pairwise~mgmttype, ref=1)
results<-summary(TP2_L, infer = T)$contrast
results



