library(ggplot2)
library(GGally)
library(readr)
library(ggcorrplot)
library(ggpubr)
library(tidyverse)


library(readr)
final_nutrient_summary <- read_csv("compiled_inputs_93_18.csv")
#View(compost_analyses_all)

#summarize data for graph
CNPSK_total<-final_nutrient_summary%>%
  filter(year<"2018")%>%
  group_by(plot, mgmttype, year)%>%
  summarise(
    compost_c_mean=mean(compost_c_kg_ha, na.rm=TRUE), 
    corn_c_mean=mean(corn_C_final, na.rm=TRUE), 
    tomato_c_mean=mean(tom_C_final, na.rm=TRUE),
    sudangrass_c_mean=mean(sudangrass_c_kg_ha, na.rm=TRUE),
    wheat_c_content_mean=mean(wheat_c_content_kg_ha, na.rm=TRUE),
    wcc_c_mean=mean(wcc_c_kgha, na.rm=TRUE), 
    compost_n_mean=mean(compost_n_kg_ha, na.rm=TRUE), 
    wcc_n_mean=mean(wcc_n_kgha, na.rm=TRUE), 
    fert_n_mean=mean(fert_N_kg_ha, na.rm=TRUE), 
    compost_p_mean=mean(compost_phosphorus_kg_ha, na.rm=TRUE), 
    fert_p_mean=mean(fert_P_kg_ha, na.rm=TRUE), 
    compost_s_mean=mean(compost_s_kg_ha, na.rm=TRUE), 
    fert_s_mean=mean(fert_S_kg_ha, na.rm=TRUE),
    compost_k_mean=mean(compost_k_kg_ha, na.rm=TRUE), 
    fert_k_mean=mean(fert_K_kg_ha, na.rm=TRUE),
  )%>%
  mutate(
    compost_c_sum=cumsum(compost_c_mean), 
    corn_c_sum=cumsum(corn_c_mean), 
    tomato_c_sum=cumsum(tomato_c_mean),
    wcc_c_sum=cumsum(wcc_c_mean), 
    sudangrass_c_sum=cumsum(sudangrass_c_mean),
    wheat_c_content_sum=cumsum(wheat_c_content_mean),
    compost_n_sum=cumsum(compost_n_mean), 
    wcc_n_sum=cumsum(wcc_n_mean), 
    fert_n_sum=cumsum(fert_n_mean), 
    compost_p_sum=cumsum(compost_p_mean), 
    fert_p_sum=cumsum(fert_p_mean), 
    compost_s_sum=cumsum(compost_s_mean), 
    fert_s_sum=cumsum(fert_s_mean),
    compost_k_sum=cumsum(compost_k_mean), 
    fert_k_sum=cumsum(fert_k_mean),
  )%>%
  group_by(mgmttype, year)%>%
  summarise(
    compost_c_mean=mean(compost_c_sum)/1000, 
    corn_c_mean=mean(corn_c_sum)/1000, 
    tomato_c_mean=mean(tomato_c_sum)/1000,
    sudangrass_c_mean=mean(sudangrass_c_sum)/1000,
    wheat_c_content_mean=mean(wheat_c_content_sum)/1000,
    wcc_c_mean=mean(wcc_c_sum)/1000, 
    compost_n_mean=mean(compost_n_sum)/1000, 
    wcc_n_mean=mean(wcc_n_sum)/1000, 
    fert_n_mean=mean(fert_n_sum)/1000, 
    compost_p_mean=mean(compost_p_sum)/1000, 
    fert_p_mean=mean(fert_p_sum)/1000, 
    compost_s_mean=mean(compost_s_sum)/1000, 
    fert_s_mean=mean(fert_s_sum)/1000,
    compost_k_mean=mean(compost_k_sum)/1000, 
    fert_k_mean=mean(fert_s_sum)/1000,
  )

CNPSK_cum_2017<-CNPSK_total%>%
  filter(year=="2017")



c_totals<-CNPSK_cum_2017%>%
  select(mgmttype, corn_c_mean, tomato_c_mean, wheat_c_content_mean, sudangrass_c_mean, compost_c_mean, wcc_c_mean)%>%
  gather(corn_c_mean, tomato_c_mean, wheat_c_content_mean, sudangrass_c_mean, compost_c_mean, wcc_c_mean, key="variable", value="value" )

n_totals<-CNPSK_cum_2017%>%
  select(mgmttype, compost_n_mean, wcc_n_mean, fert_n_mean)%>%
  gather(compost_n_mean, wcc_n_mean, fert_n_mean, key="variable", value="value" )

p_totals<-CNPSK_cum_2017%>%
  select(mgmttype, compost_p_mean, fert_p_mean)%>%
  gather(compost_p_mean, fert_p_mean, key="variable", value="value" )

s_totals<-CNPSK_cum_2017%>%
  select(mgmttype, compost_s_mean, fert_s_mean)%>%
  gather(compost_s_mean, fert_s_mean, key="variable", value="value" )

k_totals<-CNPSK_cum_2017%>%
  select(mgmttype, compost_k_mean, fert_k_mean)%>%
  gather(compost_k_mean, fert_k_mean, key="variable", value="value" )

#generate bar graphs
c_plot<-ggplot(c_totals, aes(fill=variable, y=value, x=mgmttype)) + 
  geom_bar(position="stack", stat="identity")+
  ylab("Carbon Inputs 1993-2018 (Mg/ha)") + 
  theme(axis.text.y = element_text(color = "grey20", size = 9,vjust = 1, face = "plain"))+
  scale_y_continuous(limits=c(0, 200))+
  xlab(NULL)+
  scale_x_discrete(labels=c("CMT" = "CONV", "LMT" = "CONV+WCC",
                            "OMT" = "ORG"))+
  scale_fill_manual(values=c("#b5651d","#00BA38", "#fdfd96","#ff6961", "#9B870C","#006400" ), 
                    # color layout - compost, cover crop, corn,tomato, wheat, sudangrass
                    name="C Sources",
                    breaks=c("compost_c_mean", "wcc_c_mean", "corn_c_mean", "tomato_c_mean", "wheat_c_content_mean", "sudangrass_c_mean"),
                    labels=c("Compost", "WCC", "Maize Residue", "Tomato Residue", "Wheat Residue", "Sudangrass Residue"))

n_plot<-ggplot(n_totals, aes(fill=variable, y=value, x=mgmttype)) + 
  geom_bar(position="stack", stat="identity")+
  ylab("Nitrogen Inputs 1993- 2018 (Mg/ha)") + 
  theme(axis.text.y = element_text(color = "grey20", size = 9,vjust = 1, face = "plain"))+
  scale_y_continuous(limits=c(0, 8))+
  xlab(NULL)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("CMT" = "CONV", "LMT" = "CONV+WCC",
                            "OMT" = "ORG"))+
  scale_fill_manual(values=c("#b5651d", "#00BA38", "#619CFF" ), 
                    name="Nutrient Sources",
                    breaks=c("compost_n_mean", "wcc_n_mean", "fert_n_mean"),
                    labels=c("Compost", "WCC", "Fertilizer"))

p_plot<-ggplot(p_totals, aes(fill=variable, y=value, x=mgmttype)) + 
  geom_bar(position="stack", stat="identity")+
  ylab("Phosphorus Inputs 1993- 2018  (Mg/ha)") + 
  theme(axis.text.y = element_text(color = "grey20", size = 9,vjust = 1, face = "plain"))+
  scale_y_continuous(limits=c(0, 3.5))+
  xlab(NULL)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("CMT" = "CONV", "LMT" = "CONV+WCC",
                            "OMT" = "ORG"))+
  scale_fill_manual(values=c("#b5651d", "#619CFF" ), 
                    name="P Sources",
                    breaks=c("compost_p_mean", "fert_p_mean"),
                    labels=c("Compost", "Fertilizer"))

s_plot<-ggplot(s_totals, aes(fill=variable, y=value, x=mgmttype)) + 
  geom_bar(position="stack", stat="identity")+
  ylab("Sulfur Inputs 1993- 2018 (Mg/ha)") +
  theme(axis.text.y = element_text(color = "grey20", size = 9,vjust = 1, face = "plain"))+
  scale_y_continuous(limits=c(0, 2.25))+
  xlab(NULL)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("CMT" = "CONV", "LMT" = "CONV+WCC",
                            "OMT" = "ORG"))+
  scale_fill_manual(values=c("#b5651d", "#619CFF" ), 
                    name="S Sources",
                    breaks=c("compost_s_mean", "fert_s_mean"),
                    labels=c("Compost", "Fertilizer"))

figure1 <- ggarrange(n_plot, p_plot, s_plot,
                     ncol = 3, nrow = 1)
figure1

c_plot

#generate line graph
c_graph<-ggplot(data = CNPSK_cum_2017) +
  geom_line(aes(x = year, y = cumsum(c_avg), group = mgmttype, colour=mgmttype)) +
  ylab("Cumulative kg/ha of C added") + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  scale_x_discrete(labels = CNPSK_cum_2017$year) + 
  xlab("Year")

