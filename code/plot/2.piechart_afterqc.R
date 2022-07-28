rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(readxl)
library(robustHD)
library(RColorBrewer)
library(MuMIn)
library(fmsb)
library(ggplot2)
library(ggpubr)
library(qqman)
library(CMplot)
library(ggsci)
library(scales)

# 1.read csv ####
setwd('result\\')
result1 <- read.csv("1.2.asr&eg_summarize_score.csv", header = TRUE)

# 2.totalprob ####
data_tot <- data.frame(
  group=c("E_sub","E_fam","E_birth","G_fam","G_prs"),
  value=c(result1$analysis1_out_stdCoef_e_sub[1],
          result1$analysis1_out_stdCoef_e_fam[1],
          result1$analysis1_out_stdCoef_e_birth[1],
          result1$analysis1_out_stdCoef_g_fam[1],
          result1$analysis1_out_stdCoef_g_prs[1])
)

p1 = ggplot(data_tot, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  scale_fill_npg()+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("total") +
  theme(plot.title = element_text(size=20,hjust = 0.5))

# 3.internal ####
data_int <- data.frame(
  group=c("E_sub","E_fam","E_birth","G_fam","G_prs"),
  value=c(result1$analysis1_out_stdCoef_e_sub[2],
          result1$analysis1_out_stdCoef_e_fam[2],
          result1$analysis1_out_stdCoef_e_birth[2],
          result1$analysis1_out_stdCoef_g_fam[2],
          result1$analysis1_out_stdCoef_g_prs[2])
)

p2 = ggplot(data_int, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_npg()+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("internal") +
  theme(plot.title = element_text(size=20,hjust = 0.5)) #璁剧疆鏍囬灞呬�?

# 4.external ####
data_ext <- data.frame(
  group=c("E_sub","E_fam","E_birth","G_fam","G_prs"),
  value=c(result1$analysis1_out_stdCoef_e_sub[3],
          result1$analysis1_out_stdCoef_e_fam[3],
          result1$analysis1_out_stdCoef_e_birth[3],
          result1$analysis1_out_stdCoef_g_fam[3],
          result1$analysis1_out_stdCoef_g_prs[3])
)

p3 = ggplot(data_ext, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_npg()+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("external") +
  theme(plot.title = element_text(size=20,hjust = 0.5)) #璁剧疆鏍囬灞呬�?

# 5.attention ####
data_attention <- data.frame(
  group=c("E_sub","E_fam","E_birth","G_fam","G_prs"),
  value=c(result1$analysis1_out_stdCoef_e_sub[4],
          result1$analysis1_out_stdCoef_e_fam[4],
          result1$analysis1_out_stdCoef_e_birth[4],
          result1$analysis1_out_stdCoef_g_fam[4],
          result1$analysis1_out_stdCoef_g_prs[4])
)

p4 = ggplot(data_attention, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_npg()+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("attention") +
  theme(plot.title = element_text(size=20,hjust = 0.5)) #璁剧疆鏍囬灞呬�?

# 6.thought ####
data_thought <- data.frame(
  group=c("E_sub","E_fam","E_birth","G_fam","G_prs"),
  value=c(result1$analysis1_out_stdCoef_e_sub[5],
          result1$analysis1_out_stdCoef_e_fam[5],
          result1$analysis1_out_stdCoef_e_birth[5],
          result1$analysis1_out_stdCoef_g_fam[5],
          result1$analysis1_out_stdCoef_g_prs[5])
)

p5 = ggplot(data_thought, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_npg()+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("thought") +
  theme(plot.title = element_text(size=20,hjust = 0.5)) #璁剧疆鏍囬灞呬�?

# 7.combine subplot ####
ggarrange(p1,p2,p3,p4,p5,ncol=5,nrow=1)
