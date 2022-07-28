rm(list=ls())
gc()
library(pheatmap)
library(data.table)
library(dplyr)
library(readxl)
library(robustHD)
library(RColorBrewer)
library(MuMIn)
library(fmsb)
library(ggplot2)
library(ggpubr)

# 1.plot_prepare -------------------
### read pvalue from csv
name_exposure<-c("ASR_Total","ASR_Internal","ASR_External","ASR_Attention","ASR_Thought")
name_outcome<-c("CBCL_Total","CBCL_Internal","CBCL_External","CBCL_Attention","CBCL_Thought");
### Select confoundings
confoundings <- read_excel("excel\\project2_order_variables.xlsx",2, col_names = TRUE)
confoundings=t(data.frame(confoundings));
name_conf<-rownames(confoundings)
name_e_sub<-confoundings[name_conf[1],][complete.cases(confoundings[name_conf[1],])]
name_e_fam<-confoundings[name_conf[2],][complete.cases(confoundings[name_conf[2],])]
name_e_birth<-confoundings[name_conf[3],][complete.cases(confoundings[name_conf[3],])]
name_g_fam<-confoundings[name_conf[4],][complete.cases(confoundings[name_conf[4],])]
name_g_prs<-confoundings[name_conf[5],][complete.cases(confoundings[name_conf[5],])]
name_confounding<-c(name_e_sub,name_e_fam,name_e_birth,name_g_fam,name_g_prs)

# 2.ASR & factors -----------------
setwd('result\\')
result1 <- read.csv("1.1asr&factor_fdr.csv", header = TRUE)
results_std <- result1$analysis1_out_stdCoef
results_std <- matrix(data = results_std, nrow =5);

rownames(results_std) = name_exposure;
colnames(results_std) = name_confounding;

results_std <- results_std[,-c(45:51,53:59,61:67)]
results_std['ASR_Attention','breast_fed_months']=0
results_std[,'scz_SCORE_avg']=0
results_std[c('ASR_External','ASR_Attention'),'mdd_SCORE_avg']=0

colnames(results_std) <- c("Alcohol","Marijuana","Tobacco","Caffeine","Marital Status","Household_Income",
                           "High_Education","Parental_Monitor","Family_Conflict","Maternal_Age","Paternal_Age",
                           "Planned_Pregnancy","Breast_Fed_Months","Obsteric_Complication","Alcohol_F","Alcohol_M","Alcohol_R",
                           "Drug_Use_F","Drug_Use_M","Drug_Use_R","Depression_F","Depression_M","Depression_R","Mania_F","Mania_M","Mania_R","Vs_F","Vs_M","Vs_R",
                           "Trb_F","Trb_M","Trb_R","Nrv_F","Nrv_M","Nrv_R","Dc_F","Dc_M","Dc_R","Hspd_F","Hspd_M","Hspd_R",
                           "Suicide_F","Suicide_M","Suicide_R","PRS_ADHD_Avg","PRS_SCZ_Avg","PRS_MDD_Avg")

bk1 <- c(seq(-0.140928073507066,-0.0291767497771198,by=0.0001),-0.000001)
bk2 <- c(0.000001,seq(0.030772543,0.345365912,by=0.0001))
bk <- c(bk1,bk2)  #combine the break limits for purpose of graphing

my_palette <- c(colorRampPalette(colors = c("darkblue", "lightblue"))(n = length(bk1)-1),
                "#ffffff", "#ffffff",
                c(colorRampPalette(colors = c("#ffe5e5", "#ff0000"))(n = length(bk2)-1)))
pheatmap(results_std, color = my_palette, breaks = bk, scale = "none", 
         cluster_rows = F, cluster_cols = F, margin = c(5,5),
         angle_col  ='315',cellwidth = 15, cellheight = 12,border_color = TRUE
         ,filename = "asr_eg.pdf")

# 3.ASR & CBCL --------------------
setwd('result\\')
result1 <- read.csv("2.1.asr_cbcl_before_eg_fdr.csv", header = TRUE)
results_std <- result1$out_stdCoef
results_std <- matrix(data = results_std, nrow =5);

rownames(results_std) = name_outcome;
colnames(results_std) = name_exposure;

bk <- c(seq(0.2,0.6,by=0.0001))
my_palette <- (colorRampPalette(colors = c("#ffe5e5", "#ff0000"))(n = length(bk)-1))

pheatmap(results_std, scale = "none",cluster_rows = F,cluster_cols = F,margin = c(5,5),angle_col  ='315',
         cellwidth = 60, cellheight = 48,border_color = TRUE,fontsize = 20,legend_breaks = seq(0.2,0.6,0.05),
         filename = "asr_cbcl_after.pdf")
