rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(MuMIn)
library(fmsb)
library(ggplot2)
library(ggpubr)
library(qqman)
library(CMplot)
library(ggsci)
library(lmerTest)


# 1.data prepare ####
setwd('data\\')
project2_dataset_rm_NA_win_prs<-readRDS("project2_dataset_rm_NA_win_prs.RDS")
project2_dataset_2year_rm_NA_win_prs=project2_dataset_rm_NA_win_prs[project2_dataset_rm_NA_win_prs$eventname == "2_year_follow_up_y_arm_1",]

long_data_mediation_unique<-readRDS('long_data_mediation_unique_noscale.RDS')
vol_level = long_data_mediation_unique$smri_vol_cdk_total_bl
vol_level[long_data_mediation_unique$smri_vol_cdk_total_bl > quantile(long_data_mediation_unique$smri_vol_cdk_total_bl)['25%']] = 1 # "[<50K]"
vol_level[long_data_mediation_unique$smri_vol_cdk_total_bl < quantile(long_data_mediation_unique$smri_vol_cdk_total_bl)['75%']] = 1 # "[<50K]"
vol_level[long_data_mediation_unique$smri_vol_cdk_total_bl > quantile(long_data_mediation_unique$smri_vol_cdk_total_bl)['75%']] = 2 # "[<50K]"
vol_level[long_data_mediation_unique$smri_vol_cdk_total_bl < quantile(long_data_mediation_unique$smri_vol_cdk_total_bl)['25%']] = 0 # "[<50K]"
long_data_mediation_unique$vol_level = factor(vol_level, levels= 0:2, labels = c("low", "mid", "high") )

cbcl_level = long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl
cbcl_level[long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl > quantile(long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl)['25%']] = 1 # "[<50K]"
cbcl_level[long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl < quantile(long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl)['75%']] = 1 # "[<50K]"
cbcl_level[long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl > quantile(long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl)['75%']] = 2 # "[<50K]"
cbcl_level[long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl < quantile(long_data_mediation_unique$cbcl_scr_syn_totprob_r_bl)['25%']] = 0 # "[<50K]"
long_data_mediation_unique$cbcl_level = factor(cbcl_level, levels= 0:2, labels = c("low", "mid", "high") )

data_bl<-long_data_mediation_unique[,c(1:259,414:415)]
data_bl$eventname<-"bl"
data_bl$avg_age<-"9.9"
data_2y<-cbind(data_bl[,c(1:81)],long_data_mediation_unique[,c(261:413)],data_bl[,c(235:261)])
data_2y$eventname<-"fu2"
data_2y$avg_age<-"12.0"
colnames(data_2y)=colnames(data_bl)
com_vol_cbcl<-rbind(data_bl,data_2y)
com_vol_cbcl$eventname<-as.factor(com_vol_cbcl$eventname)
com_vol_cbcl$avg_age<-factor(com_vol_cbcl$avg_age,levels=c("9.9","12.0"))

# 2.standard error plot of cbcl-vol_level ####
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE)/sqrt(dim(x)[1]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(com_vol_cbcl, varname="cbcl_scr_syn_totprob_r_bl", 
                    groupnames=c("mean_age", "vol_level"))
df3 <- data_summary(com_vol_cbcl, varname="asr_scr_totprob_r_bl", 
                    groupnames=c("mean_age", "cbcl_level"))
df4 <- data_summary(com_vol_cbcl, varname="smri_vol_cdk_total_bl", 
                    groupnames=c("mean_age", "cbcl_level"))

p1=ggplot(df2, aes(x=avg_age, y=cbcl_scr_syn_totprob_r_bl, group=vol_level, color=vol_level)) + 
  geom_line(size=1)+
  geom_pointrange(aes(ymin=cbcl_scr_syn_totprob_r_bl-sd, ymax=cbcl_scr_syn_totprob_r_bl+sd))+
  scale_color_jama()+
  theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+
  theme(aspect.ratio=2/1)+
  theme(plot.margin=unit(c(2,2,2,2),'cm'),axis.text=element_text(size = 14,vjust=2),axis.title.x =element_text(size=20,vjust=2,margin = margin(1,1,0,1,'cm')), axis.title.y=element_text(size=20,margin = margin(0,1,0,0,'cm')))+
  labs(title = "", y="CBCL_total", x = "mean_age(year)",fontsize=12)

p2=ggplot(df3, aes(x=avg_age, y=asr_scr_totprob_r_bl, group=cbcl_level, color=cbcl_level)) + 
  geom_line(size=1)+
  geom_pointrange(aes(ymin=asr_scr_totprob_r_bl-sd, ymax=asr_scr_totprob_r_bl+sd))+
  scale_color_jama()+
  theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+
  theme(aspect.ratio=2/1)+
  theme(plot.margin=unit(c(2,2,2,2),'cm'),axis.text=element_text(size = 14,vjust=2),axis.title.x =element_text(size=20,vjust=2,margin = margin(1,1,0,1,'cm')), axis.title.y=element_text(size=20,margin = margin(0,1,0,0,'cm')))+
  labs(title = "", y="ASR_total", x = "mean_age(year)",fontsize=12)

p3=ggplot(df4, aes(x=avg_age, y=smri_vol_cdk_total_bl, group=cbcl_level, color=cbcl_level)) + 
  geom_line(size=1)+
  geom_pointrange(aes(ymin=smri_vol_cdk_total_bl-sd, ymax=smri_vol_cdk_total_bl+sd))+
  scale_color_jama()+
  theme(axis.line = element_line(colour = "black",size = 1))+
  theme_classic(base_size = 15)+
  theme(aspect.ratio=2/1)+
  theme(plot.margin=unit(c(3,1,3,1),'cm'),axis.text=element_text(size = 14,vjust=2),axis.title.x =element_text(size=20,vjust=2,margin = margin(1,1,0,1,'cm')), axis.title.y=element_text(size=20,margin = margin(0,1,0,0,'cm')))+
  labs(title = "", y="Cortical volume(mm^3)", x = "mean_age(year)",fontsize=12)

ggarrange(p1,p2,p3,ncol=3,nrow=1,common.legend = T)
