rm(list=ls())
gc()
library(parallel)
library(foreach)
library(doParallel)
library(lavaan)
library(plyr)
library(nnet)
library(dplyr)
library(readxl)

detectCores()
numCores <- 12
registerDoParallel(numCores)

### data prepare
setwd('data\\')
long_data_mediation_unique<-readRDS('long_data_mediation_unique.RDS')
### Select variables
im_outcomes <- read_excel("excel\\project2_order_variables.xlsx",4, col_names = TRUE)
im_outcomes=t(data.frame(im_outcomes));
name_imout<-rownames(im_outcomes)
name_lh_vol<-im_outcomes[name_imout[1],][complete.cases(im_outcomes[name_imout[1],])]
name_lh_area<-im_outcomes[name_imout[2],][complete.cases(im_outcomes[name_imout[2],])]
name_rh_vol<-im_outcomes[name_imout[4],][complete.cases(im_outcomes[name_imout[4],])]
name_rh_area<-im_outcomes[name_imout[5],][complete.cases(im_outcomes[name_imout[5],])]
total_mean<-c("smri_vol_cdk_total","smri_area_cdk_total","smri_vol_scs_intracranialv")
name_imoutcome<-c(total_mean,name_lh_vol,name_lh_area,name_rh_vol,name_rh_area)

# 1.before control e/g factors -------------
### 1.1.definition of functions ####
LongMed.fun <- function (assList, LongMediation, data.clpm, outfile.name){
  colnames.ori <- colnames(data.clpm)

  results_analysis <- foreach (number = c(1:nrow(assList)), .combine=rbind, .packages = "lavaan") %dopar% {
    colnames(data.clpm) <- colnames.ori
    X_set<-colnames(data.clpm)[assList$x1.index[number]]; 
    M_set<-colnames(data.clpm)[assList$m1.index[number]]; 
    Y_set<-colnames(data.clpm)[assList$y1.index[number]];
    
    colnames(data.clpm)[c(assList$x1.index[number],assList$y1.index[number],assList$m1.index[number],assList$x2.index[number],assList$y2.index[number],assList$m2.index[number])] <- c("X1","Y1","M1","X2","Y2","M2")
    fit.Model <- sem(LongMediation, data = data.clpm,estimator='GLS')
    res<-summary(fit.Model,standardized = TRUE,fit.measures = TRUE)
    
    ab_est<-res$PE[which(res$PE$label=="ab"),'est']
    ab_se<-res$PE[which(res$PE$label=="ab"),'se']
    ab_z<-res$PE[which(res$PE$label=="ab"),'z']
    ab_pvalue<-res$PE[which(res$PE$label=="ab"),'pvalue']
    ab_std.lv<-res$PE[which(res$PE$label=="ab"),'std.lv']
    ab_std.all<-res$PE[which(res$PE$label=="ab"),'std.all']
    ab_std.nox<-res$PE[which(res$PE$label=="ab"),'std.nox']
    ab_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="ab"),]$ci.lower
    ab_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="ab"),]$ci.upper
    
    a_est<-res$PE[which(res$PE$label=="a"),'est']
    a_se<-res$PE[which(res$PE$label=="a"),'se']
    a_z<-res$PE[which(res$PE$label=="a"),'z']
    a_pvalue<-res$PE[which(res$PE$label=="a"),'pvalue']
    a_std.lv<-res$PE[which(res$PE$label=="a"),'std.lv']
    a_std.all<-res$PE[which(res$PE$label=="a"),'std.all']
    a_std.nox<-res$PE[which(res$PE$label=="a"),'std.nox']
    a_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="a"),]$ci.lower
    a_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="a"),]$ci.upper
    
    b_est<-res$PE[which(res$PE$label=="b"),'est']
    b_se<-res$PE[which(res$PE$label=="b"),'se']
    b_z<-res$PE[which(res$PE$label=="b"),'z']
    b_pvalue<-res$PE[which(res$PE$label=="b"),'pvalue']
    b_std.lv<-res$PE[which(res$PE$label=="b"),'std.lv']
    b_std.all<-res$PE[which(res$PE$label=="b"),'std.all']
    b_std.nox<-res$PE[which(res$PE$label=="b"),'std.nox']
    b_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="b"),]$ci.lower
    b_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="b"),]$ci.upper
    
    cfi<-fitMeasures(fit.Model)["cfi"]#YESSSSSSSS!
    tli<-fitMeasures(fit.Model)["tli"]#YESSSSSSSS!
    rmsea<-fitMeasures(fit.Model)["rmsea"]
    aic<-fitMeasures(fit.Model)["aic"]
    bic<-fitMeasures(fit.Model)["bic"]
    
    results_long_mediation <- data.frame(X_set,Y_set,M_set,
                                        ab_est,ab_se,ab_z,ab_pvalue,ab_std.lv,ab_std.all,ab_std.nox,ab_cilow,ab_ciup,
                                        a_est,a_se,a_z,a_pvalue,a_std.lv,a_std.all,a_std.nox,a_cilow,a_ciup,
                                        b_est,b_se,b_z,b_pvalue,b_std.lv,b_std.all,b_std.nox,b_cilow,b_ciup,
                                        #c_est,c_se,c_z,c_pvalue,c_std.lv,c_std.all,c_std.nox,c_cilow,c_ciup,
                                        #tot_est,tot_se,tot_z,tot_pvalue,tot_std.lv,tot_std.all,tot_std.nox,tot_cilow,tot_ciup,
                                        cfi,tli,rmsea,aic,bic)
  }
  write.csv(results_analysis,outfile.name);
  cat("results are exported in a csv file and ready for download\n")
}


### 1.2.Model A : parent-driven Single Mediation ####
LongMediation_PD <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 +femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2+ smri_vol_scs_intracranialv_bl+femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2+ smri_vol_scs_intracranialv_bl+femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'
asr_bl<-rep(c(which(colnames(long_data_mediation_unique)=="asr_scr_totprob_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_internal_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_external_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_thought_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_attention_r_bl")));
asr_2y<-rep(c(which(colnames(long_data_mediation_unique)=="asr_scr_totprob_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_internal_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_external_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_thought_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_attention_r_2y")));
cbcl_bl<-rep(c(which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_totprob_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_internal_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_external_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_thought_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_attention_r_bl")));
cbcl_2y<-rep(c(which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_totprob_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_internal_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_external_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_thought_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_attention_r_2y")));
smri_bl<-rep()
smri_2y<-rep()
for(col in name_imoutcome[c(grep("smri_vol_cdk_",name_imoutcome),grep("smri_area_cdk_",name_imoutcome))]){
  smri_bl<-c(smri_bl,which(colnames(long_data_mediation_unique)==paste(col,"_bl",sep = "")))
  smri_2y<-c(smri_2y,which(colnames(long_data_mediation_unique)==paste(col,"_2y",sep = "")))
}

### define the list for the multilevel assocaition
y1.indexs <- smri_bl
x1.indexs <- asr_bl
m1.indexs <- cbcl_bl
y2.indexs <- smri_2y
x2.indexs <- asr_2y
m2.indexs <- cbcl_2y

x1.index <- rep()
x2.index <- rep()
y1.index <- rep()
y2.index <- rep()
m1.index <- rep()
m2.index <- rep()
n <- 1
for (i in 1:length(x1.indexs)){
  for(j in 1:length(y1.indexs)){
    x1.index[n] <- x1.indexs[i]
    y1.index[n] <- y1.indexs[j]
    m1.index[n] <- m1.indexs[i]
    x2.index[n] <- x2.indexs[i]
    y2.index[n] <- y2.indexs[j]
    m2.index[n] <- m2.indexs[i]
    n <- n + 1
  }
}

### sig result in model A (singleMed)
sig_res <- read.csv("4.1.5.pd_med_vol_survive_before.csv")
x1.index<-rep();y1.index<-rep();m1.index<-rep();x2.index<-rep();y2.index<-rep();m2.index<-rep()
for(i in 1:dim(sig_res)[1]){
  x1.index <- c(x1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_bl",sep = "")))
  y1.index <- c(y1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_bl",sep = "")))
  m1.index <- c(m1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_bl",sep = "")))
  x2.index <- c(x2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_2y",sep = "")))
  y2.index <- c(y2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_2y",sep = "")))
  m2.index <- c(m2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_2y",sep = "")))
}
outfile.name <- 'result\\long_mediation_before_eg_parent_driven_vol.csv'
###############!!!!! take care !!!!!!!!!!
assList.LongMed.A <- data.frame(x1.index, y1.index, m1.index, x2.index, y2.index, m2.index)
results_analysis <- LongMed.fun(assList.LongMed.A, LongMediation_PD, long_data_mediation_unique, outfile.name)

result1=read.csv("result\\long_mediation_before_eg_parent_driven_vol.csv", header = TRUE)
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
result1<-data.frame(results_p_fdr1,result1)
write.csv(result1,"result\\long_mediation_before_eg_parent_driven_vol_fdr.csv");

### 1.3.Model B : child-driven Longitudinal Mediation ####
LongMediation_CD <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 +smri_vol_scs_intracranialv_bl+femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2+ smri_vol_scs_intracranialv_bl+femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2+ smri_vol_scs_intracranialv_bl+femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'
asr_bl<-rep(c(which(colnames(long_data_mediation_unique)=="asr_scr_totprob_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_internal_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_external_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_thought_r_bl"),which(colnames(long_data_mediation_unique)=="asr_scr_attention_r_bl")));
asr_2y<-rep(c(which(colnames(long_data_mediation_unique)=="asr_scr_totprob_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_internal_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_external_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_thought_r_2y"),which(colnames(long_data_mediation_unique)=="asr_scr_attention_r_2y")));
cbcl_bl<-rep(c(which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_totprob_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_internal_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_external_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_thought_r_bl"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_attention_r_bl")));
cbcl_2y<-rep(c(which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_totprob_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_internal_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_external_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_thought_r_2y"),which(colnames(long_data_mediation_unique)=="cbcl_scr_syn_attention_r_2y")));
smri_bl<-rep()
smri_2y<-rep()
for(col in name_imoutcome[c(grep("smri_vol_cdk_",name_imoutcome),grep("smri_area_cdk_",name_imoutcome))]){
  smri_bl<-c(smri_bl,which(colnames(long_data_mediation_unique)==paste(col,"_bl",sep = "")))
  smri_2y<-c(smri_2y,which(colnames(long_data_mediation_unique)==paste(col,"_2y",sep = "")))
}

### define the list for the multilevel assocaition
x1.indexs <- smri_bl
y1.indexs <- asr_bl
m1.indexs <- cbcl_bl
x2.indexs <- smri_2y
y2.indexs <- asr_2y
m2.indexs <- cbcl_2y

x1.index <- rep()
x2.index <- rep()
y1.index <- rep()
y2.index <- rep()
m1.index <- rep()
m2.index <- rep()
n <- 1
for (i in 1:length(x1.indexs)){
  for(j in 1:length(m1.indexs)){
    x1.index[n] <- x1.indexs[i]
    y1.index[n] <- y1.indexs[j]
    m1.index[n] <- m1.indexs[j]
    x2.index[n] <- x2.indexs[i]
    y2.index[n] <- y2.indexs[j]
    m2.index[n] <- m2.indexs[j]
    n <- n + 1
  }
}

### sig result in model B (singleMed)
sig_res <- read.csv("4.2.5.cd_med_vol_survive_before.csv")
x1.index<-rep();y1.index<-rep();m1.index<-rep();x2.index<-rep();y2.index<-rep();m2.index<-rep()
for(i in 1:dim(sig_res)[1]){
  x1.index <- c(x1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_bl",sep = "")))
  y1.index <- c(y1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_bl",sep = "")))
  m1.index <- c(m1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_bl",sep = "")))
  x2.index <- c(x2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_2y",sep = "")))
  y2.index <- c(y2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_2y",sep = "")))
  m2.index <- c(m2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_2y",sep = "")))
}
outfile.name <- 'long_mediation_before_eg_child_driven_vol.csv'
###############!!!!! take care !!!!!!!!!!
assList.LongMed.B <- data.frame(x1.index, y1.index, m1.index, x2.index, y2.index, m2.index)
results_analysis <- LongMed.fun(assList.LongMed.B, LongMediation_CD, long_data_mediation_unique, outfile.name)

result1=read.csv("result\\long_mediation_before_eg_child_driven_vol.csv", header = TRUE)
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
result1<-data.frame(results_p_fdr1,result1)
write.csv(result1,"result\\long_mediation_before_eg_child_driven_vol_fdr.csv");


# 2.after control e/g factors --------------
### 2.1.definition of functions ####
LongMed.fun <- function (assList, LongMediation, data.clpm, outfile.name){
  colnames.ori <- colnames(data.clpm)
  results_analysis <- foreach (number = c(1:nrow(assList)), .combine=rbind, .packages = "lavaan") %dopar% {
    colnames(data.clpm) <- colnames.ori
    X_set<-colnames(data.clpm)[assList$x1.index[number]]; 
    M_set<-colnames(data.clpm)[assList$m1.index[number]]; 
    Y_set<-colnames(data.clpm)[assList$y1.index[number]];
    
    colnames(data.clpm)[c(assList$x1.index[number],assList$y1.index[number],assList$m1.index[number],assList$x2.index[number],assList$y2.index[number],assList$m2.index[number])] <- c("X1","Y1","M1","X2","Y2","M2")
    fit.Model <- sem(LongMediation, data = data.clpm, estimator='GLS')
    
    res<-summary(fit.Model,standardized = TRUE,fit.measures = TRUE)
    
    ab_est<-res$PE[which(res$PE$label=="ab"),'est']
    ab_se<-res$PE[which(res$PE$label=="ab"),'se']
    ab_z<-res$PE[which(res$PE$label=="ab"),'z']
    ab_pvalue<-res$PE[which(res$PE$label=="ab"),'pvalue']
    ab_std.lv<-res$PE[which(res$PE$label=="ab"),'std.lv']
    ab_std.all<-res$PE[which(res$PE$label=="ab"),'std.all']
    ab_std.nox<-res$PE[which(res$PE$label=="ab"),'std.nox']
    ab_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="ab"),]$ci.lower
    ab_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="ab"),]$ci.upper
    
    a_est<-res$PE[which(res$PE$label=="a"),'est']
    a_se<-res$PE[which(res$PE$label=="a"),'se']
    a_z<-res$PE[which(res$PE$label=="a"),'z']
    a_pvalue<-res$PE[which(res$PE$label=="a"),'pvalue']
    a_std.lv<-res$PE[which(res$PE$label=="a"),'std.lv']
    a_std.all<-res$PE[which(res$PE$label=="a"),'std.all']
    a_std.nox<-res$PE[which(res$PE$label=="a"),'std.nox']
    a_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="a"),]$ci.lower
    a_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="a"),]$ci.upper
    
    b_est<-res$PE[which(res$PE$label=="b"),'est']
    b_se<-res$PE[which(res$PE$label=="b"),'se']
    b_z<-res$PE[which(res$PE$label=="b"),'z']
    b_pvalue<-res$PE[which(res$PE$label=="b"),'pvalue']
    b_std.lv<-res$PE[which(res$PE$label=="b"),'std.lv']
    b_std.all<-res$PE[which(res$PE$label=="b"),'std.all']
    b_std.nox<-res$PE[which(res$PE$label=="b"),'std.nox']
    b_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="b"),]$ci.lower
    b_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="b"),]$ci.upper
    
    cfi<-fitMeasures(fit.Model)["cfi"]#YESSSSSSSS!
    tli<-fitMeasures(fit.Model)["tli"]#YESSSSSSSS!
    rmsea<-fitMeasures(fit.Model)["rmsea"]
    aic<-fitMeasures(fit.Model)["aic"]
    bic<-fitMeasures(fit.Model)["bic"]
    
    results_long_mediation <- data.frame(X_set,Y_set,M_set,
                                         ab_est,ab_se,ab_z,ab_pvalue,ab_std.lv,ab_std.all,ab_std.nox,ab_cilow,ab_ciup,
                                         a_est,a_se,a_z,a_pvalue,a_std.lv,a_std.all,a_std.nox,a_cilow,a_ciup,
                                         b_est,b_se,b_z,b_pvalue,b_std.lv,b_std.all,b_std.nox,b_cilow,b_ciup,
                                         #c_est,c_se,c_z,c_pvalue,c_std.lv,c_std.all,c_std.nox,c_cilow,c_ciup,
                                         #tot_est,tot_se,tot_z,tot_pvalue,tot_std.lv,tot_std.all,tot_std.nox,tot_cilow,tot_ciup,
                                         cfi,tli,rmsea,aic,bic)
  }
  write.csv(results_analysis,outfile.name);
  cat("results are exported in a csv file and ready for download\n")
}

### 2.2.MODEL ####
LongMediation_eg_total <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2 + smri_vol_scs_intracranialv_bl + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2 + smri_vol_scs_intracranialv_bl + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

#
# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'

LongMediation_eg_inter <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 +e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2 + smri_vol_scs_intracranialv_bl + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2 + smri_vol_scs_intracranialv_bl + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

#
# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'
LongMediation_eg_exter <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2 + smri_vol_scs_intracranialv_bl + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2 + smri_vol_scs_intracranialv_bl + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

#
# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'

LongMediation_eg_thoug <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2 + c + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2 + smri_vol_scs_intracranialv_bl + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

#
# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'

LongMediation_eg_atten <- 
  '
#regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
#X2 ~ smri_vol_scs_intracranialv_bl
X2 ~ xx*X1+mx*M1 + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
M2 ~ a*X1+mm*M1+ym*Y1+x2m*X2 + smri_vol_scs_intracranialv_bl + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
Y2 ~ xy*X1+b*M1+yy*Y1+x2y*X2 + smri_vol_scs_intracranialv_bl + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

# X1 ~ e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# M1 ~ e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4
# Y1 ~ e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes+interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 +race1+race2+race3+race4

#
# X1 ~~ M1
# X1 ~~ Y1
# M1 ~~ Y1
M2 ~~ Y2

#indirect effect(a*b)
ab :=a*b
'

### sig result in model A (LongMed)
sig_res <- read.csv("result\\sig_vol_before.csv")
x1.index<-rep();y1.index<-rep();m1.index<-rep();x2.index<-rep();y2.index<-rep();m2.index<-rep()
for(i in 1:dim(sig_res)[1]){
  x1.index <- c(x1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_bl",sep = "")))
  y1.index <- c(y1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_bl",sep = "")))
  m1.index <- c(m1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_bl",sep = "")))
  x2.index <- c(x2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_2y",sep = "")))
  y2.index <- c(y2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_2y",sep = "")))
  m2.index <- c(m2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_2y",sep = "")))
}
outfile.name <- 'result\\sig_vol_after.csv'
assList.LongMed.A <- data.frame(x1.index, y1.index, m1.index, x2.index, y2.index, m2.index)
LongMediation_eg<-c(LongMediation_eg_total,LongMediation_eg_inter,LongMediation_eg_exter,LongMediation_eg_thoug,LongMediation_eg_atten)
results_analysis <- LongMed.fun(assList.LongMed.A[1,], LongMediation_eg[1], long_data_mediation_unique, outfile.name)

### sig result in model B (LongMed)
sig_res <- read.csv("result\\sig_vol_before.csv")
x1.index<-rep();y1.index<-rep();m1.index<-rep();x2.index<-rep();y2.index<-rep();m2.index<-rep()
for(i in 1:dim(sig_res)[1]){
  x1.index <- c(x1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_bl",sep = "")))
  y1.index <- c(y1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_bl",sep = "")))
  m1.index <- c(m1.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_bl",sep = "")))
  x2.index <- c(x2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$X_set[i],"_2y",sep = "")))
  y2.index <- c(y2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$Y_set[i],"_2y",sep = "")))
  m2.index <- c(m2.index,which(colnames(long_data_mediation_unique)==paste(sig_res$M_set[i],"_2y",sep = "")))
}
outfile.name <- 'result\\sig_vol_after.csv'
assList.LongMed.B <- data.frame(x1.index, y1.index, m1.index, x2.index, y2.index, m2.index)
LongMediation_eg<-c(LongMediation_eg_total,LongMediation_eg_inter,LongMediation_eg_exter,LongMediation_eg_thoug,LongMediation_eg_atten)
results_analysis <- LongMed.fun(assList.LongMed.B[c(2,4,5),], LongMediation_eg[2], long_data_mediation_unique, outfile.name)

stopImplicitCluster()


# 3.fdr ####
### 3.1.fdr for long mediation(model A)####
library(openxlsx)
setwd('result\\')
result1 <- read.csv("long_mediation_before_eg_parent_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_vol_cdk_",result1$Y_set))],]
result11 <- result1[result1$Y_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_parent_driven_area_fdr.csv", header = TRUE)
result1 <- result1[result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result12 <- result1[result1$Y_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res1<-data.frame(results_p_fdr1,rbind(result11,result12))

result1 <- read.csv("long_mediation_before_eg_parent_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[!result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_vol_cdk_",result1$Y_set))],]
result11 <- result1[result1$Y_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_parent_driven_area_fdr.csv", header = TRUE)
result1 <- result1[!result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result12 <- result1[result1$Y_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res2<-data.frame(results_p_fdr1,rbind(result11,result12))

result1 <- read.csv("long_mediation_before_eg_parent_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_vol_cdk_",result1$Y_set))],]
result11 <- result1[!result1$Y_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_parent_driven_area_fdr.csv", header = TRUE)
result1 <- result1[result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result12 <- result1[!result1$Y_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res3<-data.frame(results_p_fdr1,rbind(result11,result12))

result1 <- read.csv("long_mediation_before_eg_parent_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[!result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_vol_cdk_",result1$Y_set))],]
result11 <- result1[!result1$Y_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_parent_driven_area_fdr.csv", header = TRUE)
result1 <- result1[!result1$X_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result12 <- result1[!result1$Y_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res4<-data.frame(results_p_fdr1,rbind(result11,result12))

sheets <- list("tot_totprob"=res1,"tot_4prob"=res2,"reg_totprob"=res3,"reg_4prob"=res4)
write.xlsx(sheets,"result\\5.1.1.LongMed_parent_driven_before.xlsx")

### 3.2.fdr for long mediation(model B)####
setwd('result\\')
result1 <- read.csv("long_mediation_before_eg_child_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_vol_cdk_",result1$X_set))],]
result11 <- result1[result1$X_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_child_driven_area_fdr.csv", header = TRUE)
result1 <- result1[result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result12 <- result1[result1$X_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res1<-data.frame(results_p_fdr1,rbind(result11,result12))

result1 <- read.csv("long_mediation_before_eg_child_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[!result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_vol_cdk_",result1$X_set))],]
result11 <- result1[result1$X_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_child_driven_area_fdr.csv", header = TRUE)
result1 <- result1[!result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result12 <- result1[result1$X_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res2<-data.frame(results_p_fdr1,rbind(result11,result12))

result1 <- read.csv("long_mediation_before_eg_child_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_vol_cdk_",result1$X_set))],]
result11 <- result1[!result1$X_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_child_driven_area_fdr.csv", header = TRUE)
result1 <- result1[result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result12 <- result1[!result1$X_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res3<-data.frame(results_p_fdr1,rbind(result11,result12))

result1 <- read.csv("long_mediation_before_eg_child_driven_vol_fdr.csv", header = TRUE)
result1 <- result1[!result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_vol_cdk_",result1$X_set))],]
result11 <- result1[!result1$X_set=="smri_vol_cdk_total_bl",]
result1 <- read.csv("long_mediation_before_eg_child_driven_area_fdr.csv", header = TRUE)
result1 <- result1[!result1$Y_set=="asr_scr_totprob_r_bl",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result12 <- result1[!result1$X_set=="smri_area_cdk_total_bl",]
results_p_fdr1 <- p.adjust(c(result11$ab_pvalue,result12$ab_pvalue),method = "fdr")
res4<-data.frame(results_p_fdr1,rbind(result11,result12))

sheets <- list("tot_totprob"=res1,"tot_4prob"=res2,"reg_totprob"=res3,"reg_4prob"=res4)
write.xlsx(sheets,"result\\5.2.1.LongMed_child_driven_before.xlsx")
