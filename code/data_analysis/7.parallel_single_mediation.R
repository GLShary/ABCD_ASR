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
project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique<-readRDS("project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique.RDS")
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

# 1.before control e/g factors ---------------------
### 1.1.definition of functions ####
SingleMed.fun <- function (assList, SingleMediation, data.clpm, outfile.name){
  colnames.ori <- colnames(data.clpm)
  
  results_analysis <- foreach (number = c(1:nrow(assList)), .combine=rbind, .packages = "lavaan") %dopar% {
    colnames(data.clpm) <- colnames.ori
    X_set<-colnames(data.clpm)[assList$x1.index[number]]; 
    M_set<-colnames(data.clpm)[assList$m1.index[number]]; 
    Y_set<-colnames(data.clpm)[assList$y1.index[number]];
    
    colnames(data.clpm)[c(assList$x1.index[number],assList$y1.index[number],assList$m1.index[number])] <- c("X","Y","M")
    #fit.Model <- sem(SingleMediation, data = data.clpm)
    fit.Model <- sem(SingleMediation, data = data.clpm,
                     #missing = 'ML', #for the missing data!
                     int.ov.free = F,
                     int.lv.free = F,
                     auto.fix.first = F,
                     auto.fix.single = F,
                     auto.cov.lv.x = T,
                     auto.cov.y = T,
                     auto.var = T)
    
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
    
    c_est<-res$PE[which(res$PE$label=="c"),'est']
    c_se<-res$PE[which(res$PE$label=="c"),'se']
    c_z<-res$PE[which(res$PE$label=="c"),'z']
    c_pvalue<-res$PE[which(res$PE$label=="c"),'pvalue']
    c_std.lv<-res$PE[which(res$PE$label=="c"),'std.lv']
    c_std.all<-res$PE[which(res$PE$label=="c"),'std.all']
    c_std.nox<-res$PE[which(res$PE$label=="c"),'std.nox']
    c_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="c"),]$ci.lower
    c_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="c"),]$ci.upper
    
    tot_est<-res$PE[which(res$PE$label=="total"),'est']
    tot_se<-res$PE[which(res$PE$label=="total"),'se']
    tot_z<-res$PE[which(res$PE$label=="total"),'z']
    tot_pvalue<-res$PE[which(res$PE$label=="total"),'pvalue']
    tot_std.lv<-res$PE[which(res$PE$label=="total"),'std.lv']
    tot_std.all<-res$PE[which(res$PE$label=="total"),'std.all']
    tot_std.nox<-res$PE[which(res$PE$label=="total"),'std.nox']
    tot_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="total"),]$ci.lower
    tot_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="total"),]$ci.upper
    
    cfi<-fitMeasures(fit.Model)["cfi"]#YESSSSSSSS!
    tli<-fitMeasures(fit.Model)["tli"]#YESSSSSSSS!
    rmsea<-fitMeasures(fit.Model)["rmsea"]
    aic<-fitMeasures(fit.Model)["aic"]
    bic<-fitMeasures(fit.Model)["bic"]
    
    results_single_mediation <- data.frame(X_set,Y_set,M_set,
                                           ab_est,ab_se,ab_z,ab_pvalue,ab_std.lv,ab_std.all,ab_std.nox,ab_cilow,ab_ciup,
                                           a_est,a_se,a_z,a_pvalue,a_std.lv,a_std.all,a_std.nox,a_cilow,a_ciup,
                                           b_est,b_se,b_z,b_pvalue,b_std.lv,b_std.all,b_std.nox,b_cilow,b_ciup,
                                           c_est,c_se,c_z,c_pvalue,c_std.lv,c_std.all,c_std.nox,c_cilow,c_ciup,
                                           tot_est,tot_se,tot_z,tot_pvalue,tot_std.lv,tot_std.all,tot_std.nox,tot_cilow,tot_ciup,
                                           cfi,tli,rmsea,aic,bic)
  }
  write.csv(results_analysis,outfile.name);
  cat("results are exported in a csv file and ready for download\n")
}


### 1.2.Model A : parent-driven Single Mediation ####
SingleMediation <- '
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                      #X ~ smri_vol_scs_intracranialv + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       M ~ a*X + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ c*X + b*M + smri_vol_scs_intracranialv + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
asr_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_attention_r")));
cbcl_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_attention_r")));
smri_bl<-rep()
for(col in name_imoutcome[c(grep("smri_vol_cdk_",name_imoutcome),grep("smri_area_cdk_",name_imoutcome))]){
  smri_bl<-c(smri_bl,which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)==col))
}

### define the list for the multilevel assocaition
y1.indexs <- smri_bl
x1.indexs <- asr_bl
m1.indexs <- cbcl_bl

x1.index <- rep()
y1.index <- rep()
m1.index <- rep()
n <- 1
for (i in 1:length(x1.indexs)){
  for(j in 1:length(y1.indexs)){
    x1.index[n] <- x1.indexs[i]
    y1.index[n] <- y1.indexs[j]
    m1.index[n] <- m1.indexs[i]
    n <- n + 1
  }
}

outfile.name <- 'result\\single_mediation_before_eg_parent_driven.csv'
###############!!!!! take care !!!!!!!!!!
assList.SingleMed.A <- data.frame(x1.index, y1.index, m1.index)
results_analysis <- SingleMed.fun(assList.SingleMed.A, SingleMediation, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique, outfile.name)

### 1.3.Model B : child-driven Single Mediation ####
SingleMediation <- '
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                      #X ~ smri_vol_scs_intracranialv + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       M ~ a*X + smri_vol_scs_intracranialv + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ c*X + b*M + smri_vol_scs_intracranialv + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
asr_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_attention_r")));
cbcl_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_attention_r")));
smri_bl<-rep()
for(col in name_imoutcome[c(grep("smri_vol_cdk_",name_imoutcome),grep("smri_area_cdk_",name_imoutcome))]){
  smri_bl<-c(smri_bl,which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)==col))
}

### define the list for the multilevel assocaition
y1.indexs <- smri_bl
x1.indexs <- asr_bl
m1.indexs <- cbcl_bl

x1.index <- rep()
y1.index <- rep()
m1.index <- rep()
n <- 1
for (i in 1:length(x1.indexs)){
  for(j in 1:length(m1.indexs)){
    x1.index[n] <- x1.indexs[i]
    y1.index[n] <- y1.indexs[j]
    m1.index[n] <- m1.indexs[j]
    n <- n + 1
  }
}
outfile.name <- 'result\\single_mediation_before_eg_child_driven.csv'
###############!!!!! take care !!!!!!!!!!
assList.SingleMed.B <- data.frame(x1.index, y1.index, m1.index)
results_analysis <- SingleMed.fun(assList.SingleMed.B, SingleMediation, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique, outfile.name)


# 2.after control e/g factors ----------------------
### 2.1.definition of functions ####
SingleMed.fun <- function (assList, SingleMediation, data.clpm, outfile.name){
  colnames.ori <- colnames(data.clpm)
  
  results_analysis <- foreach (number = c(1:nrow(assList)), .combine=rbind, .packages = "lavaan") %dopar% {
    colnames(data.clpm) <- colnames.ori
    X_set<-colnames(data.clpm)[assList$x1.index[number]]; 
    M_set<-colnames(data.clpm)[assList$m1.index[number]]; 
    Y_set<-colnames(data.clpm)[assList$y1.index[number]];
    
    colnames(data.clpm)[c(assList$x1.index[number],assList$y1.index[number],assList$m1.index[number])] <- c("X","Y","M")
    #fit.Model <- sem(SingleMediation, data = data.clpm)
    fit.Model <- sem(SingleMediation[floor(number/143+1)], data = data.clpm,
                     #missing = 'ML', #for the missing data!
                     int.ov.free = F,
                     int.lv.free = F,
                     auto.fix.first = F,
                     auto.fix.single = F,
                     auto.cov.lv.x = T,
                     auto.cov.y = T,
                     auto.var = T)
    
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
    
    c_est<-res$PE[which(res$PE$label=="c"),'est']
    c_se<-res$PE[which(res$PE$label=="c"),'se']
    c_z<-res$PE[which(res$PE$label=="c"),'z']
    c_pvalue<-res$PE[which(res$PE$label=="c"),'pvalue']
    c_std.lv<-res$PE[which(res$PE$label=="c"),'std.lv']
    c_std.all<-res$PE[which(res$PE$label=="c"),'std.all']
    c_std.nox<-res$PE[which(res$PE$label=="c"),'std.nox']
    c_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="c"),]$ci.lower
    c_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="c"),]$ci.upper
    
    tot_est<-res$PE[which(res$PE$label=="total"),'est']
    tot_se<-res$PE[which(res$PE$label=="total"),'se']
    tot_z<-res$PE[which(res$PE$label=="total"),'z']
    tot_pvalue<-res$PE[which(res$PE$label=="total"),'pvalue']
    tot_std.lv<-res$PE[which(res$PE$label=="total"),'std.lv']
    tot_std.all<-res$PE[which(res$PE$label=="total"),'std.all']
    tot_std.nox<-res$PE[which(res$PE$label=="total"),'std.nox']
    tot_cilow<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="total"),]$ci.lower
    tot_ciup<-parameterEstimates(fit.Model)[which(parameterEstimates(fit.Model)$label=="total"),]$ci.upper
    
    cfi<-fitMeasures(fit.Model)["cfi"]#YESSSSSSSS!
    tli<-fitMeasures(fit.Model)["tli"]#YESSSSSSSS!
    rmsea<-fitMeasures(fit.Model)["rmsea"]
    aic<-fitMeasures(fit.Model)["aic"]
    bic<-fitMeasures(fit.Model)["bic"]
    
    results_single_mediation <- data.frame(X_set,Y_set,M_set,
                                           ab_est,ab_se,ab_z,ab_pvalue,ab_std.lv,ab_std.all,ab_std.nox,ab_cilow,ab_ciup,
                                           a_est,a_se,a_z,a_pvalue,a_std.lv,a_std.all,a_std.nox,a_cilow,a_ciup,
                                           b_est,b_se,b_z,b_pvalue,b_std.lv,b_std.all,b_std.nox,b_cilow,b_ciup,
                                           c_est,c_se,c_z,c_pvalue,c_std.lv,c_std.all,c_std.nox,c_cilow,c_ciup,
                                           tot_est,tot_se,tot_z,tot_pvalue,tot_std.lv,tot_std.all,tot_std.nox,tot_cilow,tot_ciup,
                                           cfi,tli,rmsea,aic,bic)
  }
  write.csv(results_analysis,outfile.name);
  cat("results are exported in a csv file and ready for download\n")
}

### 2.2.Model A : parent-driven Single Mediation ####
SingleMediation_eg_total <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_inter <- '# direct effect
                      # regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                      # mediator
                       M ~ a*X + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_exter <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_thoug <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_atten <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '

asr_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_attention_r")));
cbcl_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_attention_r")));
smri_bl<-rep()
for(col in name_imoutcome[c(grep("smri_vol_cdk_",name_imoutcome),grep("smri_area_cdk_",name_imoutcome))]){
  smri_bl<-c(smri_bl,which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)==col))
}

y1.indexs <- smri_bl
x1.indexs <- asr_bl
m1.indexs <- cbcl_bl

### define the list for the multilevel assocaition
x1.index <- rep()
y1.index <- rep()
m1.index <- rep()
n <- 1
for (i in 1:length(x1.indexs)){
  for(j in 1:length(y1.indexs)){
    x1.index[n] <- x1.indexs[i]
    y1.index[n] <- y1.indexs[j]
    m1.index[n] <- m1.indexs[i]
    n <- n + 1
  }
}

outfile.name <- 'result\\single_mediation_after_eg_child_driven.csv'
assList.SingleMed.A <- data.frame(x1.index, y1.index, m1.index)
SingleMediation_eg<-c(SingleMediation_eg_total,SingleMediation_eg_inter,SingleMediation_eg_exter,SingleMediation_eg_thoug,SingleMediation_eg_atten)
results_clpm <- SingleMed.fun(assList.SingleMed.A,SingleMediation_eg, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique, outfile.name)

### 2.3.Model B : child-driven Single Mediation ####
SingleMediation_eg_total <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + smri_vol_scs_intracranialv + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_inter <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       #X ~ g_prs_inter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       M ~ a*X + smri_vol_scs_intracranialv + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_exter <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + smri_vol_scs_intracranialv + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_thoug <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + smri_vol_scs_intracranialv + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '
SingleMediation_eg_atten <- '# direct effect
                      #regressions(take care!!!!!!!!!!!!!!!  icv !!!!!!!!!!!!!!!!)
                     # mediator
                       M ~ a*X + smri_vol_scs_intracranialv + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                       Y ~ b*M + c*X + smri_vol_scs_intracranialv + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes + interview_age + s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
                     # indirect effect(a*b)
                       ab :=a*b
                     # total effect
                       total := c + (a*b)
                       '

asr_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="asr_scr_attention_r")));
cbcl_bl<-rep(c(which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_totprob_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_internal_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_external_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_thought_r"),which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)=="cbcl_scr_syn_attention_r")));
smri_bl<-rep()
for(col in name_imoutcome[c(grep("smri_vol_cdk_",name_imoutcome),grep("smri_area_cdk_",name_imoutcome))]){
  smri_bl<-c(smri_bl,which(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique)==col))
}

x1.indexs <- smri_bl
y1.indexs <- asr_bl
m1.indexs <- cbcl_bl

### define the list for the multilevel assocaition
x1.index <- rep()
y1.index <- rep()
m1.index <- rep()
n <- 1
for (i in 1:length(x1.indexs)){
  for(j in 1:length(m1.indexs)){
    x1.index[n] <- x1.indexs[i]
    y1.index[n] <- y1.indexs[j]
    m1.index[n] <- m1.indexs[j]
    n <- n + 1
  }
}
outfile.name <- 'result\\single_mediation_after_eg_child_driven.csv'
assList.SingleMed.B <- data.frame(x1.index, y1.index, m1.index)
SingleMediation_eg<-c(SingleMediation_eg_total,SingleMediation_eg_inter,SingleMediation_eg_exter,SingleMediation_eg_thoug,SingleMediation_eg_atten)
results_clpm <- SingleMed.fun(assList.SingleMed.B,SingleMediation_eg, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_unique, outfile.name)

# 3.fdr for single mediation -------------
### 3.1.fdr for single mediation(model A)####
library(openxlsx)
setwd('result\\')
result1 <- read.csv("single_mediation_after_eg_parent_driven.csv", header = TRUE)
result1 <- result1[result1$X_set=="asr_scr_totprob_r",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result1 <- result1[result1$Y_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res1<-data.frame(results_p_fdr1,result1)

result1 <- read.csv("single_mediation_after_eg_parent_driven.csv", header = TRUE)
result1 <- result1[!result1$X_set=="asr_scr_totprob_r",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result1 <- result1[result1$Y_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res2<-data.frame(results_p_fdr1,result1)

result1 <- read.csv("single_mediation_after_eg_parent_driven.csv", header = TRUE)
result1 <- result1[result1$X_set=="asr_scr_totprob_r",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result1 <- result1[!result1$Y_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res3<-data.frame(results_p_fdr1,result1)

result1 <- read.csv("single_mediation_after_eg_parent_driven.csv", header = TRUE)
result1 <- result1[!result1$X_set=="asr_scr_totprob_r",]
result1 <- result1[result1$Y_set==result1$Y_set[c(grep("smri_area_cdk_",result1$Y_set))],]
result1 <- result1[!result1$Y_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res4<-data.frame(results_p_fdr1,result1)

sheets <- list("tot_totprob"=res1,"tot_4prob"=res2,"reg_totprob"=res3,"reg_4prob"=res4)
write.xlsx(sheets,"summary_parent_driven_area_after.xlsx")

### 3.2.fdr for single mediation(model B)####
setwd('result\\')
result1 <- read.csv("single_mediation_before_eg_child_driven.csv", header = TRUE)
result1 <- result1[result1$Y_set=="asr_scr_totprob_r",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result1 <- result1[result1$X_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res1<-data.frame(results_p_fdr1,result1)

result1 <- read.csv("single_mediation_before_eg_child_driven.csv", header = TRUE)
result1 <- result1[!result1$Y_set=="asr_scr_totprob_r",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result1 <- result1[result1$X_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res2<-data.frame(results_p_fdr1,result1)

result1 <- read.csv("single_mediation_before_eg_child_driven.csv", header = TRUE)
result1 <- result1[result1$Y_set=="asr_scr_totprob_r",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result1 <- result1[!result1$X_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res3<-data.frame(results_p_fdr1,result1)

result1 <- read.csv("single_mediation_before_eg_child_driven.csv", header = TRUE)
result1 <- result1[!result1$Y_set=="asr_scr_totprob_r",]
result1 <- result1[result1$X_set==result1$X_set[c(grep("smri_area_cdk_",result1$X_set))],]
result1 <- result1[!result1$X_set=="smri_area_cdk_total",]
results_p_fdr1 <- p.adjust(result1$ab_pvalue,method = "fdr")
res4<-data.frame(results_p_fdr1,result1)

sheets <- list("tot_totprob"=res1,"tot_4prob"=res2,"reg_totprob"=res3,"reg_4prob"=res4)
write.xlsx(sheets,"summary_child_driven_area_before.xlsx")

# 4.select survive region(parent-driven) ---------------
### 4.1.volume(choose from etable) ####
library(readxl)
setwd('result\\')
result_before1 <- read_excel("summary_parent_driven_vol_before.xlsx",1, col_names = TRUE)
result_before1 <- result_before1[result_before1$results_p_fdr1<=0.05,]
result_after1 <- read_excel("summary_parent_driven_vol_after.xlsx",1, col_names = TRUE)
result_after1 <- result_after1[result_after1$ab_pvalue<=0.05,]
vol_survive_before1 <- result_before1[result_before1$X %in% result_after1$X,]
vol_survive_after1 <- result_after1[result_after1$X %in% result_before1$X,]

result_before2 <- read_excel("summary_parent_driven_vol_before.xlsx",2, col_names = TRUE)
result_before2 <- result_before2[result_before2$results_p_fdr1<=0.05,]
result_after2 <- read_excel("summary_parent_driven_vol_after.xlsx",2, col_names = TRUE)
result_after2 <- result_after2[result_after2$ab_pvalue<=0.05,]
vol_survive_before2 <- result_before2[result_before2$X %in% result_after2$X,]
vol_survive_after2 <- result_after2[result_after2$X %in% result_before2$X,]

result_before3 <- read_excel("summary_parent_driven_vol_before.xlsx",3, col_names = TRUE)
result_before3 <- result_before3[result_before3$results_p_fdr1<=0.05,]
result_after3 <- read_excel("summary_parent_driven_vol_after.xlsx",3, col_names = TRUE)
result_after3 <- result_after3[result_after3$ab_pvalue<=0.05,]
vol_survive_before3 <- result_before3[result_before3$X %in% result_after3$X,]
vol_survive_after3 <- result_after3[result_after3$X %in% result_before3$X,]

result_before4 <- read_excel("summary_parent_driven_vol_before.xlsx",4, col_names = TRUE)
result_before4 <- result_before4[result_before4$results_p_fdr1<=0.05,]
result_after4 <- read_excel("summary_parent_driven_vol_after.xlsx",4, col_names = TRUE)
result_after4 <- result_after4[result_after4$ab_pvalue<=0.05,]
vol_survive_before4 <- result_before4[result_before4$X %in% result_after4$X,]
vol_survive_after4 <- result_after4[result_after4$X %in% result_before4$X,]

write.csv(rbind(vol_survive_before1,vol_survive_before2,vol_survive_before3,vol_survive_before4),"med_vol_survive_before.csv");
write.csv(rbind(vol_survive_after1,vol_survive_after2,vol_survive_after3,vol_survive_after4),"med_vol_survive_after.csv");

### 4.2 area(choose from etable)####
library(readxl)
setwd('result\\')
result_before1 <- read_excel("summary_parent_driven_area_before.xlsx",1, col_names = TRUE)
result_before1 <- result_before1[result_before1$results_p_fdr1<=0.05,]
result_after1 <- read_excel("summary_parent_driven_area_after.xlsx",1, col_names = TRUE)
result_after1 <- result_after1[result_after1$ab_pvalue<=0.05,]
vol_survive_before1 <- result_before1[result_before1$X %in% result_after1$X,]
vol_survive_after1 <- result_after1[result_after1$X %in% result_before1$X,]

result_before2 <- read_excel("summary_parent_driven_area_before.xlsx",2, col_names = TRUE)
result_before2 <- result_before2[result_before2$results_p_fdr1<=0.05,]
result_after2 <- read_excel("summary_parent_driven_area_after.xlsx",2, col_names = TRUE)
result_after2 <- result_after2[result_after2$ab_pvalue<=0.05,]
vol_survive_before2 <- result_before2[result_before2$X %in% result_after2$X,]
vol_survive_after2 <- result_after2[result_after2$X %in% result_before2$X,]

result_before3 <- read_excel("summary_parent_driven_area_before.xlsx",3, col_names = TRUE)
result_before3 <- result_before3[result_before3$results_p_fdr1<=0.05,]
result_after3 <- read_excel("summary_parent_driven_area_after.xlsx",3, col_names = TRUE)
result_after3 <- result_after3[result_after3$ab_pvalue<=0.05,]
vol_survive_before3 <- result_before3[result_before3$X %in% result_after3$X,]
vol_survive_after3 <- result_after3[result_after3$X %in% result_before3$X,]

result_before4 <- read_excel("summary_parent_driven_area_before.xlsx",4, col_names = TRUE)
result_before4 <- result_before4[result_before4$results_p_fdr1<=0.05,]
result_after4 <- read_excel("summary_parent_driven_area_after.xlsx",4, col_names = TRUE)
result_after4 <- result_after4[result_after4$ab_pvalue<=0.05,]
vol_survive_before4 <- result_before4[result_before4$X %in% result_after4$X,]
vol_survive_after4 <- result_after4[result_after4$X %in% result_before4$X,]

write.csv(rbind(vol_survive_before1,vol_survive_before2,vol_survive_before3,vol_survive_before4),"med_area_survive_before.csv");
write.csv(rbind(vol_survive_after1,vol_survive_after2,vol_survive_after3,vol_survive_after4),"med_area_survive_after.csv")