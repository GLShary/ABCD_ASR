rm(list=ls())
gc()
library(pheatmap)
library(data.table)
library(dplyr)
library(readxl)
library(lmerTest)
library(robustHD)
library(RColorBrewer)
library(MuMIn)
library(fmsb)


setwd('data\\')
project2_dataset_baseline_rm_NA_win_prs<-readRDS("project2_dataset_baseline_rm_NA_win_prs.RDS")

# 1.calculate correlation of asr and factors ------------------

### Select exposures
name_exposure<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_attention_r","asr_scr_thought_r")
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

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

analysis1_outcome<-rep()
analysis1_exposure<-rep()
analysis1_out_beta<-rep()
analysis1_out_tvalue <- rep()
analysis1_out_pvalue<-rep()
analysis1_out_df<-rep()
analysis1_out_stdCoef<-rep()
analysis1_out_r.squared<-rep()
analysis1_out_ci_l<-rep()
analysis1_out_ci_u<-rep()
analysis1_number=1

for (i in name_confounding){
  for(j in name_exposure){
    project2_dataset_baseline_rm_NA_win_prs$z = as.numeric(unlist(project2_dataset_baseline_rm_NA_win_prs[i]))
    project2_dataset_baseline_rm_NA_win_prs$x = as.numeric(unlist(project2_dataset_baseline_rm_NA_win_prs[j]))
    if(is.logical(grep("_SCORE_",i))==FALSE){
      lmer.fit <- lmer(z~female + race_ethnicity+interview_age +x+(1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs)
    }
    else{
      lmer.fit <- lmer(z~paste0("prs_PC",1:15,collapse="+")+female + race_ethnicity+interview_age +x+(1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs)
    }
    analysis1<-summary(lmer.fit)$coefficients
    analysis1_out_tvalue[analysis1_number] = analysis1['x',4]
    analysis1_out_beta[analysis1_number] = analysis1['x',1]
    analysis1_out_df[analysis1_number] = analysis1['x',3]
    analysis1_out_pvalue[analysis1_number] = analysis1['x',5]
    analysis1_out_stdCoef[analysis1_number] = stdCoef.merMod(lmer.fit)['x',1]
    analysis1_out_r.squared[analysis1_number] = r.squaredGLMM(lmer.fit)[1]
    analysis1_outcome[analysis1_number] = i
    analysis1_exposure[analysis1_number] = j
    analysis1_out_ci_l[analysis1_number] = confint(lmer.fit, method="Wald")['x',1]
    analysis1_out_ci_u[analysis1_number] = confint(lmer.fit, method="Wald")['x',2]
    analysis1_number = analysis1_number + 1
  }
}
results_analysis1 <- data.frame()
results_analysis1 = data.frame(analysis1_outcome, analysis1_exposure,analysis1_out_pvalue, analysis1_out_tvalue, analysis1_out_beta,analysis1_out_stdCoef,analysis1_out_r.squared,analysis1_out_df,analysis1_out_ci_l,analysis1_out_ci_u)
results_analysis1$fdr_p <- p.adjust(result$analysis1_out_pvalue,method = "fdr")
write.csv(results_analysis1,'result\\1.1.asr&factor_fdr.csv')

# 2.lmer for each domain -------------
setwd('data\\')
project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy<-readRDS("project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy.RDS")

analysis1_asr<-rep()
analysis1_out_beta_e_sub<-rep();analysis1_out_beta_e_fam<-rep();analysis1_out_beta_e_birth<-rep();analysis1_out_beta_g_fam<-rep();analysis1_out_beta_g_prs<-rep()
analysis1_out_pvalue_e_sub<-rep();analysis1_out_pvalue_e_fam<-rep();analysis1_out_pvalue_e_birth<-rep();analysis1_out_pvalue_g_fam<-rep();analysis1_out_pvalue_g_prs<-rep()
analysis1_out_stdCoef_e_sub<-rep();analysis1_out_stdCoef_e_fam<-rep();analysis1_out_stdCoef_e_birth<-rep();analysis1_out_stdCoef_g_fam<-rep();analysis1_out_stdCoef_g_prs<-rep();
analysis1_out_r.squared<-rep();
analysis1_number=1
asr<-c("total","inter","exter","thought","attention")

for(kk in asr){
  if(kk=="total"){lmer.fit <- lmer(asr_scr_totprob_r~female+race_ethnicity+interview_age + e_sub_total+e_fam_total+e_birth_total+g_fam_total+g_prs_total + (1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,REML = FALSE,control = lmerControl(optimizer = "Nelder_Mead"))}
  if(kk=="inter"){lmer.fit <- lmer(asr_scr_internal_r~female+race_ethnicity+interview_age + e_sub_inter+e_fam_inter+e_birth_inter+g_fam_inter+g_prs_inter + (1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,REML = FALSE,control = lmerControl(optimizer = "Nelder_Mead"))}
  if(kk=="exter"){lmer.fit <- lmer(asr_scr_external_r~female+race_ethnicity+interview_age + e_sub_exter+e_fam_exter+e_birth_exter+g_fam_exter+g_prs_exter + (1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,REML = FALSE,control = lmerControl(optimizer = "Nelder_Mead"))}
  if(kk=="attention"){lmer.fit <- lmer(asr_scr_attention_r~female+race_ethnicity+interview_age + e_sub_atten+e_fam_atten+e_birth_atten+g_fam_atten+g_prs_atten + (1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,REML = FALSE,control = lmerControl(optimizer = "Nelder_Mead"))}
  if(kk=="thought"){lmer.fit <- lmer(asr_scr_thought_r~female+race_ethnicity+interview_age + e_sub_thoug+e_fam_thoug+e_birth_thoug+g_fam_thoug+g_prs_thoug + (1 | site_id_l/rel_family_id),data = project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,REML = FALSE,control = lmerControl(optimizer = "Nelder_Mead"))}
  
  analysis1<-summary(lmer.fit)$coefficients
  analysis1_out_beta_e_sub[analysis1_number] = analysis1[grep('e_sub',rownames(analysis1)),1]
  analysis1_out_beta_e_fam[analysis1_number] = analysis1[grep('e_fam',rownames(analysis1)),1]
  analysis1_out_beta_e_birth[analysis1_number] = analysis1[grep('e_birth',rownames(analysis1)),1]
  analysis1_out_beta_g_fam[analysis1_number] = analysis1[grep('g_fam',rownames(analysis1)),1]
  analysis1_out_beta_g_prs[analysis1_number] = analysis1[grep('g_prs',rownames(analysis1)),1]
  
  analysis1_out_pvalue_e_sub[analysis1_number] = analysis1[grep('e_sub',rownames(analysis1)),5]
  analysis1_out_pvalue_e_fam[analysis1_number] = analysis1[grep('e_fam',rownames(analysis1)),5]
  analysis1_out_pvalue_e_birth[analysis1_number] = analysis1[grep('e_birth',rownames(analysis1)),5]
  analysis1_out_pvalue_g_fam[analysis1_number] = analysis1[grep('g_fam',rownames(analysis1)),5]
  analysis1_out_pvalue_g_prs[analysis1_number] = analysis1[grep('g_prs',rownames(analysis1)),5]
  
  analysis1_out_stdCoef_e_sub[analysis1_number] = stdCoef.merMod(lmer.fit)[grep('e_sub',rownames(analysis1)),1]
  analysis1_out_stdCoef_e_fam[analysis1_number] = stdCoef.merMod(lmer.fit)[grep('e_fam',rownames(analysis1)),1]
  analysis1_out_stdCoef_e_birth[analysis1_number] = stdCoef.merMod(lmer.fit)[grep('e_birth',rownames(analysis1)),1]
  analysis1_out_stdCoef_g_fam[analysis1_number] = stdCoef.merMod(lmer.fit)[grep('g_fam',rownames(analysis1)),1]
  analysis1_out_stdCoef_g_prs[analysis1_number] = stdCoef.merMod(lmer.fit)[grep('g_prs',rownames(analysis1)),1]
  
  analysis1_out_r.squared[analysis1_number] = r.squaredGLMM(lmer.fit)[1]
  analysis1_asr[analysis1_number] = kk
  analysis1_number<-analysis1_number+1
}

results_analysis1 <- data.frame()
results_analysis1 = data.frame(analysis1_asr,analysis1_out_beta_e_sub,analysis1_out_beta_e_fam,analysis1_out_beta_e_birth,analysis1_out_beta_g_fam,analysis1_out_beta_g_prs,
                               analysis1_out_pvalue_e_sub,analysis1_out_pvalue_e_fam,analysis1_out_pvalue_e_birth,analysis1_out_pvalue_g_fam,analysis1_out_pvalue_g_prs,
                               analysis1_out_stdCoef_e_sub,analysis1_out_stdCoef_e_fam,analysis1_out_stdCoef_e_birth,analysis1_out_stdCoef_g_fam,analysis1_out_stdCoef_g_prs,
                               analysis1_out_r.squared)
write.csv(results_analysis1,'result\\1.2.asr&eg_summarize_score.csv');
cat("results_analysis1_asr&conf.csv is exported can ready to download\n")