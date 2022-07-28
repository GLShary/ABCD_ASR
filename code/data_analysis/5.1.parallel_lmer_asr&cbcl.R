rm(list=ls())
gc()
library(foreach)
library(doParallel)
library(lmerTest)
library(readxl)
library(MuMIn)

numCores <- 10
registerDoParallel(numCores)

# 1.definition of functions ------------------

multiAss.parallel <- function (assList, data, outfile.name) {
  # running the multilevel assocation through a table of (x,y,covariates)
  # assList is a dataframe         
  #              $x.name              x variable name.  n rows
  #              $y.name              y varialbe name.  n rows
  #              $cov.names.fixed     fixed effect covariate names in formula. n rows
  #              $cov.names.rand      random effect covariate names in formula. n rows
  # NOTE: the above variable names must be the same as they are in the data
  
  results_analysis <- foreach (number = c(1:dim(assList)[1]), .combine=rbind, .packages = c('MuMIn','lmerTest')) %dopar% {
    stdCoef.merMod <- function(object) {
      sdy <- sd(getME(object,"y"))
      sdx <- apply(getME(object,"X"), 2, sd)
      sc <- fixef(object)*sdx/sdy
      se.fixef <- coef(summary(object))[,"Std. Error"]
      se <- se.fixef*sdx/sdy
      return(data.frame(stdcoef=sc, stdse=se))
    }
    
    data$x <- as.numeric(unlist(data[assList$x.name[number]]))
    out <- formula(paste(assList$y.name[number], '~', paste(c('x', as.character(assList$cov.names.fixed[number]), as.character(assList$cov.names.rand[number])), collapse="+")))
    lmer.fit <- lmer(out,data = data)
    
    analysis2 <- summary(lmer.fit)$coefficients
    out_tvalue = analysis2['x',4]
    out_beta = analysis2['x',1]
    out_df = analysis2['x',3]
    out_pvalue = analysis2['x',5]
    out_stdCoef = stdCoef.merMod(lmer.fit)['x',1]
    out_r.squared = r.squaredGLMM(lmer.fit)[1]
    outcome = x.name[number]
    exposure = y.name[number]
    out_ci_l = confint(lmer.fit, method="Wald")['x',1]
    out_ci_u = confint(lmer.fit, method="Wald")['x',2]
    result <- data.frame(outcome, exposure, out_pvalue, out_tvalue, out_beta, out_stdCoef, out_r.squared, out_df, out_ci_l, out_ci_u)
  }
  write.csv(results_analysis, outfile.name);
  cat("results are exported in a csv file and ready for download\n")
}

# 2.asr&cbcl before eg -----------------
project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy<-readRDS("data\\project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy.RDS")
name_asr<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_thought_r","asr_scr_attention_r")
name_cbcl<-c("cbcl_scr_syn_totprob_r","cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","cbcl_scr_syn_thought_r","cbcl_scr_syn_attention_r")
cov.fixed <- 'female + race_ethnicity + interview_age'
cov.rand <- '(1 | site_id_l/rel_family_id)'
### define the list for the multilevel association
x.name <- rep()
y.name <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
for (i in name_asr){
  for(j in name_cbcl){
    n <- n + 1
    x.name[n] <- i
    y.name[n] <- j
    cov.names.fixed[n] <- cov.fixed
    cov.names.rand[n] <- cov.rand
  }
}
outfile.name <- 'result\\2.1.asr&cbcl_before_eg.csv'
assList.asr.cbcl <- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis_vol <- multiAss.parallel(assList.asr.cbcl, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy, outfile.name)

# 3.sensitivity analysis(after eg)  --------------------
project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy<-readRDS("data\\project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy.RDS")
name_asr<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_thought_r","asr_scr_attention_r")
name_cbcl<-c("cbcl_scr_syn_totprob_r","cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","cbcl_scr_syn_thought_r","cbcl_scr_syn_attention_r")
cov.fixed <- 'female + race_ethnicity + interview_age + '
cov.rand <- '(1 | site_id_l/rel_family_id)'
domain.names<-c('e_sub_total+e_fam_total+e_birth_total+g_fam_total+g_prs_total','e_sub_inter+e_fam_inter+e_birth_inter+g_fam_inter+g_prs_inter','e_sub_exter+e_fam_exter+e_birth_exter+g_fam_exter+g_prs_exter','e_sub_thoug+e_fam_thoug+e_birth_thoug+g_fam_thoug+g_prs_thoug','e_sub_atten+e_fam_atten+e_birth_atten+g_fam_atten+g_prs_atten')

### define the list for the multilevel assocaition
x.name <- rep()
y.name <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
k <- 0
for (i in name_asr){
  k <- k + 1
  for(j in name_cbcl){
    n <- n + 1
    x.name[n] <- i
    y.name[n] <- j
    cov.names.fixed[n] <- paste(cov.fixed,domain.names[k],sep = "")
    cov.names.rand[n] <- cov.rand
  }
}
outfile.name <- 'result\\2.2.asr&cbcl_after_eg.csv'
assList.asr.cbcl <- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis_vol <- multiAss.parallel(assList.asr.cbcl, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy, outfile.name)

stopImplicitCluster()


