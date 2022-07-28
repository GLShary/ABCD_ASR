rm(list=ls())
gc()
library(foreach)
library(doParallel)
library(lmerTest)
library(readxl)
library(MuMIn)

numCores <- 10
registerDoParallel(numCores)


# 1.definition of functions ----------------

multiAss.parallel <- function (assList, data, outfile.name) {
  # running the multilevel assocation through a table of (x,y,covariates)
  # assList is a dataframe         
  #              $x.name              x variable name.  n rows
  #              $y.name              y varialbe name.  n rows
  #              $cov.names.fixed     fixed effect covariate names in formula. n rows
  #              $cov.names.rand      random effect covariate names in formula. n rows
  # NOTE: the above variable names must be the same as they are in the data
  
  results_analysis <- foreach (number = c(1:dim(assList)[1]), .combine=rbind) %dopar% {
    library(lmerTest)
    library(MuMIn)
    
    stdCoef.merMod <- function(object) {
      sdy <- sd(getME(object,"y"))
      sdx <- apply(getME(object,"X"), 2, sd)
      sc <- fixef(object)*sdx/sdy
      se.fixef <- coef(summary(object))[,"Std. Error"]
      se <- se.fixef*sdx/sdy
      return(data.frame(stdcoef=sc, stdse=se))
    }
    data$x <- as.numeric(unlist(data[assList$x.name[number]]))
    data$y <- as.numeric(unlist(data[assList$y.name[number]]))
    out <- formula(paste('y~', paste(c('x', as.character(assList$cov.names.fixed[number]), as.character(assList$cov.names.rand[number])), collapse="+")))
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

# 2.asr&bpm_ksads before eg -------------
name_bpm <- c("bpm_t_scr_totalprob_r","bpm_t_scr_internal_r","bpm_t_scr_external_r","bpm_t_scr_attention_r")
name_ksads <- c(
  "KSADS.Depression",
  "KSADS.Bipolar.and.Related.Disorder",
  "KSADS.Auditory.Hallucinations",
  "KSADS.Delusions",
  "KSADS.Associated.Psychotic.Symptoms",
  "KSADS.Panic.and.Other.Specified.Anxiety.Disorder",
  "KSADS.Agoraphobia.and.Other.Specified.Anxiety.Disorder",
  "KSADS.Separation.and.Other.Specified.Anxiety.Disorder",
  "KSADS.Social.and.Other.Specified.Anxiety.Disorder",
  "KSADS.Specific.Phobia",
  "KSADS.Generalized.Anxiety.Disorder.and.other.Specified.Anxiety.Disorder",
  "KSADS.Obsessive.Compulsive.and.Related.Disorder",
  "KSADS.Encopresis",
  "KSADS.Feeding.or.Eating.Disorder",
  "KSADS.Attention.Deficit.Hyperactivity.Disorder",
  "KSADS.Oppositional.Defiant.Disorder",
  "KSADS.Conduct.Disorder",
  "KSADS.Alcohol.Use.and.Alcohol.Related.Disorder",
  "KSADS.Substance.Related.Disorder",
  "KSADS.PTSD.and.Other.Specified.Trauma.and.Stressor.Related.Disorder",
  "KSADS.Insomnia",
  "KSADS.homicidal.ideation.and.behavior",
  "KSADS.Selective.Mutism.and.Other.Specified.Anxiety.Disorder")

project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads_unique<-readRDS("data\\project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads_unique.RDS")
name_asr<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_thought_r","asr_scr_attention_r")
name_outcome<-c(name_bpm,name_ksads)
cov.fixed <- 'female + race_ethnicity + interview_age'
cov.rand <- '(1 | site_id_l)'
# define the list for the multilevel association
x.name <- rep()
y.name <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
for (i in name_asr){
  for(j in name_outcome){
    n <- n + 1
    x.name[n] <- i
    y.name[n] <- j
    cov.names.fixed[n] <- cov.fixed
    cov.names.rand[n] <- cov.rand
  }
}
outfile.name <- 'result\\2.3.asr&bpm_ksads_before_eg_unique.csv'
assList.asr.outcome <- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis <- multiAss.parallel(assList.asr.outcome, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads_unique, outfile.name)

setwd('result\\')
result <- read.csv("2.3.asr&bpm_ksads_before_eg_unique.csv", header = TRUE)
result$fdr_p <- p.adjust(result$out_pvalue,method = "fdr")
write.csv(result,'result\\2.3.asr&bpm_ksads_before_eg_unique_fdr.csv')

# 3.sensitivity analysis(after eg)     ########################
project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads_unique<-readRDS("data\\project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads_unique.RDS")
name_asr<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_thought_r","asr_scr_attention_r")
name_outcome<-c(name_bpm,name_ksads)
cov.fixed <- 'female + race_ethnicity + interview_age'
cov.rand <- '(1 | site_id_l)'
#domain.names<-c('e_sub_total+e_fam_total+e_birth_total+g_fam_total+g_prs_total','e_sub_inter+e_fam_inter+e_birth_inter+g_fam_inter+g_prs_inter','e_sub_exter+e_fam_exter+e_birth_exter+g_fam_exter+g_prs_exter','e_sub_thoug+e_fam_thoug+e_birth_thoug+g_fam_thoug+g_prs_thoug','e_sub_atten+e_fam_atten+e_birth_atten+g_fam_atten+g_prs_atten')
domain.names<-c('e_sub_total+e_fam_total+e_birth_total+g_fam_total+g_prs_total','e_sub_inter+e_fam_inter+e_birth_inter+g_fam_inter+g_prs_inter','e_sub_exter+e_fam_exter+e_birth_exter+g_fam_exter+g_prs_exter','e_sub_thoug+e_fam_thoug+e_birth_thoug+g_fam_thoug+g_fam_thoug','e_sub_atten+e_fam_atten+e_birth_atten+g_fam_atten+g_prs_atten')

# define the list for the multilevel assocaition
x.name <- rep()
y.name <- rep()
cov.names.fixed <- rep()
cov.names.rand <- rep()
n <- 0
k <- 0
for (i in name_asr){
  k <- k + 1
  for(j in name_outcome){
    n <- n + 1
    x.name[n] <- i
    y.name[n] <- j
    cov.names.fixed[n] <- paste(cov.fixed,domain.names[k],sep = "+")
    cov.names.rand[n] <- cov.rand
  }
}

outfile.name <- 'result\\2.4.asr&bpm_ksads_after_eg_unique.csv'
assList.asr.outcome <- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis <- multiAss.parallel(assList.asr.outcome, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads_unique, outfile.name)

stopImplicitCluster()