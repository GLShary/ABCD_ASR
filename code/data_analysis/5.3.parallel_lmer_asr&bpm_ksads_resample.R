rm(list=ls())
gc()
library(foreach)
library(doParallel)
library(lmerTest)
library(readxl)
library(MuMIn)
library(nnet)
numCores <- 8
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
  
  results_analysis <- foreach (number = c(1:dim(assList)[1]), .combine=rbind) %dopar% {
    library(lmerTest)
    library(MuMIn)
    library(dplyr)
    
    data.ori<-data
    count <- 0
    for(i in 1:100){
      data <- data.ori %>% group_by(rel_family_id) %>% sample_n(1)
      #colnames(data) <- colnames.ori
      data$x <- as.numeric(unlist(data[assList$x.name[number]]))
      data$y <- as.numeric(unlist(data[assList$y.name[number]]))
      out <- formula(paste('y~', paste(c('x', as.character(assList$cov.names.fixed[number]), as.character(assList$cov.names.rand[number])), collapse="+")))
      lmer.fit <- lmer(out,data = data)
      analysis2 <- summary(lmer.fit)$coefficients
      out_pvalue = analysis2['x',5]
      
      outcome = x.name[number]
      exposure = y.name[number]
      if(out_pvalue<0.05){
        count<-count+1
      }
    }
    results_analysis <- data.frame(exposure,outcome,out_pvalue,count)
  }
  
  write.csv(results_analysis, outfile.name);
  cat("results are exported in a csv file and ready for download\n")
}



# 2.asr&bpm_ksads before eg -----------------------
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

project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads<-readRDS("data\\project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads.RDS")
name_asr<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_thought_r","asr_scr_attention_r")
name_outcome<-c(name_bpm,name_ksads)
cov.fixed <- 'female + race_ethnicity + interview_age'
cov.rand <- '(1 | site_id_l)'
### define the list for the multilevel assocaition
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
outfile.name <- 'result\\2.3.asr&bpm_ksads_before_eg_resample.csv'
assList.asr.outcome <- data.frame(x.name, y.name, cov.names.fixed, cov.names.rand)
results_analysis <- multiAss.parallel(assList.asr.outcome, project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy_bpm_ksads, outfile.name)
