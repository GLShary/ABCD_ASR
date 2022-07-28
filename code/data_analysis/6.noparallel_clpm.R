##### clpm 
rm(list=ls())
gc()
library(lavaan)

setwd('data\\')
long_data_noscale_unique<-readRDS('long_data_noscale_unique.RDS')

# 1.before control eg factors -----------------------
### 1.1.definition of functions ####
crosslaggedModel <- function (clpmModel, data.clpm, outfile.name){
  colnames.ori <- colnames(data.clpm)
  asr_bl<-rep(c(which(colnames(data.clpm)=="asr_scr_totprob_r_bl"),which(colnames(data.clpm)=="asr_scr_internal_r_bl"),which(colnames(data.clpm)=="asr_scr_external_r_bl"),which(colnames(data.clpm)=="asr_scr_thought_r_bl"),which(colnames(data.clpm)=="asr_scr_attention_r_bl")));
  asr_2y<-rep(c(which(colnames(data.clpm)=="asr_scr_totprob_r_2y"),which(colnames(data.clpm)=="asr_scr_internal_r_2y"),which(colnames(data.clpm)=="asr_scr_external_r_2y"),which(colnames(data.clpm)=="asr_scr_thought_r_2y"),which(colnames(data.clpm)=="asr_scr_attention_r_2y")));
  cbcl_bl<-rep(c(which(colnames(data.clpm)=="cbcl_scr_syn_totprob_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_internal_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_external_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_thought_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_attention_r_bl")));
  cbcl_2y<-rep(c(which(colnames(data.clpm)=="cbcl_scr_syn_totprob_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_internal_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_external_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_thought_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_attention_r_2y")));
  colnames.target <- data.frame(asr_bl,asr_2y,cbcl_bl,cbcl_2y)
  
  iv<-rep();dv<-rep();
  est_a2c<-rep();p_a2c<-rep();cilow_a2c<-rep();ciup_a2c<-rep();
  est_c2a<-rep();p_c2a<-rep();cilow_c2a<-rep();ciup_c2a<-rep();
  est_a2a<-rep();p_a2a<-rep();cilow_a2a<-rep();ciup_a2a<-rep();
  est_c2c<-rep();p_c2c<-rep();cilow_c2c<-rep();ciup_c2c<-rep();
  est_aa<-rep();p_aa<-rep();cilow_aa<-rep();ciup_aa<-rep();
  est_cc<-rep();p_cc<-rep();cilow_cc<-rep();ciup_cc<-rep()
  number<-1
  
  for(i in rep(1:5)){
    colnames(data.clpm)<-colnames.ori
    iv[number] <- colnames(data.clpm[colnames.target$asr_bl[number]])
    dv[number] <- colnames(data.clpm[colnames.target$cbcl_bl[number]])
    
    colnames(data.clpm)[c(colnames.target[number,1],colnames.target[number,2],colnames.target[number,3],colnames.target[number,4])] <- c("x1", "x2", "y1", "y2")  # 1--baseline, 2--follow-up
    fit.clpmModel <- lavaan(clpmModel, data = data.clpm,
                            missing = 'ML', #for the missing data!
                            int.ov.free = F,
                            int.lv.free = F,
                            auto.fix.first = F,
                            auto.fix.single = F,
                            auto.cov.lv.x = F,
                            auto.cov.y = F,
                            auto.var = F)
    res<-standardizedSolution(fit.clpmModel, type = "std.all", se = TRUE, zstat = TRUE,
                              pvalue = TRUE, ci = TRUE, level = 0.95, cov.std = TRUE,
                              remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE,
                              partable = NULL, GLIST = NULL, est = NULL,
                              output = "data.frame")
    est_a2c[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    p_a2c[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    cilow_a2c[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    ciup_a2c[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    
    est_c2a[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    p_c2a[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    cilow_c2a[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    ciup_c2a[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    
    est_a2a[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    p_a2a[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    cilow_a2a[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    ciup_a2a[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    
    est_c2c[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    p_c2c[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    cilow_c2c[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    ciup_c2c[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    
    est_aa[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    p_aa[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    cilow_aa[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    ciup_aa[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    
    est_cc[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    p_cc[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    cilow_cc[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    ciup_cc[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    
    number = number + 1
  }
  result <- data.frame()
  result <- data.frame(iv,dv,
                       est_a2c,p_a2c,cilow_a2c,ciup_a2c,
                       est_c2a,p_c2a,cilow_c2a,ciup_c2a,
                       est_a2a,p_a2a,cilow_a2a,ciup_a2a,
                       est_c2c,p_c2c,cilow_c2c,ciup_c2c,
                       est_aa,p_aa,cilow_aa,ciup_aa,
                       est_cc,p_cc,cilow_cc,ciup_cc)
  write.csv(result, outfile.name);
}

### 1.2.Model 1: cross-lagged path model(before control eg) ####
clpmModel <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
kappa =~ 1*x1 + 1*x2 
omega =~ 1*y1 + 1*y2
x1 ~ mu1*1 #intercepts
x2 ~ mu2*1
y1 ~ pi1*1
y2 ~ pi2*1

kappa ~~ 0*kappa #variance
omega ~~ 0*omega #variance
kappa ~~ 0*omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
q1 =~ 1*y1
q2 =~ 1*y2

#regressions
p2 ~ a2a*p1 + c2a*q1 + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q2 ~ c2c*q1 + a2c*p1+ femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
p1 ~ femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q1 ~ femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4

p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ aa*q1 #p1 and q1 covariance
p2 ~~ cc*q2'

outfile.name <- 'result\\3.1.clpm_before_eg_scale.csv'
results_clpm <- crosslaggedModel(clpmModel, long_data_scale_unique, outfile.name)



# 2.after control eg factors -----------------------
### 2.1.definition of functions ####
crosslaggedModel_control <- function (clpmModel, data.clpm, outfile.name){
  colnames.ori <- colnames(data.clpm)
  asr_bl<-rep(c(which(colnames(data.clpm)=="asr_scr_totprob_r_bl"),which(colnames(data.clpm)=="asr_scr_internal_r_bl"),which(colnames(data.clpm)=="asr_scr_external_r_bl"),which(colnames(data.clpm)=="asr_scr_thought_r_bl"),which(colnames(data.clpm)=="asr_scr_attention_r_bl")));
  asr_2y<-rep(c(which(colnames(data.clpm)=="asr_scr_totprob_r_2y"),which(colnames(data.clpm)=="asr_scr_internal_r_2y"),which(colnames(data.clpm)=="asr_scr_external_r_2y"),which(colnames(data.clpm)=="asr_scr_thought_r_2y"),which(colnames(data.clpm)=="asr_scr_attention_r_2y")));
  cbcl_bl<-rep(c(which(colnames(data.clpm)=="cbcl_scr_syn_totprob_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_internal_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_external_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_thought_r_bl"),which(colnames(data.clpm)=="cbcl_scr_syn_attention_r_bl")));
  cbcl_2y<-rep(c(which(colnames(data.clpm)=="cbcl_scr_syn_totprob_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_internal_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_external_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_thought_r_2y"),which(colnames(data.clpm)=="cbcl_scr_syn_attention_r_2y")));
  colnames.target <- data.frame(asr_bl,asr_2y,cbcl_bl,cbcl_2y)
  
  iv<-rep();dv<-rep();
  est_a2c<-rep();p_a2c<-rep();cilow_a2c<-rep();ciup_a2c<-rep();
  est_c2a<-rep();p_c2a<-rep();cilow_c2a<-rep();ciup_c2a<-rep();
  est_a2a<-rep();p_a2a<-rep();cilow_a2a<-rep();ciup_a2a<-rep();
  est_c2c<-rep();p_c2c<-rep();cilow_c2c<-rep();ciup_c2c<-rep();
  est_aa<-rep();p_aa<-rep();cilow_aa<-rep();ciup_aa<-rep();
  est_cc<-rep();p_cc<-rep();cilow_cc<-rep();ciup_cc<-rep()
  number<-1
  
  for(i in rep(1:5)){
    colnames(data.clpm)<-colnames.ori
    iv[number] <- colnames(data.clpm[colnames.target$asr_bl[number]])
    dv[number] <- colnames(data.clpm[colnames.target$cbcl_bl[number]])
    
    colnames(data.clpm)[c(colnames.target[number,1],colnames.target[number,2],colnames.target[number,3],colnames.target[number,4])] <- c("x1", "x2", "y1", "y2")  # 1--baseline, 2--follow-up
    fit.clpmModel <- lavaan(clpmModel[number], data = data.clpm,
                            missing = 'ML', #for the missing data!
                            int.ov.free = F,
                            int.lv.free = F,
                            auto.fix.first = F,
                            auto.fix.single = F,
                            auto.cov.lv.x = F,
                            auto.cov.y = F,
                            auto.var = F)
    res<-standardizedSolution(fit.clpmModel, type = "std.all", se = TRUE, zstat = TRUE,
                              pvalue = TRUE, ci = TRUE, level = 0.95, cov.std = TRUE,
                              remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE,
                              partable = NULL, GLIST = NULL, est = NULL,
                              output = "data.frame")
    est_a2c[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    p_a2c[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    cilow_a2c[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    ciup_a2c[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='a2c')]
    
    est_c2a[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    p_c2a[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    cilow_c2a[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    ciup_c2a[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='c2a')]
    
    est_a2a[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    p_a2a[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    cilow_a2a[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    ciup_a2a[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='a2a')]
    
    est_c2c[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    p_c2c[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    cilow_c2c[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    ciup_c2c[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='c2c')]
    
    est_aa[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    p_aa[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    cilow_aa[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    ciup_aa[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='aa')]
    
    est_cc[number] <- res$est.std[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    p_cc[number] <- res$pvalue[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    cilow_cc[number] <- res$ci.lower[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    ciup_cc[number] <- res$ci.upper[which(parameterEstimates(fit.clpmModel)$label=='cc')]
    
    number = number + 1
    
  }
  
  result <- data.frame()
  result <- data.frame(iv,dv,
                       est_a2c,p_a2c,cilow_a2c,ciup_a2c,
                       est_c2a,p_c2a,cilow_c2a,ciup_c2a,
                       est_a2a,p_a2a,cilow_a2a,ciup_a2a,
                       est_c2c,p_c2c,cilow_c2c,ciup_c2c,
                       est_aa,p_aa,cilow_aa,ciup_aa,
                       est_cc,p_cc,cilow_cc,ciup_cc)
  write.csv(result, outfile.name);
}


### 2.2.Model 2: cross-lagged path model(after control eg) ####
clpmModel_eg_total <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
kappa =~ 1*x1 + 1*x2 
omega =~ 1*y1 + 1*y2
x1 ~ mu1*1 #intercepts
x2 ~ mu2*1
y1 ~ pi1*1
y2 ~ pi2*1

kappa ~~ 0*kappa #variance
omega ~~ 0*omega #variance
kappa ~~ 0*omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
q1 =~ 1*y1
q2 =~ 1*y2

#regressions
p2 ~ a2a*p1 + c2a*q1 + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q2 ~ c2c*q1 + a2c*p1 + e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
p1 ~ e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q1 ~ e_sub_total + e_fam_total + e_birth_total + g_fam_total + g_prs_total + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4

p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ aa*q1 #p1 and q1 covariance
p2 ~~ cc*q2'

clpmModel_eg_inter <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
kappa =~ 1*x1 + 1*x2 
omega =~ 1*y1 + 1*y2
x1 ~ mu1*1 #intercepts
x2 ~ mu2*1
y1 ~ pi1*1
y2 ~ pi2*1

kappa ~~ 0*kappa #variance
omega ~~ 0*omega #variance
kappa ~~ 0*omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
q1 =~ 1*y1
q2 =~ 1*y2

#regressions
p2 ~ a2a*p1 + c2a*q1 + e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q2 ~ c2c*q1 + a2c*p1+ e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
p1 ~ e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q1 ~ e_sub_inter + e_fam_inter + e_birth_inter + g_fam_inter + g_prs_inter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4

p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ aa*q1 #p1 and q1 covariance
p2 ~~ cc*q2'


clpmModel_eg_exter <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
kappa =~ 1*x1 + 1*x2
omega =~ 1*y1 + 1*y2
x1 ~ mu1*1 #intercepts
x2 ~ mu2*1
y1 ~ pi1*1
y2 ~ pi2*1

kappa ~~ 0*kappa #variance
omega ~~ 0*omega #variance
kappa ~~ 0*omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
q1 =~ 1*y1
q2 =~ 1*y2

#regressions
p2 ~ a2a*p1 + c2a*q1 + e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q2 ~ c2c*q1 + a2c*p1+ e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
p1 ~ e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q1 ~ e_sub_exter + e_fam_exter + e_birth_exter + g_fam_exter + g_prs_exter + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4

p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ aa*q1 #p1 and q1 covariance
p2 ~~ cc*q2'

clpmModel_eg_thoug <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
kappa =~ 1*x1 + 1*x2
omega =~ 1*y1 + 1*y2
x1 ~ mu1*1 #intercepts
x2 ~ mu2*1
y1 ~ pi1*1
y2 ~ pi2*1

kappa ~~ 0*kappa #variance
omega ~~ 0*omega #variance
kappa ~~ 0*omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
q1 =~ 1*y1
q2 =~ 1*y2

#regressions
p2 ~ a2a*p1 + c2a*q1 + e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q2 ~ c2c*q1 + a2c*p1+ e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
p1 ~ e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q1 ~ e_sub_thoug + e_fam_thoug + e_birth_thoug + g_fam_thoug + g_prs_thoug + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4

p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ aa*q1 #p1 and q1 covariance
p2 ~~ cc*q2'

clpmModel_eg_atten <- 
  '
#Note, the data contain x1-2 and y1-2
#Latent mean Structure with intercepts
kappa =~ 1*x1 + 1*x2
omega =~ 1*y1 + 1*y2
x1 ~ mu1*1 #intercepts
x2 ~ mu2*1
y1 ~ pi1*1
y2 ~ pi2*1

kappa ~~ 0*kappa #variance
omega ~~ 0*omega #variance
kappa ~~ 0*omega #covariance

#laten vars for AR and cross-lagged effects
p1 =~ 1*x1 #each factor loading set to 1
p2 =~ 1*x2
q1 =~ 1*y1
q2 =~ 1*y2

#regressions
p2 ~ a2a*p1 + c2a*q1 + e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q2 ~ c2c*q1 + a2c*p1+ e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
p1 ~ e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4
q1 ~ e_sub_atten + e_fam_atten + e_birth_atten + g_fam_atten + g_prs_atten + femaleyes +interview_age+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21 + race1+race2+race3+race4

p1 ~~ p1 #variance
p2 ~~ u2*p2
q1 ~~ q1 #variance
q2 ~~ v2*q2
p1 ~~ aa*q1 #p1 and q1 covariance
p2 ~~ cc*q2'

clpmModel_eg<-c(clpmModel_eg_total,clpmModel_eg_inter,clpmModel_eg_exter,clpmModel_eg_thoug,clpmModel_eg_atten)
outfile.name <- 'result\\3.2.clpm_after_eg_scale.csv'
results_clpm <- crosslaggedModel_control(clpmModel_eg, long_data_scale_unique, outfile.name)
