rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(readxl)
library(nnet)

# 1.summarize score ---------------------
setwd('data\\')
project2_dataset_rm_NA_win_prs<-readRDS("project2_dataset_rm_NA_win_prs.RDS")

### Select exposures
name_exposure<-c("asr_scr_totprob_r","asr_scr_internal_r","asr_scr_external_r","asr_scr_thought_r","asr_scr_attention_r")
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

### read beta coefficient of correation of asr & confounding factors
setwd('result\\')
result1 <- read.csv("1.1.asr&factor_fdr.csv", header = TRUE)
result_beta <- result1$analysis1_out_stdCoef
res_beta=matrix(data = result_beta, nrow =length(name_confounding), ncol = length(name_exposure),byrow = TRUE);
colnames(res_beta) =name_exposure;
rownames(res_beta) =name_confounding
res_beta<-res_beta[-c(45:51,53:59,61:67),]

### weighted score
name_confounding<-name_confounding[-c(45:51,53:59,61:67)]
project2_dataset_rm_NA_win_prs_summarize<-project2_dataset_rm_NA_win_prs

total_project2_dataset_rm_NA_win_prs_summarize<-project2_dataset_rm_NA_win_prs_summarize
inter_project2_dataset_rm_NA_win_prs_summarize<-project2_dataset_rm_NA_win_prs_summarize
exter_project2_dataset_rm_NA_win_prs_summarize<-project2_dataset_rm_NA_win_prs_summarize
attention_project2_dataset_rm_NA_win_prs_summarize<-project2_dataset_rm_NA_win_prs_summarize
thought_project2_dataset_rm_NA_win_prs_summarize<-project2_dataset_rm_NA_win_prs_summarize
for(i in name_confounding){
  total_project2_dataset_rm_NA_win_prs_summarize[,i]<-as.numeric(total_project2_dataset_rm_NA_win_prs_summarize[,i])*res_beta[i,1]
  inter_project2_dataset_rm_NA_win_prs_summarize[,i]<-as.numeric(inter_project2_dataset_rm_NA_win_prs_summarize[,i])*res_beta[i,2]
  exter_project2_dataset_rm_NA_win_prs_summarize[,i]<-as.numeric(exter_project2_dataset_rm_NA_win_prs_summarize[,i])*res_beta[i,3]
  attention_project2_dataset_rm_NA_win_prs_summarize[,i]<-as.numeric(attention_project2_dataset_rm_NA_win_prs_summarize[,i])*res_beta[i,5]
  thought_project2_dataset_rm_NA_win_prs_summarize[,i]<-as.numeric(thought_project2_dataset_rm_NA_win_prs_summarize[,i])*res_beta[i,4]
}

### sum score of each kind of factor
project2_dataset_rm_NA_win_prs_summarize$e_sub_total<-apply(total_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_sub)], 1, sum)
project2_dataset_rm_NA_win_prs_summarize$e_fam_total<-apply(total_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_birth_total<-apply(total_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_birth)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_fam_total<-apply(total_project2_dataset_rm_NA_win_prs_summarize[,c(name_g_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_prs_total<-apply(total_project2_dataset_rm_NA_win_prs_summarize[,c("adhd_SCORE_avg","scz_SCORE_avg","mdd_SCORE_avg")],1,sum)

project2_dataset_rm_NA_win_prs_summarize$e_sub_inter<-apply(inter_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_sub)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_fam_inter<-apply(inter_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_birth_inter<-apply(inter_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_birth)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_fam_inter<-apply(inter_project2_dataset_rm_NA_win_prs_summarize[,c(name_g_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_prs_inter<-apply(inter_project2_dataset_rm_NA_win_prs_summarize[,c("adhd_SCORE_avg","scz_SCORE_avg","mdd_SCORE_avg")],1,sum)

project2_dataset_rm_NA_win_prs_summarize$e_sub_exter<-apply(exter_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_sub)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_fam_exter<-apply(exter_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_birth_exter<-apply(exter_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_birth)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_fam_exter<-apply(exter_project2_dataset_rm_NA_win_prs_summarize[,c(name_g_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_prs_exter<-apply(exter_project2_dataset_rm_NA_win_prs_summarize[,c("adhd_SCORE_avg","scz_SCORE_avg","mdd_SCORE_avg")],1,sum)

project2_dataset_rm_NA_win_prs_summarize$e_sub_atten<-apply(attention_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_sub)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_fam_atten<-apply(attention_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_birth_atten<-apply(attention_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_birth)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_fam_atten<-apply(attention_project2_dataset_rm_NA_win_prs_summarize[,c(name_g_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_prs_atten<-apply(attention_project2_dataset_rm_NA_win_prs_summarize[,c("adhd_SCORE_avg","scz_SCORE_avg","mdd_SCORE_avg")],1,sum)

project2_dataset_rm_NA_win_prs_summarize$e_sub_thoug<-apply(thought_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_sub)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_fam_thoug<-apply(thought_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$e_birth_thoug<-apply(thought_project2_dataset_rm_NA_win_prs_summarize[,c(name_e_birth)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_fam_thoug<-apply(thought_project2_dataset_rm_NA_win_prs_summarize[,c(name_g_fam)],1,sum)
project2_dataset_rm_NA_win_prs_summarize$g_prs_thoug<-apply(thought_project2_dataset_rm_NA_win_prs_summarize[,c("adhd_SCORE_avg","scz_SCORE_avg","mdd_SCORE_avg")],1,sum)

setwd("data\\")
saveRDS(project2_dataset_rm_NA_win_prs_summarize,file = "project2_dataset_rm_NA_win_prs_summarize.RDS")
project2_dataset_baseline_rm_NA_win_prs_summarize=project2_dataset_rm_NA_win_prs_summarize[project2_dataset_rm_NA_win_prs_summarize$eventname == "baseline_year_1_arm_1",]
saveRDS(project2_dataset_baseline_rm_NA_win_prs_summarize,file = "project2_dataset_baseline_rm_NA_win_prs_summarize.RDS")
project2_dataset_2year_rm_NA_win_prs_summarize=project2_dataset_rm_NA_win_prs_summarize[project2_dataset_rm_NA_win_prs_summarize$eventname == "2_year_follow_up_y_arm_1",]
saveRDS(project2_dataset_2year_rm_NA_win_prs_summarize,file = "project2_dataset_2year_rm_NA_win_prs_summarize.RDS")

# 2.scale and dummy data ---------------------------------
### scale data
project2_dataset_rm_NA_win_prs_summarize_scale<-project2_dataset_rm_NA_win_prs_summarize
scaled<-c(15:204,206:269)
for(i in scaled){
  project2_dataset_rm_NA_win_prs_summarize_scale[,i]<-scale(project2_dataset_rm_NA_win_prs_summarize_scale[,i])
}
saveRDS(project2_dataset_rm_NA_win_prs_summarize_scale,file = "project2_dataset_rm_NA_win_prs_summarize_scale.RDS")

### dummy variable which is categorical
project2_dataset_rm_NA_win_prs_summarize_scale<-readRDS("project2_dataset_rm_NA_win_prs_summarize_scale.RDS")
dummysex=class.ind(project2_dataset_rm_NA_win_prs_summarize_scale$female)
dummyrace=class.ind(project2_dataset_rm_NA_win_prs_summarize_scale$race_ethnicity)
dummysite=class.ind(project2_dataset_rm_NA_win_prs_summarize_scale$site_id_l)
project2_dataset_rm_NA_win_prs_summarize_scale=cbind(dummysex, dummyrace, dummysite, project2_dataset_rm_NA_win_prs_summarize_scale)
project2_dataset_rm_NA_win_prs_summarize_scale = project2_dataset_rm_NA_win_prs_summarize_scale[,c(-1,-6,-8)]
colnames(project2_dataset_rm_NA_win_prs_summarize_scale)[1:27] <- c("femaleyes","race1","race2","race3","race4","s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21","s22")
saveRDS(project2_dataset_rm_NA_win_prs_summarize_scale,file = "project2_dataset_rm_NA_win_prs_summarize_scale_dummy.RDS")

project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy=project2_dataset_rm_NA_win_prs_summarize_scale[project2_dataset_rm_NA_win_prs_summarize_scale$eventname == "baseline_year_1_arm_1",]
saveRDS(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,file = "project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy.RDS")
project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy=project2_dataset_rm_NA_win_prs_summarize_scale[project2_dataset_rm_NA_win_prs_summarize_scale$eventname == "2_year_follow_up_y_arm_1",]
saveRDS(project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy,file = "project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy.RDS")

# 3.merge data at baseline and 2 year ----------------
colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy)[79:231]<-paste(c(colnames(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy)[79:231]),"_bl",sep = "")
colnames(project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy)[79:231]<-paste(c(colnames(project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy)[79:231]),"_2y",sep = "")
project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy<-project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy[,-c(1:27,30:78,232:296)]
long_data <- left_join(project2_dataset_baseline_rm_NA_win_prs_summarize_scale_dummy,project2_dataset_2year_rm_NA_win_prs_summarize_scale_dummy, by="src_subject_id")
saveRDS(long_data,file = "long_data.RDS")

# 4.save clpm data ------------------
long_data_no_del<-readRDS("long_data.RDS")
for(i in c(79:88,298:307)){
  long_data_no_del=long_data_no_del[!(is.na(long_data_no_del[,i])),]
}#delete subject whose asr or cbcl is NA
saveRDS(long_data_no_del,file = "long_data_clpm.RDS")

# 5.save longitudinal mediation data -----------------
setwd("data\\")
long_data_no_del<-readRDS("long_data_clpm.RDS")
for(i in c(79:231,298:450)){
  long_data_no_del=long_data_no_del[!(is.na(long_data_no_del[,i])),]
}#delete subject whose smri is NA
saveRDS(long_data_no_del,file = "long_data_mediation.RDS")

### select one child each family
family_num <- unique(long_data_mediation$rel_family_id)
ind_fam<-c()#index of selected child
pickedfam<-c()
for (i in 1:dim(long_data_mediation)[1]){
  subject<-long_data_mediation[i,'rel_family_id']
  if((!is.na(subject)) & (!subject %in% pickedfam )){
    ind_fam<-c(ind_fam,i)
    pickedfam<-c(pickedfam,as.numeric(as.character(subject)))
  }
}
long_data_clpm_unique<-long_data_mediation[ind_fam,]
dim(long_data_mediation_unique)
saveRDS(long_data_mediation_unique,file = "long_data_mediation_unique.RDS")