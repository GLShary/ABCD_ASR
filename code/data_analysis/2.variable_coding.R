rm(list=ls())
gc()

## Load libraries ####
library(Hmisc)
library(psych)
library(mice)
library(data.table)
library(dplyr)
library(readxl)
library(purrr)
library(broom)
library(mgcv)
library(lubridate)
library(robustHD)
library(foreign)

setwd('data\\')
project2_dataset_rm_NA <- readRDS("project2_dataset_rm_NA.RDS")

# 1.integration & process data for analyses ---------------------------------
### sex
project2_dataset_rm_NA$female = factor(as.numeric(project2_dataset_rm_NA$sex == "F"), levels = 0:1, labels = c("no", "yes") ) 

### race_ethnicity
project2_dataset_rm_NA$race_ethnicity[project2_dataset_rm_NA$race_ethnicity==1]<-'White';
project2_dataset_rm_NA$race_ethnicity[project2_dataset_rm_NA$race_ethnicity==2]<-'Black';
project2_dataset_rm_NA$race_ethnicity[project2_dataset_rm_NA$race_ethnicity==3]<-'Hispanic';
project2_dataset_rm_NA$race_ethnicity[project2_dataset_rm_NA$race_ethnicity==4]<-'Asian';
project2_dataset_rm_NA$race_ethnicity[project2_dataset_rm_NA$race_ethnicity==5]<-'Other';
project2_dataset_rm_NA$race_ethnicity<-as.factor(project2_dataset_rm_NA$race_ethnicity)

### interview age
project2_dataset_rm_NA$interview_age<-(project2_dataset_rm_NA$interview_age)/12

### family id
project2_dataset_rm_NA$rel_family_id<-as.factor(project2_dataset_rm_NA$rel_family_id)

### substnace use
project2_dataset_rm_NA$tobacco<-as.factor(project2_dataset_rm_NA$devhx_8_tobacco.x|project2_dataset_rm_NA$devhx_9_tobacco.x)
project2_dataset_rm_NA$alcohol<-as.factor(project2_dataset_rm_NA$devhx_8_alcohol.x|project2_dataset_rm_NA$devhx_9_alcohol.x)
project2_dataset_rm_NA$marijuana<-as.factor(project2_dataset_rm_NA$devhx_8_marijuana.x|project2_dataset_rm_NA$devhx_9_marijuana.x)
project2_dataset_rm_NA$devhx_caffeine_11.x[project2_dataset_rm_NA$devhx_caffeine_11.x==-1] = NA
project2_dataset_rm_NA$devhx_caffeine_11.x[project2_dataset_rm_NA$devhx_caffeine_11.x>0] = TRUE
project2_dataset_rm_NA$devhx_caffeine_11.x[project2_dataset_rm_NA$devhx_caffeine_11.x==0] = FALSE
project2_dataset_rm_NA$caffeine<-as.factor(project2_dataset_rm_NA$devhx_caffeine_11.x)
project2_dataset_rm_NA$coc_crack<-as.factor(project2_dataset_rm_NA$devhx_8_coc_crack.x|project2_dataset_rm_NA$devhx_9_coc_crack.x)
project2_dataset_rm_NA$her_morph<-as.factor(project2_dataset_rm_NA$devhx_8_her_morph.x|project2_dataset_rm_NA$devhx_9_her_morph.x)
project2_dataset_rm_NA$oxycont<-as.factor(project2_dataset_rm_NA$devhx_8_oxycont.x|project2_dataset_rm_NA$devhx_9_oxycont.x)

### household income
household.income = project2_dataset_rm_NA$demo_comb_income_v2
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "1"] = 1 # "[<50K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "2"] = 1 # "[<50K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "3"] = 1 # "[<50K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "4"] = 1 # "[<50K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "5"] = 1 # "[<50K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "6"] = 1 # "[<50K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "7"] = 2 # "[>=50K & <100K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "8"] = 2 # "[>=50K & <100K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "9"] = 3 # "[>=100K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "10"] = 3 # "[>=100K]"
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "777"] = NA
household.income[project2_dataset_rm_NA$demo_comb_income_v2 == "999"] = NA
household.income[household.income %in% c(NA, "999", "777")] = NA
project2_dataset_rm_NA$household.income = factor( household.income, levels= 1:3, labels = c("[<50K]", "[>=50K & <100K]", "[>=100K]") )

### high education(Here a simplified version of the highest education that results in only 5 different levels. These levels correspond to the numbers published by the American Community Survey (ACS).)
high.educ1 = project2_dataset_rm_NA$demo_prnt_ed_v2
high.educ2 = project2_dataset_rm_NA$demo_prtnr_ed_v2
high.educ1[which(high.educ1 == "999")] = NA
high.educ2[which(high.educ2 == "999")] = NA
high.educ1[which(high.educ1 == "777")] = NA
high.educ2[which(high.educ2 == "777")] = NA
high.educ = pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)), na.rm=T)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] = 1 # "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] = 2 # "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] = 3 # "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] = 4 # "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] = 5 # "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
high.educ[which(high.educ == "777")]=NA
project2_dataset_rm_NA$high.educ = factor( high.educ, levels= 1:5, labels = c("< HS Diploma","HS Diploma/GED","Some College","Bachelor","Post Graduate Degree") )

### marrital status
married = rep(NA, length(project2_dataset_rm_NA$demo_prnt_marital_v2))
married[project2_dataset_rm_NA$demo_prnt_marital_v2 == 1] = 1
married[project2_dataset_rm_NA$demo_prnt_marital_v2 %in% 2:6] = 0
project2_dataset_rm_NA$married = factor( married, levels= 0:1, labels = c("no", "yes") )
### Add another variable that also includes couples that just live together. 
married.livingtogether = rep(NA, length(project2_dataset_rm_NA$demo_prnt_marital_v2))
married.livingtogether[project2_dataset_rm_NA$demo_prnt_marital_v2 %in% c(1,6)] = 1
married.livingtogether[project2_dataset_rm_NA$demo_prnt_marital_v2 %in% 2:5] = 0
project2_dataset_rm_NA$married.or.livingtogether = factor( married.livingtogether, levels= 0:1, labels = c("no", "yes") )

### Body-Mass index
anthroweight_3m<-c('anthroweight1lb','anthroweight2lb','anthroweight3lb')
anthroweight_2m<-c('anthroweight1lb','anthroweight2lb')
project2_dataset_rm_NA$anthroweight_avg<-rowMeans(project2_dataset_rm_NA[anthroweight_3m],na.rm=TRUE)
project2_dataset_rm_NA$bmi<-703*(project2_dataset_rm_NA$anthroheightcalc)/((project2_dataset_rm_NA$anthroweight_avg)^2)
#project2_dataset_rm_NA$bmi[which(project2_dataset_rm_NA$bmi>36 | project2_dataset_rm_NA$bmi < 11)]=NA; #reset unrealistic values;

### screentime
sqt_weekday<-c('screen1_wkdy_y','screen2_wkdy_y','screen3_wkdy_y','screen4_wkdy_y','screen5_wkdy_y','screen_wkdy_y')
sqt_weekend<-c('screen7_wknd_y','screen8_wknd_y','screen9_wknd_y','screen10_wknd_y','screen11_wknd_y','screen12_wknd_y')
project2_dataset_rm_NA$screen_weekday<-rowSums(project2_dataset_rm_NA[sqt_weekday],na.rm=TRUE)
project2_dataset_rm_NA$screen_weekend<-rowSums(project2_dataset_rm_NA[sqt_weekend],na.rm=TRUE)

### ABCD Youth Pubertal Development Scale and Menstrual Cycle Survey History (PDMS)
total_number=dim(project2_dataset_rm_NA)[1]
ind_male=which(project2_dataset_rm_NA$sex=='M')#separated by sex
ind_female=which(project2_dataset_rm_NA$sex=='F')
project2_dataset_rm_NA$ypdms_male_sum<-NA
project2_dataset_rm_NA$ypdms_male_mean<-NA
project2_dataset_rm_NA$ypdms_sum_y_m<-NA
project2_dataset_rm_NA$ypdms_female_sum<-NA
project2_dataset_rm_NA$ypdms_female_mean<-NA
project2_dataset_rm_NA$ypdms_sum_y_m<-NA
for (indx in 1:total_number) {
  #male
  if (indx %in% ind_male){
    project2_dataset_rm_NA$ypdms_male_sum[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_ht2_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_bdyhair_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_skin2_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_m4_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_m5_y))[indx],na.rm = TRUE)
    project2_dataset_rm_NA$ypdms_male_mean[indx]=mean(project2_dataset_rm_NA$pds_male_sum[indx],na.rm = TRUE)
    project2_dataset_rm_NA$ypdms_sum_y_m[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_bdyhair_y[indx])), as.numeric(unlist(project2_dataset_rm_NA$pds_m4_y[indx])),as.numeric(unlist(project2_dataset_rm_NA$pds_m5_y[indx])),na.rm = TRUE)
  }
  #female
  else{
    project2_dataset_rm_NA$ypdms_female_sum[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_ht2_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_bdyhair_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_skin2_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_f4_2_y))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_f5_y))[indx],na.rm = TRUE)
    project2_dataset_rm_NA$ypdms_female_mean[indx]=mean(project2_dataset_rm_NA$pds_female_sum[indx],na.rm = TRUE)
    project2_dataset_rm_NA$ypdms_sum_y_m[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_bdyhair_y[indx])), as.numeric(unlist(project2_dataset_rm_NA$pds_f4_2_y[indx])),as.numeric(unlist(project2_dataset_rm_NA$pds_f5_y[indx])),na.rm = TRUE)
  }
}
#prepuberty        3
#early puberty  >= 4 and <= 5
#mid puberty    >= 6 and <= 8
#late puberty   >= 9 and <=11
#post puberty      12
puberty.category <- function(s){
  ifelse(s==3,return(1),ifelse(s>= 4 & s<= 5,return(2),ifelse(s>= 6 & s<= 8,return(3),ifelse(s>= 9 & s<= 11,return(4),ifelse(s==12,return(5),return(NA))))))
}
ypdms_cat<-c()
for (i in project2_dataset_rm_NA$ypdms_sum_y_m){
  ypdms_cat<-c(ypdms_cat,puberty.category(as.numeric(unlist(i))))
}
project2_dataset_rm_NA$ypdms_y_ss_cat_sum<-as.factor(ypdms_cat)

### take vitamins(-1 = Not applicable No aplica)
project2_dataset_rm_NA$devhx_10.x[project2_dataset_rm_NA$devhx_10.x==-1] = NA
project2_dataset_rm_NA$devhx_vitamin<-as.factor(project2_dataset_rm_NA$devhx_10.x)

### birth related(-1 = Not applicable No aplica)
project2_dataset_rm_NA$planned_pregnancy<-as.factor(project2_dataset_rm_NA$devhx_6_p.x)#planned_pregnancy
project2_dataset_rm_NA$devhx_preg_awareness_weeks<-project2_dataset_rm_NA$devhx_7_p.x#pregnancy_awareness_weeks
project2_dataset_rm_NA$maternal_age<-project2_dataset_rm_NA$devhx_3_p.x#maternal age
project2_dataset_rm_NA$paternal_age<-project2_dataset_rm_NA$devhx_4_p.x#paternal age
project2_dataset_rm_NA$born_premature<-as.factor(project2_dataset_rm_NA$devhx_12a_p.x)#born_premature 
project2_dataset_rm_NA$born_premature_weeks<-project2_dataset_rm_NA$devhx_12_p.x#born_premature_weeks age
project2_dataset_rm_NA$caesarian_section<-as.factor(project2_dataset_rm_NA$devhx_13_3_p.x)#caesarian_section 
project2_dataset_rm_NA$incubator_days<-project2_dataset_rm_NA$devhx_15.x#For how many days  after birth was he/she in an incubator?
project2_dataset_rm_NA$firstyear_fever_104degrees_days<-project2_dataset_rm_NA$devhx_16_p.x#About how many days  in the first 12 months of life, did he/she have a fever of 104 degrees or greater?
project2_dataset_rm_NA$firstyear_infections_ser_ill_days<-project2_dataset_rm_NA$devhx_17_p.x#About how many days  in the first 12 months of life did he/she have any infections or serious illnesses? 
project2_dataset_rm_NA$breast_fed_months<-project2_dataset_rm_NA$devhx_18_p.x#For how many months  was he/she breast fed? 

### caffine(-1 = Not applicable No aplica)
project2_dataset_rm_NA$devhx_caffeine_11.x[project2_dataset_rm_NA$devhx_caffeine_11.x==-1] = NA
project2_dataset_rm_NA$devhx_caffeine<-as.factor(ifelse(project2_dataset_rm_NA$devhx_caffeine_11.x %in% c(2,3),1,project2_dataset_rm_NA$devhx_caffeine_11.x))
project2_dataset_rm_NA$devhx_caffeine_11<-project2_dataset_rm_NA$devhx_caffeine_11.x
#0 = No; 1 = Yes - less than once a week;2 = Yes - less than once a day but more than once a week; ; 3 = Yes - at least once a day;
project2_dataset_rm_NA$devhx_caffeine_11[project2_dataset_rm_NA$devhx_caffeine_11==1] = 4;
project2_dataset_rm_NA$devhx_caffeine_11[project2_dataset_rm_NA$devhx_caffeine_11==3] = 1;
project2_dataset_rm_NA$devhx_caffeine_11[project2_dataset_rm_NA$devhx_caffeine_11==4] = 3

### obstetric complications (at least 1 condition = present)
obs_comps<-c('devhx_10a3_p.x','devhx_10b3_p.x','devhx_10c3_p.x','devhx_10d3_p.x','devhx_10e3_p.x','devhx_10f3_p.x','devhx_10g3_p.x','devhx_10h3_p.x','devhx_10i3_p.x','devhx_10j3_p.x','devhx_10k3_p.x','devhx_10l3_p.x','devhx_10m3_p.x')
project2_dataset_rm_NA$obsteric_complication<-as.factor(ifelse(rowSums(project2_dataset_rm_NA[obs_comps],na.rm=TRUE)>0,1,0))
names(project2_dataset_rm_NA)[which(c(colnames(project2_dataset_rm_NA)==obs_comps[1])):which(c(colnames(project2_dataset_rm_NA)==obs_comps[13]))]<-c('Severe.nausea.vomiting','Heavy.bleeding','Pre-eclampsia.eclampsia.toxemia','Severe.gall.bladder.attack','Persistent.proteinuria','Rubella','Severe.anemia','UTI','Preg-related.diabetes','Preg-related.HBP','problems.with.the.placenta','accident.or.injury','other.conditions')

###  birth complications (at least 1 condition = present)
birth_comps<-c('devhx_14a3_p.x','devhx_14b3_p.x','devhx_14c3_p.x','devhx_14d3_p.x','devhx_14e3_p.x','devhx_14f3_p.x','devhx_14g3_p.x','devhx_14h3_p.x')
project2_dataset_rm_NA$birth_complication<-as.factor(ifelse(rowSums(project2_dataset_rm_NA[birth_comps],na.rm=TRUE)>0,1,0))
names(project2_dataset_rm_NA)[which(c(colnames(project2_dataset_rm_NA)==birth_comps[1])):which(c(colnames(project2_dataset_rm_NA)==birth_comps[8]))]<-c('Blue.at.birth','slow.heart.beat','not.breathe.at.first','convulsions','Jaundice.needing.treatment','Required.oxygen','Required.blood.transfusion','Rh.incompatibility')

### Pubertal stage and menstrual phase (for postmenarcheal girls) - parent survey
project2_dataset_rm_NA$pds_male_sum<-NA
project2_dataset_rm_NA$pds_male_mean<-NA
project2_dataset_rm_NA$pds_sum_y_m<-NA
project2_dataset_rm_NA$pds_female_sum<-NA
project2_dataset_rm_NA$pds_female_mean<-NA
project2_dataset_rm_NA$pds_sum_y_m<-NA
for (indx in 1:total_number) {
  #male
  if (indx %in% ind_male){
    project2_dataset_rm_NA$pds_male_sum[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_1_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_2_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_3_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_m4_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_m5_p))[indx],na.rm = TRUE)
    project2_dataset_rm_NA$pds_male_mean[indx]=mean(project2_dataset_rm_NA$pds_male_sum[indx],na.rm = TRUE)
    project2_dataset_rm_NA$pds_sum_y_m[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_2_p[indx])), as.numeric(unlist(project2_dataset_rm_NA$pds_m4_p[indx])),as.numeric(unlist(project2_dataset_rm_NA$pds_m5_p[indx])),na.rm = TRUE)
  }
  #female
  else{
    project2_dataset_rm_NA$pds_female_sum[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_1_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_2_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_3_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_f4_p))[indx],as.numeric(unlist(project2_dataset_rm_NA$pds_f5b_p))[indx],na.rm = TRUE)
    project2_dataset_rm_NA$pds_female_mean[indx]=mean(project2_dataset_rm_NA$pds_female_sum[indx],na.rm = TRUE)
    project2_dataset_rm_NA$pds_sum_y_m[indx]=sum(as.numeric(unlist(project2_dataset_rm_NA$pds_2_p[indx])), as.numeric(unlist(project2_dataset_rm_NA$pds_f4_p[indx])),as.numeric(unlist(project2_dataset_rm_NA$pds_f5b_p[indx])),na.rm = TRUE)
  }
}
pds_cat<-c()
for (i in project2_dataset_rm_NA$pds_sum_y_m){
  pds_cat<-c(pds_cat,puberty.category(as.numeric(unlist(i))))
}
project2_dataset_rm_NA$pds_y_ss_cat_sum<-as.factor(pds_cat)

### physical health
project2_dataset_rm_NA$Asthma<-as.factor(project2_dataset_rm_NA$medhx_2a)
project2_dataset_rm_NA$Allergies<-as.factor(project2_dataset_rm_NA$medhx_2b)
project2_dataset_rm_NA$Brain_Injury<-as.factor(project2_dataset_rm_NA$medhx_2c)
project2_dataset_rm_NA$Bronchitis<-as.factor(project2_dataset_rm_NA$medhx_2d)
project2_dataset_rm_NA$Cancer_or_Leukemia<-as.factor(project2_dataset_rm_NA$medhx_2e)
project2_dataset_rm_NA$Cerebral_Palsy<-as.factor(project2_dataset_rm_NA$medhx_2f)
project2_dataset_rm_NA$Diabetes<-as.factor(project2_dataset_rm_NA$medhx_2g)
project2_dataset_rm_NA$Epilepsy_or_Seizures<-as.factor(project2_dataset_rm_NA$medhx_2h)
project2_dataset_rm_NA$Hearing_Problem<-as.factor(project2_dataset_rm_NA$medhx_2i)
project2_dataset_rm_NA$Kidney_Disease<-as.factor(project2_dataset_rm_NA$medhx_2j)
project2_dataset_rm_NA$Lead_Poisoning<-as.factor(project2_dataset_rm_NA$medhx_2k)
project2_dataset_rm_NA$Muscular_Dystrophy_Since_LAST_meeting<-as.factor(project2_dataset_rm_NA$medhx_2l)
project2_dataset_rm_NA$Multiple_Sclerosis<-as.factor(project2_dataset_rm_NA$medhx_2m)
project2_dataset_rm_NA$Problems_with_Vision<-as.factor(project2_dataset_rm_NA$medhx_2n)
project2_dataset_rm_NA$Problems_with_Heart<-as.factor(project2_dataset_rm_NA$medhx_2o)
project2_dataset_rm_NA$Sickle_Cell_Anemia<-as.factor(project2_dataset_rm_NA$medhx_2p)
project2_dataset_rm_NA$Very_Bad_Headaches<-as.factor(project2_dataset_rm_NA$medhx_2q)
project2_dataset_rm_NA$Operation<-as.factor(project2_dataset_rm_NA$medhx_2r)
project2_dataset_rm_NA$Other_Illness<-as.factor(project2_dataset_rm_NA$medhx_2s)
project2_dataset_rm_NA$Broken_Bones<-as.factor(project2_dataset_rm_NA$medhx_6a)
project2_dataset_rm_NA$Sprains<-as.factor(project2_dataset_rm_NA$medhx_6b)
project2_dataset_rm_NA$Cuts_or_Scrapes<-as.factor(project2_dataset_rm_NA$medhx_6c)
project2_dataset_rm_NA$Stitches<-as.factor(project2_dataset_rm_NA$medhx_6d)
project2_dataset_rm_NA$Other_Serious_Wounds<-as.factor(project2_dataset_rm_NA$medhx_6e)
project2_dataset_rm_NA$Falls<-as.factor(project2_dataset_rm_NA$medhx_6f)
project2_dataset_rm_NA$Burns<-as.factor(project2_dataset_rm_NA$medhx_6g)
project2_dataset_rm_NA$High_Fever<-as.factor(project2_dataset_rm_NA$medhx_6h)
project2_dataset_rm_NA$Head_Injury<-as.factor(project2_dataset_rm_NA$medhx_6i)
project2_dataset_rm_NA$Knocked_Unconscious<-as.factor(project2_dataset_rm_NA$medhx_6j)
project2_dataset_rm_NA$Bruises<-as.factor(project2_dataset_rm_NA$medhx_6k)
project2_dataset_rm_NA$Asthma_Attack_Since_LAST_meeting<-as.factor(project2_dataset_rm_NA$medhx_6l)
project2_dataset_rm_NA$Broken_Teeth<-as.factor(project2_dataset_rm_NA$medhx_6m)
project2_dataset_rm_NA$Animal_Bite<-as.factor(project2_dataset_rm_NA$medhx_6n)
project2_dataset_rm_NA$Overdose<-as.factor(project2_dataset_rm_NA$medhx_6o)
project2_dataset_rm_NA$Seizure<-as.factor(project2_dataset_rm_NA$medhx_6p)
project2_dataset_rm_NA$Accidental_Poisoning<-as.factor(project2_dataset_rm_NA$medhx_6q)
project2_dataset_rm_NA$Gun_Shot_Wound<-as.factor(project2_dataset_rm_NA$medhx_6r)
project2_dataset_rm_NA$Wound_from_knife_or_any_other_weapon<-as.factor(project2_dataset_rm_NA$medhx_6s)
project2_dataset_rm_NA$Other_hospitalizations<-as.factor(project2_dataset_rm_NA$medhx_6t)
project2_dataset_rm_NA$ehi_y_ss_scoreb<-as.factor(project2_dataset_rm_NA$ehi_y_ss_scoreb)

### family history problem :substance use and mental health problem
famhx_ss_relative <- read_excel("excel\\famhx_relative.xlsx", col_names = TRUE);
famhx_ss_relative=t(data.frame(famhx_ss_relative));
rownames(famhx_ss_relative)
name_relative_alcohol <- famhx_ss_relative[1,];
name_relative_dg <- famhx_ss_relative[2,];
name_relative_dprs <- famhx_ss_relative[3,];
name_relative_ma <- famhx_ss_relative[4,];
name_relative_vs<- famhx_ss_relative[5,];
name_relative_trb <- famhx_ss_relative[6,];
name_relative_nrv <- famhx_ss_relative[7,];
name_relative_prf <- famhx_ss_relative[8,];
name_relative_hspd <- famhx_ss_relative[9,];
name_relative_scd <- famhx_ss_relative[10,]
project2_dataset_rm_NA$famhx_ss_relative_prob_alc_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_alcohol],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_dg_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_dg],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_dprs_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_dprs],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_ma_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_ma],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_vs_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_vs],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_trb_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_trb],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_nrv_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_nrv],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_prf_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_prf],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_hspd_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_hspd],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$famhx_ss_relative_prob_scd_p<-ifelse(rowSums(project2_dataset_rm_NA[name_relative_scd],na.rm=TRUE)>0,1,0)

### ABCD Youth Substance Use Interview
tlfb_alc_v<-c('tlfb_alc_sip','tlfb_alc_use')
tlfb_tob_v<-c('tlfb_tob_puff','tlfb_cig_use','tlfb_ecig_use','tlfb_chew_use','tlfb_cigar_use','tlfb_hookah_use','tlfb_pipes_use','tlfb_nicotine_use')
tlfb_mj_v<-c('tlfb_mj_puff','tlfb_mj_use','tlfb_blunt_use','tlfb_edible_use','tlfb_mj_conc_use','tlfb_mj_drink_use','tlfb_tincture_use','tlfb_mj_synth_use')
project2_dataset_rm_NA$tlfb_alc<-ifelse(rowSums(project2_dataset_rm_NA[tlfb_alc_v],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$tlfb_tob<-ifelse(rowSums(project2_dataset_rm_NA[tlfb_tob_v],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$tlfb_mj<-ifelse(rowSums(project2_dataset_rm_NA[tlfb_mj_v],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$tlfb_caf<-project2_dataset_rm_NA$caff_max_type
project2_dataset_rm_NA$tlfb_caf[project2_dataset_rm_NA$tlfb_caf>0]=1

### ABCD Parent Community Risk and Protective Factors (CRPF)
su_risk_p<-c('su_risk_p_1','su_risk_p_2','su_risk_p_3','su_risk_p_4','su_risk_p_5','su_risk_p_7','su_risk_p_8','su_risk_p_10','su_risk_p_11','su_risk_p_12','su_risk_p_13')#how hard to get those substances,the higher the score, more risks it has(0 = Very hard ; 1 = Sort of hard ; 2 = Sort of easy ; 3 = Very easy; 4 = Don't know)
project2_dataset_rm_NA[su_risk_p][project2_dataset_rm_NA[su_risk_p] ==4 ] <- NA
project2_dataset_rm_NA$su_risk_p_6[project2_dataset_rm_NA$su_risk_p_6 ==2 ] <- NA;
project2_dataset_rm_NA$su_risk_p_6[project2_dataset_rm_NA$su_risk_p_6 ==0 ] <- 2;
project2_dataset_rm_NA$su_risk_p_6[project2_dataset_rm_NA$su_risk_p_6 ==1 ] <- 0;
project2_dataset_rm_NA$su_risk_p_6[project2_dataset_rm_NA$su_risk_p_6 ==2 ] <- 1
su_risks<-c(su_risk_p,'su_risk_p_6','su_risk_p_9')
project2_dataset_rm_NA$su_risks_Perceived_availability<-rowSums(project2_dataset_rm_NA[su_risks],na.rm=TRUE)

### ABCD Parent Demographics Survey
project2_dataset_rm_NA['demo_prnt_prtnr_v2'][project2_dataset_rm_NA['demo_prnt_prtnr_v2'] ==2 ] <- 0

### Residential History Derived Scores
reshist_pm252016aa<-c('reshist_addr1_pm252016aa','reshist_addr2_pm252016aa','reshist_addr3_pm252016aa')#Residential history derived - annual average of PM 2.5 in 2016 at primary residential address at 1x1km2
reshist_leadrisk<-c('reshist_addr1_leadrisk','reshist_addr2_leadrisk','reshist_addr3_leadrisk')#Estimated lead risk in census tract of tertiary residential address (1-10 scale)
reshist_urban_area<-c('reshist_addr1_urban_area','reshist_addr2_urban_area','reshist_addr3_urban_area')#Census Tract Urban Classification at current address 1 = Rural ; 2 = Urban Clusters ; 3 = Urbanized Area

### ABCD Parent Diagnostic Interview for DSM-5 Full (KSADS-5)
ksad_cat_fle <- read_excel("excel\\ksad_cat.xlsx", col_names = TRUE)
ksad_cat_fle=t(data.frame(ksad_cat_fle))
rownames(ksad_cat_fle)
name_ksad_all <- ksad_cat_fle[1,]
summary(project2_dataset_rm_NA[name_ksad_all])
project2_dataset_rm_NA[name_ksad_all][project2_dataset_rm_NA[name_ksad_all] > 1 ] <- NA#remove na values larger than 1 such as 555,888
project2_dataset_rm_NA$KSADS.Depression<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[3,][complete.cases(ksad_cat_fle[3,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Bipolar.and.Related.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[5,][complete.cases(ksad_cat_fle[5,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Hallucinations<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[13,][complete.cases(ksad_cat_fle[13,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Auditory.Hallucinations<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[11,][complete.cases(ksad_cat_fle[11,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Delusions<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[12,][complete.cases(ksad_cat_fle[12,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Associated.Psychotic.Symptoms<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[10,][complete.cases(ksad_cat_fle[10,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Panic.and.Other.Specified.Anxiety.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[15,][complete.cases(ksad_cat_fle[15,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Agoraphobia.and.Other.Specified.Anxiety.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[17,][complete.cases(ksad_cat_fle[17,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Separation.and.Other.Specified.Anxiety.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[19,][complete.cases(ksad_cat_fle[19,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Social.and.Other.Specified.Anxiety.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[21,][complete.cases(ksad_cat_fle[21,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Specific.Phobia<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[23,][complete.cases(ksad_cat_fle[23,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Generalized.Anxiety.Disorder.and.other.Specified.Anxiety.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[25,][complete.cases(ksad_cat_fle[25,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Obsessive.Compulsive.and.Related.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[27,][complete.cases(ksad_cat_fle[27,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Encopresis<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[29,][complete.cases(ksad_cat_fle[29,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Feeding.or.Eating.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[31,][complete.cases(ksad_cat_fle[31,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Attention.Deficit.Hyperactivity.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[33,][complete.cases(ksad_cat_fle[33,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Oppositional.Defiant.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[35,][complete.cases(ksad_cat_fle[35,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Conduct.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[37,][complete.cases(ksad_cat_fle[37,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Alcohol.Use.and.Alcohol.Related.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[39,][complete.cases(ksad_cat_fle[39,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Substance.Related.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[41,][complete.cases(ksad_cat_fle[41,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.PTSD.and.Other.Specified.Trauma.and.Stressor.Related.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[43,][complete.cases(ksad_cat_fle[43,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Insomnia<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[45,][complete.cases(ksad_cat_fle[45,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.homicidal.ideation.and.behavior<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[47,][complete.cases(ksad_cat_fle[47,])]],na.rm=TRUE)>0,1,0)
project2_dataset_rm_NA$KSADS.Selective.Mutism.and.Other.Specified.Anxiety.Disorder<-ifelse(rowSums(project2_dataset_rm_NA[ksad_cat_fle[49,][complete.cases(ksad_cat_fle[49,])]],na.rm=TRUE)>0,1,0)

### sleep disturbance scale
add_sds_name<-names(project2_dataset_rm_NA)[which(colnames(project2_dataset_rm_NA)=='sleepdisturb1_p'):which(colnames(project2_dataset_rm_NA)=='sleepdisturb26_p')]
sds_dims_name<-add_sds_name[c(1:5,11)];sds_sbd_name<-add_sds_name[13:15];sds_da_name<-add_sds_name[c(17,20,21)];sds_swtd_name<-add_sds_name[c(6:8,12,18,19)];sds_does_name<-add_sds_name[22:26];sds_shy_name<-add_sds_name[c(9,16)];
project2_dataset_rm_NA$sds_dims <- rowSums(project2_dataset_rm_NA[sds_dims_name],na.rm=TRUE)
project2_dataset_rm_NA$sds_sbd <- rowSums(project2_dataset_rm_NA[sds_sbd_name],na.rm=TRUE)
project2_dataset_rm_NA$sds_da <- rowSums(project2_dataset_rm_NA[sds_da_name],na.rm=TRUE)
project2_dataset_rm_NA$sds_swtd <- rowSums(project2_dataset_rm_NA[sds_swtd_name],na.rm=TRUE)
project2_dataset_rm_NA$sds_does <- rowSums(project2_dataset_rm_NA[sds_does_name],na.rm=TRUE)
project2_dataset_rm_NA$sds_shy <- rowSums(project2_dataset_rm_NA[sds_shy_name],na.rm=TRUE)
sds_subscale<-names(project2_dataset_rm_NA)[which(names(project2_dataset_rm_NA)=='sds_dims'):which(names(project2_dataset_rm_NA)=='sds_shy')]
project2_dataset_rm_NA$sds_total<-rowSums(project2_dataset_rm_NA[sds_subscale],na.rm=TRUE)

saveRDS(project2_dataset_rm_NA, file = "project2_dataset_rm_NA_coding.RDS")

# 2.coding dummy and continuous variables -----------------------
setwd('/share/inspurStorage/home1/Lixuan/ABCDR/project2/20220106_add_txt')
project2_dataset_rm_NA<-readRDS("project2_dataset_rm_NA.RDS")
all_variables <- read_excel("excel\\covariates_x2.xlsx",1,col_names = TRUE)
all_variables=t(data.frame(all_variables))
dummy_v<-all_variables['dummy',][complete.cases(all_variables['dummy',])]
name_all<-all_variables['name_variable_in_abcdV4',][complete.cases(all_variables['name_variable_in_abcdV4',])]

ind_dummy<-c()
for(i in 1:length(dummy_v)){
  ind_dummy<-c(ind_dummy,which(names(project2_dataset_rm_NA)==dummy_v[i]))
}
project2_dataset_rm_NA[,ind_dummy]<-lapply(project2_dataset_rm_NA[,ind_dummy],as.factor)
continuous_v<-name_all[!name_all %in% dummy_v]

ind_continuous<-c()
for(i in 1:length(continuous_v)){
  ind_continuous<-c(ind_continuous,which(names(project2_dataset_rm_NA)==continuous_v[i]))
}
name_continuous<-names(project2_dataset_rm_NA)[ind_continuous]
project2_dataset_rm_NA_win<-project2_dataset_rm_NA
for (i in name_continuous){
  project2_dataset_rm_NA_win[i]=winsor(project2_dataset_rm_NA_win[i], trim = 0.01,na.rm=TRUE)
}

### recode dummy variable from T/F to 1/0
project2_dataset_rm_NA_win$alcohol<-ifelse(project2_dataset_rm_NA_win$alcohol==TRUE,1,0)
project2_dataset_rm_NA_win$tobacco<-ifelse(project2_dataset_rm_NA_win$tobacco==TRUE,1,0)
project2_dataset_rm_NA_win$marijuana<-ifelse(project2_dataset_rm_NA_win$marijuana==TRUE,1,0)
project2_dataset_rm_NA_win$married.or.livingtogether<-ifelse(project2_dataset_rm_NA_win$married.or.livingtogether=="yes",1,0)
project2_dataset_rm_NA_win$household.income=ifelse(project2_dataset_rm_NA_win$household.income=="[<50K]",1,ifelse(project2_dataset_rm_NA_win$household.income=="[>=50K & <100K]",2,3))
project2_dataset_rm_NA_win$high.educ=as.numeric(project2_dataset_rm_NA_win$high.edu)
project2_dataset_rm_NA_win$planned_pregnancy<-ifelse(project2_dataset_rm_NA_win$planned_pregnancy==1,1,0)
project2_dataset_rm_NA_win$obsteric_complication<-ifelse(project2_dataset_rm_NA_win$obsteric_complication==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_alc_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_alc_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_alc_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_alc_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_alc_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_alc_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_dg_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_dg_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_dg_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_dg_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_dg_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_dg_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_dprs_p<-ifelse(project2_dataset_rm_NA_win$obsteric_complication==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_dprs_p<-ifelse(project2_dataset_rm_NA_win$obsteric_complication==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_dprs_p<-ifelse(project2_dataset_rm_NA_win$obsteric_complication==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_ma_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_ma_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_ma_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_ma_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_ma_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_ma_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_hspd_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_hspd_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_hspd_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_hspd_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_hspd_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_hspd_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_vs_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_vs_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_vs_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_vs_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_vs_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_vs_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_trb_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_trb_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_trb_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_trb_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_trb_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_trb_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_nrv_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_nrv_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_nrv_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_nrv_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_nrv_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_nrv_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_prf_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_prf_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_prf_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_prf_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_prf_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_prf_p==1,1,0)

project2_dataset_rm_NA_win$famhx_ss_fath_prob_scd_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_fath_prob_scd_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_moth_prob_scd_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_moth_prob_scd_p==1,1,0)
project2_dataset_rm_NA_win$famhx_ss_relative_prob_scd_p<-ifelse(project2_dataset_rm_NA_win$famhx_ss_relative_prob_scd_p==1,1,0)


setwd('data\\')
saveRDS(project2_dataset_rm_NA_win, file = "project2_dataset_rm_NA_win.RDS")

# 3.add_prs -----------------------
project2_dataset_rm_NA_win_prs <- project2_dataset_rm_NA_win
setwd_list <- c('data\\adhd2019','data\\scz2022','data\\mdd2018')
p.threshold <- c(0.001,0.05,0.1,0.2,0.3,0.4,0.5)
# Read in the PCs
pcs <- read.table("data\\scz2022\\ABCD.eigenvec", header=F);
colnames(pcs) <- c("FID", "src_subject_id", paste0("prs_PC",1:15))
project2_dataset_rm_NA_win_prs <- left_join(project2_dataset_rm_NA_win_prs, pcs, by="src_subject_id")
for(i in setwd_list){
  setwd(i)
  # Read in the covariates
  for(j in p.threshold){
    prs <- read.table(paste0("ABCD.",j,".profile"), header=T)
    if(i==setwd_list[1]) {
      colnames(prs) <- c("FID","src_subject_id","PHENO","CNT","CNT2",paste0("adhd_SCORE_",j))
      project2_dataset_rm_NA_win_prs <- left_join(project2_dataset_rm_NA_win_prs, prs[,c("src_subject_id",paste0("adhd_SCORE_",j))], by="src_subject_id")
    }
    if(i==setwd_list[2]) {
      colnames(prs) <- c("FID","src_subject_id","PHENO","CNT","CNT2",paste0("scz_SCORE_",j))
      project2_dataset_rm_NA_win_prs <- left_join(project2_dataset_rm_NA_win_prs, prs[,c("src_subject_id",paste0("scz_SCORE_",j))], by="src_subject_id")
    }
    if(i==setwd_list[3]) {
      colnames(prs) <- c("FID","src_subject_id","PHENO","CNT","CNT2",paste0("mdd_SCORE_",j))
      project2_dataset_rm_NA_win_prs <- left_join(project2_dataset_rm_NA_win_prs, prs[,c("src_subject_id",paste0("mdd_SCORE_",j))], by="src_subject_id")
    }
  }
}
adhd_name<-c('adhd_SCORE_0.001','adhd_SCORE_0.05','adhd_SCORE_0.1','adhd_SCORE_0.2','adhd_SCORE_0.3','adhd_SCORE_0.4','adhd_SCORE_0.5')
scz_name<-c('scz_SCORE_0.001','scz_SCORE_0.05','scz_SCORE_0.1','scz_SCORE_0.2','scz_SCORE_0.3','scz_SCORE_0.4','scz_SCORE_0.5')
mdd_name<-c('mdd_SCORE_0.001','mdd_SCORE_0.05','mdd_SCORE_0.1','mdd_SCORE_0.2','mdd_SCORE_0.3','mdd_SCORE_0.4','mdd_SCORE_0.5')
project2_dataset_rm_NA_win_prs$adhd_SCORE_avg<-rowMeans(project2_dataset_rm_NA_win_prs[adhd_name],na.rm=TRUE)
project2_dataset_rm_NA_win_prs$scz_SCORE_avg<-rowMeans(project2_dataset_rm_NA_win_prs[scz_name],na.rm=TRUE)
project2_dataset_rm_NA_win_prs$mdd_SCORE_avg<-rowMeans(project2_dataset_rm_NA_win_prs[mdd_name],na.rm=TRUE)

setwd('data\\')
saveRDS(project2_dataset_rm_NA_win_prs, file = "project2_dataset_rm_NA_win_prs.RDS")
project2_dataset_baseline_rm_NA_win_prs=project2_dataset_rm_NA_win_prs[project2_dataset_rm_NA_win_prs$eventname == "baseline_year_1_arm_1",]
saveRDS(project2_dataset_baseline_rm_NA_win_prs,file = "project2_dataset_baseline_rm_NA_win_prs.RDS")
project2_dataset_1year_rm_NA_win_prs=project2_dataset_rm_NA_win_prs[project2_dataset_rm_NA_win_prs$eventname == "1_year_follow_up_y_arm_1",]
saveRDS(project2_dataset_1year_rm_NA_win_prs,file = "project2_dataset_1year_rm_NA_win_prs.RDS")
project2_dataset_2year_rm_NA_win_prs=project2_dataset_rm_NA_win_prs[project2_dataset_rm_NA_win_prs$eventname == "2_year_follow_up_y_arm_1",]
saveRDS(project2_dataset_2year_rm_NA_win_prs,file = "project2_dataset_2year_rm_NA_win_prs.RDS")
project2_dataset_3year_rm_NA_win_prs=project2_dataset_rm_NA_win_prs[project2_dataset_rm_NA_win_prs$eventname == "3_year_follow_up_y_arm_1",]
saveRDS(project2_dataset_3year_rm_NA_win_prs,file = "project2_dataset_3year_rm_NA_win_prs.RDS")


