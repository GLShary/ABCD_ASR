rm(list=ls())
gc()

## Load libraries ####
library(data.table)
library(dplyr)
library(readxl)
library(pheatmap)
library(lmerTest)
library(robustHD)

setwd('data\\')
dataset<-readRDS("dataset4.0.Rds")

#delete subject who has NA in baseline 
dataset<-dataset[-39768,]#this subject is baseline data, but has too much NA and doesn't in order 
dataset_baseline<-dataset[dataset$eventname == "baseline_year_1_arm_1",]

## exclude missing data ####
exclusion <- read_excel("excel\\preprocessing2.xlsx", col_names = TRUE)
exclusion=t(data.frame(exclusion))
ind_exclu<-c()
for(i in 1:length(exclusion['exclusion_project2',])){
  ind_exclu = c(ind_exclu,c(which(names(dataset)==exclusion['exclusion_project2',][i])))
}

ind_del<-c()
for(i in ind_exclu){
  ind_del<-c(ind_del,which(is.na(dataset_baseline[,i])))#index of subject in baseline data
}  
ind_del<-c(ind_del,which(dataset_baseline$imgincl_t1w_include==0))#index of subject which don't pass QC in baseline data
ind_del<-ind_del[!c(duplicated(ind_del))]#delete index of duplicated subjects
ind_del_subject_id<-dataset$src_subject_id[ind_del]#subject id needed to be deleted

ind_del_row<-c()
for(i in ind_del_subject_id){
  ind_del_row<-c(ind_del_row,which(dataset$src_subject_id==i))#find all data(including baseline&follow up) matching the subject id
}
ind_del_row<-ind_del_row[!c(duplicated(ind_del_row))]
dataset<-dataset[-ind_del_row,]
dataset_baseline<-dataset[dataset$eventname == "baseline_year_1_arm_1",]

setwd('data\\')
saveRDS(dataset,file = "project2_dataset_rm_NA.RDS")