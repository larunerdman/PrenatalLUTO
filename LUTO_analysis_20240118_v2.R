library(readxl)
library(dttr2)
library(randomForest)
library(Metrics)
library(cutpointr)


### PREP ANALYSIS DATA

## MAIN DF
in_df = read_excel("C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/LUTO prediction data_20230814_v10-1_20240118.xlsx", sheet = 1)
str(in_df)
dim(in_df)

in_df$prenatal_diagnosis_msh
table(in_df$category)

# in_df$Bilateral_HN = factor(in_df$`Hydro side`, levels = c(0,1,2), labels = c("None","Unilateral","Bilateral"))

# in_df$AmnioticFluid = factor(in_df$AmnioticFluid,levels = 1:5)

unique(in_df$`genetic anomaly`)
str(in_df$`genetic anomaly`)

in_df$`normal amniotic fluid` = as.numeric(in_df$`normal amniotic fluid`)

in_df$intervention = in_df$interven_yn

in_df$urine_normal = as.numeric(in_df$`normal urine analysis`)
in_df$urine_normal[is.na(in_df$urine_normal)] = 0

in_df$lt_hn = factor(in_df$left_hn, levels = 1:2)
in_df$rt_hn = factor(in_df$right_hn, levels = 1:2)

in_df$afi_level = as.numeric(in_df$afi_level)

in_df$rt_renalp_diameter = as.numeric(in_df$rt_renalp_diameter)

in_df$lt_renalp_diameter = as.numeric(in_df$lt_renalp_diameter)

in_df$mean_rt_length = as.numeric(in_df$mean_rt_length)

in_df$mean_lt_length = as.numeric(in_df$mean_lt_length)

in_df$bladderwall_thickness = as.numeric(in_df$bladderwall_thickness)

in_df$bladderwall_thickening_yn[in_df$bladderwall_thickening_yn == 3] = NA
in_df$bladderwall_thickening_yn = factor(in_df$bladderwall_thickening_yn, c(1,2))

in_df$GAatDiagnosis = in_df$`GAat Diagnosis`
in_df$GAInitial = in_df$`GA initial US MSH round`

in_df$MeanKidneyVol = in_df$MeanKidneyVol

in_df$rt_hydroureter = in_df$rt_hydroureter

in_df$lt_hydroureter = in_df$lt_hydroureter

in_df$keyhole_sign = as.numeric(in_df$keyhole_sign)

in_df$rt_echogenicity = as.numeric(in_df$rt_echogenicity)

in_df$lt_echogenicity = as.numeric(in_df$lt_echogenicity)

in_df$Megacystis = factor(in_df$Megacystis, levels = c(0,1))

in_df$Ascites = as.numeric(in_df$Ascites)

in_df$Female = ifelse(in_df$gender == 1, 0, 1)

in_df$dialysis
table(in_df$dialysis)
table(in_df$dialysis, in_df$category)

in_df$maxvertpocket = as.numeric(in_df$maxvertpocket)

in_df$Diversions
in_df$diversions = ifelse(as.numeric(in_df$Diversions) > 0, 1, 0)
# in_df$Diversions = ifelse(in_df$diversions > 0, 1, 0)
table(in_df$Diversions)
table(in_df$Diversions, in_df$category)
table(in_df$diversions, in_df$Diversions)
table(in_df$diversions, in_df$category)


in_df$transplantation = as.numeric(in_df$transplantation)
table(in_df$transplantation)

# in_df$rrt = in_df$RRT
in_df$RRT = NA
in_df$RRT[in_df$transplantation == 0] = 0
in_df$RRT[in_df$dialysis == 0] = 0
in_df$RRT[in_df$transplantation == 1] = 1

in_df$RRT[in_df$dialysis == 1] = 1
table(in_df$RRT)
# table(in_df$RRT, in_df$rrt)

in_df$InitialUSDays = in_df$GAInitial*7

# in_df$InitialUSDays = sapply(in_df$`GA exact 1st visit`, function(x){
#   
#   wk_day = as.numeric(strsplit(x = as.character(x),split = "\\.")[[1]])
#   
#   days = wk_day[1]*7 + wk_day[2]
#   
#   return(days)
#   
# })


in_df$death = ifelse(in_df$category %in% c(3,4,5,6,7),1,0)
table(in_df$death, in_df$category)
# in_df$death = in_df$Death
table(in_df$death)
table(in_df$diversions)
table(in_df$dialysis)
table(in_df$transplantation)
table(in_df$RRT)

# in_df_ga = in_df[in_df$`GA initial US MSH round` >= 13,]
in_df_ga = in_df[in_df$`GA initial US MSH round` >= 13 && in_df$`GA initial US MSH round` <= 26,]
# in_df_ga = in_df[in_df$`GA US exact` >= 13 && in_df$`GA US exact` <= 26,]
# in_df_ga = in_df[in_df$`GA US exact` >= 13,]

table(in_df_ga$death)
table(in_df_ga$diversions)
table(in_df_ga$dialysis)
table(in_df_ga$transplantation)
table(in_df_ga$RRT)

dim(in_df)
dim(in_df_ga)

table(in_df_ga$death, in_df_ga$category)
table(in_df_ga$diversions, in_df_ga$category)
table(in_df_ga$dialysis, in_df_ga$category)

table(in_df_ga$death)


in_df_lutos = in_df_ga
# in_df_lutos = in_df_ga[in_df_ga$prenatal_diagnosis_msh %in% c(1,2,10),]
# in_df_ga$prenatal_diagnosis_msh[!(in_df_ga$record_id %in% in_df_lutos)]
dim(in_df_lutos)

#### BEST MODEL FILTERS, ONLY USE THESE: 
in_df_lutos_born = in_df_lutos[in_df_lutos$category %in% c(1,3,4,5,6,7,8,9,11,12),]
in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$prenatal_diagnosis_msh %in% c(1,10,11,12),]
in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$record_id != 23,]



# in_df_lutos_born = in_df_lutos[in_df_lutos$category != 2,]
# dim(in_df_lutos_born)
# in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$category != 8,]
# in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$category != 9,]
# in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$category != 11,]
# in_df_lutos_born = in_df_lutos[in_df_lutos$category %in% c(1,3,4,5,6,7,12),]

# in_df_lutos_born = in_df_lutos[in_df_lutos$category %in% c(1,3,4,5,6,7,8,9,11),]
# in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$prenatal_diagnosis_msh %in% c(1,10,11,12),]
# in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$postnatal_diagnosis %in% c(1,3,4,5,6,15,21,25,27),]

# in_df_lutos_born = in_df_lutos_born[in_df_lutos_born$record_id != 23,]
dim(in_df_lutos_born)

table(in_df_lutos_born$death)
table(in_df_lutos_born$diversions)
table(in_df_lutos_born$dialysis)
table(in_df_lutos_born$transplantation)
table(in_df_lutos_born$RRT)


## OTHER DF
# in_df2 = read_excel("C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/Clean Data CAKUT.xlsx", sheet = "1st US features")



### merging

merged_in_df = in_df_lutos_born

str(merged_in_df)

write.csv(merged_in_df$record_id[!is.na(merged_in_df$transplantation)],
          file = "C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/TransplantIDs_20240118.csv", row.names = FALSE)

write.csv(merged_in_df$record_id[!is.na(merged_in_df$Diversions)],
          file = "C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/DiversionsIDs_20240118.csv", row.names = FALSE)

write.csv(merged_in_df$record_id[!is.na(merged_in_df$dialysis)],
          file = "C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/DiaylysisIDs_20240118.csv", row.names = FALSE)

write.csv(merged_in_df$record_id[!is.na(merged_in_df$death)],
          file = "C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/DeathIDs_20240118.csv", row.names = FALSE)

write.csv(merged_in_df$record_id[!is.na(merged_in_df$RRT)],
          file = "C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/RRTIDs_20240118.csv", row.names = FALSE)


merged_anal_df = merged_in_df[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                 "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",#"urine_normal",
                                 "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"intervention", "GAInitial",
                                 "lt_echogenicity","Megacystis","Ascites","Female","RRT","diversions","dialysis","death", "genetic anomaly",
                                 "transplantation","InitialUSDays")]

table(merged_anal_df$diversions)
table(merged_anal_df$transplantation)
table(merged_anal_df$dialysis)
table(merged_anal_df$death)
table(merged_anal_df$RRT)

# preanal_in_df$record_id[preanal_in_df$Transplant == 1]
# 
# merged_anal_df = merge(preanal_in_df2, preanal_in_df,by.x = "Study ID",by.y = "record_id")
# 
# str(merged_anal_df)
# 
# merged_anal_df$`Study ID`[merged_anal_df$Transplant == 1]

## subset to ga first US within 13-26 weeks 
## first recode weeks + days into days

# hist(merged_anal_df$ga_us_totaldays)
# summary(merged_anal_df$ga_us_totaldays)

merged_anal_sub = merged_anal_df

## Check column to use
# merged_anal_sub = merged_anal_df[merged_anal_df$InitialUSDays > 90 & merged_anal_df$InitialUSDays < 183,]

# merged_anal_sub = merged_anal_sub[!is.na(merged_anal_sub$record_id),]

# merged_anal_sub[merged_anal_sub$transplant == 1,]

# table(in_df$RRT)
# table(merged_anal_df$RRT)
# table(merged_in_df$RRT)
# 
# merged_in_df[,c("record_id","RRT")]
# 
# data.frame(in_df[,c("record_id","RRT")])

##################
##**************##
## Random forest
##**************##
##################


names(merged_anal_sub)

### DEATH

merged_anal_sub_rf_death = merged_anal_sub[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                              "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",#"urine_normal",
                                              "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"GAInitial",
                                              "lt_echogenicity","Megacystis","Ascites", "death",#"intervention",
                                              "InitialUSDays","genetic anomaly")]

str(merged_anal_sub_rf_death)
merged_anal_sub_rf_death$death = factor(merged_anal_sub_rf_death$death, levels = c(0,1))
merged_anal_sub_rf_death = data.frame(merged_anal_sub_rf_death[!is.na(merged_anal_sub_rf_death$death),])
table(merged_anal_sub_rf_death$death)

rf_death = randomForest(death ~ ., merged_anal_sub_rf_death,na.action = na.roughfix,importance=TRUE)
death_importance = rf_death$importance[order(rf_death$importance[,2]),]

# rf_div = randomForest(Diversion ~ ., merged_anal_sub_rf_div)

loo_death_pred = c()
loo_death_pred_prob = c()

set.seed(1234)
for(i in 1:nrow(merged_anal_sub_rf_death)){
  fit_df = merged_anal_sub_rf_death[-i,]
  pred_df = merged_anal_sub_rf_death[i,]
  
  fit_df = fit_df[,!(1:ncol(fit_df) %in% which(is.na(pred_df)))]
  
  rf_loo_death_fit = randomForest(death ~ ., data = fit_df,na.action = na.roughfix)
  
  my_pred = predict(rf_loo_death_fit, newdata = pred_df)
  my_pred_prob = predict(rf_loo_death_fit, newdata = pred_df, type = "prob")
  
  loo_death_pred = c(loo_death_pred, my_pred)
  loo_death_pred_prob = c(loo_death_pred_prob, my_pred_prob[2])
}

table(merged_anal_sub_rf_death$death,loo_death_pred)

merged_anal_sub_rf_death$pred_prob = loo_death_pred_prob

cp_death = cutpointr(merged_anal_sub_rf_death, pred_prob, death, method = maximize_metric,
                     metric = sum_sens_spec)

## accuracy
sum(diag(table(merged_anal_sub_rf_death$death,loo_death_pred)))/sum(table(merged_anal_sub_rf_death$death,loo_death_pred))

## AUROC
Metrics::auc(merged_anal_sub_rf_death$death,loo_death_pred_prob)


### DIVERSION 

merged_anal_sub_rf_div = merged_anal_sub[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                            "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",
                                            "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"urine_normal",
                                            "lt_echogenicity","Megacystis","Ascites","Female", "diversions",#"intervention", "GAInitial",
                                            "InitialUSDays","genetic anomaly")]

str(merged_anal_sub_rf_div)
merged_anal_sub_rf_div$diversions = factor(merged_anal_sub_rf_div$diversions, levels = c(0,1))
merged_anal_sub_rf_div = data.frame(merged_anal_sub_rf_div[!is.na(merged_anal_sub_rf_div$diversions),])
table(merged_anal_sub_rf_div$diversions)

rf_div = randomForest(diversions ~ ., merged_anal_sub_rf_div,na.action = na.roughfix,importance=TRUE)
div_importance = rf_div$importance[order(rf_div$importance[,2]),]


# rf_div = randomForest(Diversion ~ ., merged_anal_sub_rf_div)

loo_div_pred = c()
loo_div_pred_prob = c()

set.seed(1234)
for(i in 1:nrow(merged_anal_sub_rf_div)){
  fit_df = merged_anal_sub_rf_div[-i,]
  pred_df = merged_anal_sub_rf_div[i,]
  
  fit_df = fit_df[,!(1:ncol(fit_df) %in% which(is.na(pred_df)))]
  
  
  rf_loo_div_fit = randomForest(diversions ~ ., data = fit_df,na.action = na.roughfix)
  
  my_pred = predict(rf_loo_div_fit, newdata = pred_df)
  my_pred_prob = predict(rf_loo_div_fit, newdata = pred_df, type = "prob")
  
  loo_div_pred = c(loo_div_pred, my_pred)
  loo_div_pred_prob = c(loo_div_pred_prob, my_pred_prob[2])
}


merged_anal_sub_rf_div$pred_prob = loo_div_pred_prob

cp_div = cutpointr(merged_anal_sub_rf_div, pred_prob, diversions, method = maximize_metric,
                   metric = sum_sens_spec)

table(merged_anal_sub_rf_div$diversions,loo_div_pred)

## accuracy
sum(diag(table(merged_anal_sub_rf_div$diversions,loo_div_pred)))/sum(table(merged_anal_sub_rf_div$diversions,loo_div_pred))

## AUROC
Metrics::auc(merged_anal_sub_rf_div$diversions,loo_div_pred_prob)


### RRT
merged_anal_sub_rf_rrt = merged_anal_sub[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                            "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",#"urine_normal",
                                            "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"intervention","GAInitial",
                                            "lt_echogenicity","Megacystis","Ascites","Female","RRT","InitialUSDays","genetic anomaly")]

merged_anal_sub_rf_rrt = data.frame(merged_anal_sub_rf_rrt[!is.na(merged_anal_sub_rf_rrt$RRT),])
merged_anal_sub_rf_rrt$RRT = factor(merged_anal_sub_rf_rrt$RRT, levels = c(0,1))
table(merged_anal_sub_rf_rrt$RRT)

rf_rrt = randomForest(RRT ~ ., merged_anal_sub_rf_rrt,na.action = na.roughfix,importance=TRUE)
rrt_importance = rf_rrt$importance[order(rf_rrt$importance[,4]),]


loo_rrt_pred = c()
loo_rrt_pred_prob = c()

set.seed(1234)
for(i in 1:nrow(merged_anal_sub_rf_rrt)){
  fit_df = merged_anal_sub_rf_rrt[-i,]
  pred_df = merged_anal_sub_rf_rrt[i,]
  
  fit_df = fit_df[,!(1:ncol(fit_df) %in% which(is.na(pred_df)))]
  
  
  rf_loo_rrt_fit = randomForest(RRT ~ ., fit_df,na.action = na.roughfix)
  
  my_pred = predict(rf_loo_rrt_fit, newdata = pred_df)
  my_pred_prob = predict(rf_loo_rrt_fit, newdata = pred_df, type = "prob")
  
  loo_rrt_pred = c(loo_rrt_pred, my_pred)
  loo_rrt_pred_prob = c(loo_rrt_pred_prob, my_pred_prob[2])
}

merged_anal_sub_rf_rrt$pred_prob = loo_rrt_pred_prob

cp_rrt = cutpointr(merged_anal_sub_rf_rrt, pred_prob, RRT, method = maximize_metric,
                   metric = sum_sens_spec)

table(merged_anal_sub_rf_rrt$RRT,loo_rrt_pred)

## accuracy
sum(diag(table(merged_anal_sub_rf_rrt$RRT,loo_rrt_pred)))/sum(table(merged_anal_sub_rf_rrt$RRT,loo_rrt_pred))

## AUROC
Metrics::auc(merged_anal_sub_rf_rrt$RRT,loo_rrt_pred_prob)



### DIALYSIS

merged_anal_sub_rf_dia = merged_anal_sub[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                            "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",#"urine_normal",
                                            "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"intervention","GAInitial",
                                            "lt_echogenicity","Megacystis","Ascites","Female","dialysis","InitialUSDays","genetic anomaly")]

merged_anal_sub_rf_dia = data.frame(merged_anal_sub_rf_dia[!is.na(merged_anal_sub_rf_dia$dialysis),])
merged_anal_sub_rf_dia$dialysis = factor(merged_anal_sub_rf_dia$dialysis, levels = c(0,1))
table(merged_anal_sub_rf_dia$dialysis)

rf_dia = randomForest(dialysis ~ ., merged_anal_sub_rf_dia,na.action = na.roughfix,importance=TRUE)
dia_importance = rf_dia$importance[order(rf_dia$importance[,4]),]


loo_dia_pred = c()
loo_dia_pred_prob = c()

set.seed(1234)
for(i in 1:nrow(merged_anal_sub_rf_dia)){
  fit_df = merged_anal_sub_rf_dia[-i,]
  pred_df = merged_anal_sub_rf_dia[i,]
  
  fit_df = fit_df[,!(1:ncol(fit_df) %in% which(is.na(pred_df)))]
  
  
  rf_loo_dia_fit = randomForest(dialysis ~ ., fit_df,na.action = na.roughfix)
  
  my_pred = predict(rf_loo_dia_fit, newdata = pred_df)
  my_pred_prob = predict(rf_loo_dia_fit, newdata = pred_df, type = "prob")
  
  loo_dia_pred = c(loo_dia_pred, my_pred)
  loo_dia_pred_prob = c(loo_dia_pred_prob, my_pred_prob[2])
  
}

merged_anal_sub_rf_dia$pred_prob = loo_dia_pred_prob

cp_dia = cutpointr(merged_anal_sub_rf_dia, pred_prob, dialysis, method = maximize_metric,
                   metric = sum_sens_spec)

table(merged_anal_sub_rf_dia$dialysis,loo_dia_pred)

## accuracy
sum(diag(table(merged_anal_sub_rf_dia$dialysis,loo_dia_pred)))/sum(table(merged_anal_sub_rf_dia$dialysis,loo_dia_pred))

## AUROC
Metrics::auc(merged_anal_sub_rf_dia$dialysis,loo_dia_pred_prob)


##### TRANSPLANT

merged_anal_sub_rf_trans = merged_anal_sub[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                              "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",#"urine_normal",
                                              "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"intervention","GAInitial",
                                              "lt_echogenicity","Megacystis","Ascites","Female","transplantation","InitialUSDays","genetic anomaly")]


merged_anal_sub_rf_trans = data.frame(merged_anal_sub_rf_trans[!is.na(merged_anal_sub_rf_trans$transplantation),])
str(merged_anal_sub_rf_trans)
merged_anal_sub_rf_trans$transplantation = factor(merged_anal_sub_rf_trans$transplantation, levels = c(0,1))
table(merged_anal_sub_rf_trans$transplantation)

rf_trans = randomForest(transplantation ~ ., merged_anal_sub_rf_trans,na.action = na.roughfix,importance=TRUE)
trans_importance = rf_trans$importance[order(rf_trans$importance[,4]),]


loo_trans_pred = c()
loo_trans_pred_prob = c()

set.seed(1234)
for(i in 1:nrow(merged_anal_sub_rf_trans)){
  fit_df = merged_anal_sub_rf_trans[-i,]
  pred_df = merged_anal_sub_rf_trans[i,]
  
  fit_df = fit_df[,!(1:ncol(fit_df) %in% which(is.na(pred_df)))]
  
  
  rf_loo_trans_fit = randomForest(transplantation ~ ., fit_df,na.action = na.roughfix)
  
  my_pred = predict(rf_loo_trans_fit, newdata = pred_df)
  my_pred_prob = predict(rf_loo_trans_fit, newdata = pred_df, type = "prob")
  
  loo_trans_pred = c(loo_trans_pred, my_pred)
  loo_trans_pred_prob = c(loo_trans_pred_prob, my_pred_prob[2])
}

merged_anal_sub_rf_trans$pred_prob = loo_trans_pred_prob

cp_trans = cutpointr(merged_anal_sub_rf_trans, pred_prob, transplantation, method = maximize_metric,
                     metric = sum_sens_spec)

table(merged_anal_sub_rf_trans$transplantation,loo_trans_pred)

## accuracy
sum(diag(table(merged_anal_sub_rf_trans$transplantation,loo_trans_pred)))/sum(table(merged_anal_sub_rf_trans$transplantation,loo_trans_pred))

## AUROC
Metrics::auc(merged_anal_sub_rf_trans$transplantation,loo_trans_pred_prob)


########################
###******************###
## Overall models' performance
###******************###
########################

## DEATH
acc_death = (sum(as.numeric(as.character(merged_anal_sub_rf_death$death[loo_death_pred_prob >= cp_death$optimal_cutpoint]))) + 
               sum(1 - as.numeric(as.character(merged_anal_sub_rf_death$death[loo_death_pred_prob < cp_death$optimal_cutpoint]))))/
  length(merged_anal_sub_rf_death$death)
auroc_death = round(Metrics::auc(merged_anal_sub_rf_death$death,loo_death_pred_prob),2)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[2,2]/(table(merged_anal_sub_rf_div$diversions)[2])
sens_death = round(sum(as.numeric(as.character(merged_anal_sub_rf_death$death[loo_death_pred_prob >= cp_death$optimal_cutpoint])))/
                     sum(as.numeric(as.character(merged_anal_sub_rf_death$death))),2)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[1,1]/(table(merged_anal_sub_rf_div$diversions)[1])
spec_death = round(sum(1 - as.numeric(as.character(merged_anal_sub_rf_death$death[loo_death_pred_prob < cp_death$optimal_cutpoint])))/
                     sum(1 - as.numeric(as.character(merged_anal_sub_rf_death$death))),2)

## DIVERSIONS
acc_div = (sum(as.numeric(as.character(merged_anal_sub_rf_div$diversions[loo_div_pred_prob >= cp_div$optimal_cutpoint]))) + 
             sum(1 - as.numeric(as.character(merged_anal_sub_rf_div$diversions[loo_div_pred_prob < cp_div$optimal_cutpoint]))))/
  length(merged_anal_sub_rf_div$diversions)
auroc_div = Metrics::auc(merged_anal_sub_rf_div$diversions,loo_div_pred_prob)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[2,2]/(table(merged_anal_sub_rf_div$diversions)[2])
sens_div = sum(as.numeric(as.character(merged_anal_sub_rf_div$diversions[loo_div_pred_prob >= cp_div$optimal_cutpoint])))/
  sum(as.numeric(as.character(merged_anal_sub_rf_div$diversions)))
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[1,1]/(table(merged_anal_sub_rf_div$diversions)[1])
spec_div = sum(1 - as.numeric(as.character(merged_anal_sub_rf_div$diversions[loo_div_pred_prob < cp_div$optimal_cutpoint])))/
  sum(1 - as.numeric(as.character(merged_anal_sub_rf_div$diversions)))

## RRT
acc_rrt = (sum(as.numeric(as.character(merged_anal_sub_rf_rrt$RRT[loo_rrt_pred_prob >= cp_rrt$optimal_cutpoint]))) + 
             sum(1 - as.numeric(as.character(merged_anal_sub_rf_rrt$RRT[loo_rrt_pred_prob < cp_rrt$optimal_cutpoint]))))/
  length(merged_anal_sub_rf_rrt$RRT)
auroc_rrt = round(Metrics::auc(merged_anal_sub_rf_rrt$RRT,loo_rrt_pred_prob),2)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[2,2]/(table(merged_anal_sub_rf_div$diversions)[2])
sens_rrt = sum(as.numeric(as.character(merged_anal_sub_rf_rrt$RRT[loo_rrt_pred_prob >= cp_rrt$optimal_cutpoint])))/
  sum(as.numeric(as.character(merged_anal_sub_rf_rrt$RRT)))
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[1,1]/(table(merged_anal_sub_rf_div$diversions)[1])
spec_rrt = round(sum(1 - as.numeric(as.character(merged_anal_sub_rf_rrt$RRT[loo_rrt_pred_prob < cp_rrt$optimal_cutpoint])))/
                   sum(1 - as.numeric(as.character(merged_anal_sub_rf_rrt$RRT))),2)

## Dialysis
acc_dia = (sum(as.numeric(as.character(merged_anal_sub_rf_dia$dialysis[loo_dia_pred_prob >= cp_dia$optimal_cutpoint]))) + 
             sum(1 - as.numeric(as.character(merged_anal_sub_rf_dia$dialysis[loo_dia_pred_prob < cp_dia$optimal_cutpoint]))))/
  length(merged_anal_sub_rf_dia$dialysis)
auroc_dia = round(Metrics::auc(merged_anal_sub_rf_dia$dialysis,loo_dia_pred_prob),2)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[2,2]/(table(merged_anal_sub_rf_div$diversions)[2])
sens_dia = sum(as.numeric(as.character(merged_anal_sub_rf_dia$dialysis[loo_dia_pred_prob >= cp_dia$optimal_cutpoint])))/
  sum(as.numeric(as.character(merged_anal_sub_rf_dia$dialysis)))
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[1,1]/(table(merged_anal_sub_rf_div$diversions)[1])
spec_dia = round(sum(1 - as.numeric(as.character(merged_anal_sub_rf_dia$dialysis[loo_dia_pred_prob < cp_dia$optimal_cutpoint])))/
                   sum(1 - as.numeric(as.character(merged_anal_sub_rf_dia$dialysis))),2)

## Transplant
acc_trans = (sum(as.numeric(as.character(merged_anal_sub_rf_trans$transplantation[loo_trans_pred_prob >= cp_trans$optimal_cutpoint]))) + 
               sum(1 - as.numeric(as.character(merged_anal_sub_rf_trans$transplantation[loo_trans_pred_prob < cp_trans$optimal_cutpoint]))))/
  length(merged_anal_sub_rf_trans$transplantation)
auroc_trans = round(Metrics::auc(merged_anal_sub_rf_trans$transplantation,loo_trans_pred_prob),2)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[2,2]/(table(merged_anal_sub_rf_div$diversions)[2])
sens_trans = round(sum(as.numeric(as.character(merged_anal_sub_rf_trans$transplantation[loo_trans_pred_prob >= cp_trans$optimal_cutpoint])))/
                     sum(as.numeric(as.character(merged_anal_sub_rf_trans$transplantation))),2)
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)[1,1]/(table(merged_anal_sub_rf_div$diversions)[1])
spec_trans = round(sum(1 - as.numeric(as.character(merged_anal_sub_rf_trans$transplantation[loo_trans_pred_prob < cp_trans$optimal_cutpoint])))/
                     sum(1 - as.numeric(as.character(merged_anal_sub_rf_trans$transplantation))),2)


cat(paste0("Death\n",
           "accuracy: ",acc_death, "\n",
           "AUROC: ", auroc_death, "\n",
           "Sensititivy: ", sens_death, "\n",
           "Specificity: ", spec_death, "\n\n\n",
           
           "Diversions\n",
           "accuracy: ",acc_div, "\n",
           "AUROC: ", auroc_div, "\n",
           "Sensititivy: ", sens_div, "\n",
           "Specificity: ", spec_div, "\n\n\n",
           
           "RRT\n",
           "accuracy: ",acc_rrt, "\n",
           "AUROC: ", auroc_rrt, "\n",
           "Sensititivy: ", sens_rrt, "\n",
           "Specificity: ", spec_rrt, "\n\n\n",
           
           "Dialysis\n",
           "accuracy: ",acc_dia, "\n",
           "AUROC: ", auroc_dia, "\n",
           "Sensititivy: ", sens_dia, "\n",
           "Specificity: ", spec_dia, "\n\n\n",
           
           "Transplant\n",
           "accuracy: ",acc_trans, "\n",
           "AUROC: ", auroc_trans, "\n",
           "Sensititivy: ", sens_trans, "\n",
           "Specificity: ", spec_trans, "\n\n\n"))


########################
###******************###
## CONFUSION MATRIX
###******************###
########################

cat("Death\n")
# table(merged_anal_sub_rf_death$death,loo_death_pred)
table("true" = merged_anal_sub_rf_death$death,"predicted" = ifelse(loo_death_pred_prob >= cp_death$optimal_cutpoint,1,0))

cat("Diversion\n")
# table(merged_anal_sub_rf_div$diversions,loo_div_pred)
table("true" = merged_anal_sub_rf_div$diversions,"predicted" = ifelse(loo_div_pred_prob >= cp_div$optimal_cutpoint,1,0))

cat("RRT\n")
# table(merged_anal_sub_rf_rrt$RRT,loo_rrt_pred)
table("true" = merged_anal_sub_rf_rrt$RRT,"predicted" = ifelse(loo_rrt_pred_prob >= cp_rrt$optimal_cutpoint,1,0))

cat("Dialysis\n")
# table(merged_anal_sub_rf_dia$dialysis,loo_dia_pred)
table("true" = merged_anal_sub_rf_dia$dialysis,"predicted" = ifelse(loo_dia_pred_prob >= cp_dia$optimal_cutpoint,1,0))

cat("Transplant\n")
# table(merged_anal_sub_rf_trans$transplantation,loo_trans_pred)
table("true" = merged_anal_sub_rf_trans$transplantation,"predicted" = ifelse(loo_trans_pred_prob >= cp_trans$optimal_cutpoint,1,0))




########################
###******************###
## ROC curves
###******************###
########################
library("ROCR")

## Death
pred <- ROCR::prediction(loo_death_pred_prob, merged_anal_sub_rf_death$death)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=FALSE, lwd = 2)
abline(0,1,col = "red", lty = 2)

## Diversion
pred <- ROCR::prediction(loo_div_pred_prob, merged_anal_sub_rf_div$diversions)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=FALSE, lwd = 2)
abline(0,1,col = "red", lty = 2)

## RRT
pred <- ROCR::prediction(loo_rrt_pred_prob, merged_anal_sub_rf_rrt$RRT)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=FALSE, lwd = 2)
abline(0,1,col = "red", lty = 2)

## Dialysis
pred <- ROCR::prediction(loo_dia_pred_prob, merged_anal_sub_rf_dia$dialysis)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=FALSE, lwd = 2)
abline(0,1,col = "red", lty = 2)

## Transplant
pred <- ROCR::prediction(loo_trans_pred_prob, merged_anal_sub_rf_trans$transplantation)
# pred <- prediction(df$predictions, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=FALSE, lwd =2)
abline(0,1, col = "red", lty = 2)


## ------------------------------------------------------------ ## 
### ROC V2
## ------------------------------------------------------------ ## 

library(ggplot2)

theme_set(
  theme_bw(base_size = 12)
)

plot(cp_death)

plot(cp_div)

plot(cp_dia)

plot(cp_rrt)

plot(cp_trans)

########################
###******************###
## Overall importance
###******************###
########################

my_rows = unique(c(rownames(div_importance), rownames(dia_importance), rownames(trans_importance),rownames(rrt_importance)))

length(rownames(death_importance))
length(rownames(div_importance))
length(rownames(dia_importance))
length(rownames(trans_importance))
length(rownames(rrt_importance))

## add column showing decrease in Gini
over_importance = data.frame(
  Variable = my_rows,  
  Dialysis = order(dia_importance[,4],decreasing = TRUE)[match(my_rows,rownames(dia_importance))],
  Death = order(death_importance[,4],decreasing = TRUE)[match(my_rows,rownames(death_importance))],
  Transplant = order(trans_importance[,4],decreasing = TRUE)[match(my_rows,rownames(trans_importance))],
  Diversion = order(div_importance[,4],decreasing = TRUE)[match(my_rows,rownames(div_importance))],
  RRT = order(rrt_importance[,4],decreasing = TRUE)[match(my_rows,rownames(rrt_importance))]
)

write.csv(over_importance[order(over_importance$Dialysis,over_importance$Death,over_importance$Transplant,over_importance$Diversion),],
          file = "C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/RF_VariableImportance_20240118_withDeath_withRRT.csv")

## predict in terminated patients


## PREP TERMINATED DATA



in_df_tops = in_df_lutos[in_df_lutos$category == 2,]
in_df_tops = in_df_tops[in_df_tops$prenatal_diagnosis_msh %in% c(1,10,11,12),]
dim(in_df_tops)
str(in_df_tops)

merged_tops_df = in_df_tops

merged_tops_df_rf = merged_tops_df[,c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
                                      "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",
                                      "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#,"urine_normal""intervention","GAInitial",
                                      "lt_echogenicity","Megacystis","Ascites","Female", "diversions", "transplantation","dialysis",
                                      "death","InitialUSDays","genetic anomaly")]
str(merged_tops_df_rf)

## predict outcome

top_dia_pred = c()
top_death_pred = c()
top_div_pred = c()
top_trans_pred = c()
top_rrt_pred = c()


predictors = c("normal amniotic fluid", "rt_renalp_diameter","lt_renalp_diameter","mean_rt_length","mean_lt_length",
               "bladderwall_thickness","bladderwall_thickening_yn","GAatDiagnosis","afi_level","lt_hn","rt_hn",
               "MeanKidneyVol","rt_hydroureter","lt_hydroureter","keyhole_sign","rt_echogenicity",#"intervention","urine_normal","GAInitial",
               "lt_echogenicity","Megacystis","Ascites","Female","InitialUSDays","genetic anomaly")

for(i in 1:nrow(merged_tops_df_rf)){
  
  i_top_predictors = names(data.frame(merged_tops_df_rf))[!is.na(data.frame(merged_tops_df_rf[i,]))]
  i_top_predictors = i_top_predictors[i_top_predictors %in% predictors]
  
  if(length(i_top_predictors) > 0){
    dia_df = data.frame(merged_anal_sub)[,c("dialysis",i_top_predictors)]
    death_df = data.frame(merged_anal_sub)[,c("death",i_top_predictors)]
    div_df = data.frame(merged_anal_sub)[,c("diversions",i_top_predictors)]
    trans_df = data.frame(merged_anal_sub)[,c("transplantation",i_top_predictors)]
    rrt_df = data.frame(merged_anal_sub)[,c("RRT",i_top_predictors)]
    
    dia_df$dialysis = factor(dia_df$dialysis, levels = 0:1)
    death_df$death = factor(death_df$death, levels = 0:1)
    div_df$diversions = factor(div_df$diversions, levels = 0:1)
    trans_df$transplantation = factor(trans_df$transplantation, levels = 0:1)
    rrt_df$RRT = factor(rrt_df$RRT, levels = 0:1)
    
    top_dia_rf = randomForest(dialysis ~ ., dia_df, na.action = na.roughfix)
    top_death_rf = randomForest(death ~ ., death_df, na.action = na.roughfix)
    top_div_rf = randomForest(diversions ~ ., div_df, na.action = na.roughfix)
    top_trans_rf = randomForest(transplantation ~ ., trans_df, na.action = na.roughfix)
    top_rrt_rf = randomForest(RRT ~ ., rrt_df, na.action = na.roughfix)
    
    top_dia_pred_i = predict(top_dia_rf,newdata=data.frame(merged_tops_df_rf[i,]),type = "prob")[2]
    top_death_pred_i = predict(top_death_rf,newdata=data.frame(merged_tops_df_rf[i,]),type = "prob")[2]
    top_div_pred_i = predict(top_div_rf,newdata=data.frame(merged_tops_df_rf[i,]),type = "prob")[2]
    top_trans_pred_i = predict(top_trans_rf,newdata=data.frame(merged_tops_df_rf[i,]),type = "prob")[2]
    top_rrt_pred_i = predict(top_rrt_rf,newdata=data.frame(merged_tops_df_rf[i,]),type = "prob")[2]
    
    top_dia_pred = c(top_dia_pred, top_dia_pred_i)
    top_death_pred = c(top_death_pred, top_death_pred_i)
    top_div_pred = c(top_div_pred, top_div_pred_i)
    top_trans_pred = c(top_trans_pred, top_trans_pred_i)    
    top_rrt_pred = c(top_rrt_pred, top_rrt_pred_i)    
    
  } else{
    
    top_dia_pred = c(top_dia_pred, NA)
    top_death_pred = c(top_death_pred, NA)
    top_div_pred = c(top_div_pred, NA)
    top_trans_pred = c(top_trans_pred, NA)    
    top_rrt_pred = c(top_rrt_pred, NA)    
    
    
  }
  
  
  
}

# summary(top_dia_pred)
# summary(top_div_pred)
# summary(top_trans_pred)

cat("Death\n")
table(ifelse(top_death_pred >= cp_death$optimal_cutpoint, 1, 0))
tops_death_vec = ifelse(top_death_pred >= cp_death$optimal_cutpoint, 1, 0)

cat("Dialysis\n")
table(ifelse(top_dia_pred >= cp_dia$optimal_cutpoint, 1, 0))
tops_dia_vec = ifelse(top_dia_pred >= cp_dia$optimal_cutpoint, 1, 0)

cat("Diversions\n")
table(ifelse(top_div_pred >= cp_div$optimal_cutpoint, 1, 0))
tops_div_vec = ifelse(top_div_pred >= cp_div$optimal_cutpoint, 1, 0)

cat("RRT\n")
table(ifelse(top_rrt_pred >= cp_rrt$optimal_cutpoint, 1, 0))
tops_rrt_vec = ifelse(top_rrt_pred >= cp_rrt$optimal_cutpoint, 1, 0)

cat("Transplant\n")
table(ifelse(top_trans_pred >= cp_trans$optimal_cutpoint, 1, 0))
tops_trans_vec = ifelse(top_trans_pred >= cp_trans$optimal_cutpoint, 1, 0)

table(tops_trans_vec,
      tops_death_vec)

table(tops_dia_vec,
      tops_death_vec)

table(tops_div_vec,
      tops_death_vec)

table(tops_rrt_vec,
      tops_death_vec)

table(tops_dia_vec,
      tops_trans_vec)

table(tops_div_vec,
      tops_trans_vec)

table(tops_rrt_vec,
      tops_trans_vec)

table(tops_div_vec,
      tops_dia_vec)

table(tops_rrt_vec,
      tops_dia_vec)

table(tops_rrt_vec,
      tops_div_vec)

# # top_in_df = read_excel("C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/New_Data Prediction .xlsx",sheet = "TOPs w autopsy")
# top_in_df = read_excel("C:/Users/lauren erdman/Desktop/kidney_img/LUTO_Juliane/Clean Data CAKUT.xlsx",sheet = "TOPs LUTO")
# str(top_in_df) ## WHAT DO I INCLUDE HERE? 
# 
# top_in_df$record_id
# top_in_df$`GA diagnosis round`
# top_in_df$rtkidney_apd
# top_in_df$ltkidney_apd
# top_in_df$hydrops
# top_in_df$maxvertpocket
# top_in_df$afi_level
# top_in_df$bladderwall_mm
# top_in_df$bladderwall
# top_in_df$ga_us_totaldays = top_in_df$ga_us_days + top_in_df$ga_ultrasound*7
# top_in_df$rt_echogenicity
# top_in_df$ltkidney_echogenicity
# top_in_df$beta2mg_urine
# 
# 
# top_in_preanal = top_in_df[,c("record_id","GA diagnosis round","rtkidney_apd","ltkidney_apd","hydrops","maxvertpocket","afi_level","bladderwall_mm","bladderwall",
#     "ga_us_totaldays","rt_echogenicity","ltkidney_echogenicity","beta2mg_urine")]
# 
# 
# names(top_in_preanal) = c("record_id","GA diagnosis round","rtkidney_APD pelvis","ltkidney_APD pelvis","hydrops","maxvertpocket","afi_level","bladderwall_mm","bladderwall",
#                          "ga_us_totaldays","rt_echogenicity","ltkidney_echogenicity","beta2mg_urine")
# 
# 
#       ## MERGE
# 
# top_merged_anal_df = merge(preanal_in_df2, top_in_preanal,by.x = "Study ID",by.y = "record_id")
# 
# str(top_merged_anal_df)
# 
#     ## subset to ga first US within 13-26 weeks 
#     ## first recode weeks + days into days
# 
# hist(top_merged_anal_df$ga_us_totaldays)
# summary(top_merged_anal_df$ga_us_totaldays)
# 
# top_merged_anal_sub = top_merged_anal_df[top_merged_anal_df$ga_us_totaldays > 90 & top_merged_anal_df$ga_us_totaldays < 183,]
# top_merged_anal_sub = top_merged_anal_sub[!is.na(top_merged_anal_sub$`Study ID`),]
# 
# str(top_merged_anal_sub)
# top_merged_anal_sub$`Study ID`
# 
# 
#     ## recode factors
# 
# top_merged_anal_sub$bladderwall = factor(top_merged_anal_sub$bladderwall, levels = 1:3)
# top_merged_anal_sub$gender = factor(top_merged_anal_sub$gender, levels = 1:2)
# 
# 

