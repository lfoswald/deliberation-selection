### Preparing Data for P1b Analysis ###

source("code/tracking_packages.R")

### class data + information, communication

user_data_deliberation <- read_excel("data/final/user_data_deliberation_P1b_V3.xlsx")

### Regression Preprocessing

# recoding step 1
user_data_deliberation <- user_data_deliberation%>%
  mutate(educ = na_if(educ, "6"),# NA
         educ = na_if(educ, "1"), # still in hs
         educ = ifelse(educ == 5, 2 ,educ), # pool no degree + Hauptschule
         abitur = ifelse(educ == 4, 1,0),
         polinterest = na_if(polinterest, "6"), # 6 don't know
         demsatis = na_if(demsatis, "6"),
         relationship = na_if(relationship, "4"), # sonstiges
         relationship = na_if(relationship, "5"), #NA
         relationship = ifelse(relationship == 1, 2, relationship), #pool all non single
         leftright = na_if(leftright, "0"),
         afdvote = ifelse(firstvote == 6, 1, 0),
         votecertainty = na_if(votecertainty, "997"),
         firstvote = na_if(firstvote, "7"), # other
         firstvote = na_if(firstvote, "8")) # don't know

# load extra information: extremism, efficacy, political knowledge
ee_data <- readRDS("data/final/extr_effic_extra_scales_P1.Rda")
know_data <- readRDS("data/final/extr_knowledge_scales_P1.Rda")

# prepare for left join
ee_data$caseid <- as.numeric(ee_data$personid)
know_data$caseid <- as.numeric(know_data$personid)
user_data_deliberation$caseid <- as.numeric(user_data_deliberation$caseid.x)

user_data_deliberation <- left_join(user_data_deliberation, ee_data, by="caseid")%>%
  left_join(., know_data, by="caseid")

#write_xlsx(user_data_deliberation, "data/temp/user_data_deliberation_regr_1.xlsx")

# recoding step 2
user_data_deliberation <- user_data_deliberation%>%
  mutate(gender = as.factor(gender),
         educ = as.factor(educ),
         firstvote = as.factor(firstvote),
         afdvote = as.factor(afdvote),
         relationship = as.factor(relationship),
         online = online_clicks)

# gelman scale - divide by 2 standard deviations for better interpretation 
gelman_scale <- function(x){
  ( (x - mean(x, na.rm=T)) / (2*sd(x, na.rm=TRUE)))
}

user_data_deliberation[c("age", "hhincome", "polinterest", 
                         "demsatis", "leftright",
                         "understand_efficacy", "politicians_care_efficacy",
                         "dictator", "socialism", "national_pride",
                         "resp_prop_correct","online")] <- lapply(user_data_deliberation[c("age", 
                                                                                           "hhincome", "polinterest", 
                                                                                           "demsatis", "leftright",
                                                                                           "understand_efficacy", "politicians_care_efficacy",
                                                                                           "dictator", "socialism", "national_pride",
                                                                                           "resp_prop_correct","online")], gelman_scale )


#write_xlsx(user_data_deliberation, "data/temp/user_data_deliberation_regr_2.xlsx")


####################################################################################

# Regression Preprocessing for specific domains

user_data_deliberations <- read_excel("data/final/user_data_deliberation_special_P1.xlsx")

# recoding step 1
user_data_deliberations <- user_data_deliberations%>%
  mutate(educ = na_if(educ, "6"),# NA
         educ = na_if(educ, "1"), # still in hs
         educ = ifelse(educ == 5, 2 ,educ), # pool no degree + Hauptschule
         abitur = ifelse(educ == 4, 1,0),
         polinterest = na_if(polinterest, "6"), # 6 don't know
         demsatis = na_if(demsatis, "6"),
         relationship = na_if(relationship, "4"), # sonstiges
         relationship = na_if(relationship, "5"), #NA
         relationship = ifelse(relationship == 1, 2, relationship), #pool all non single
         leftright = na_if(leftright, "0"),
         afdvote = ifelse(firstvote == 6, 1, 0),
         votecertainty = na_if(votecertainty, "997"),
         firstvote = na_if(firstvote, "7"), # other
         firstvote = na_if(firstvote, "8")) # don't know

## load extra information: extremism, efficacy, political knowledge
ee_data <- readRDS("data/final/extr_effic_extra_scales_P1.Rda")
know_data <- readRDS("data/final/extr_knowledge_scales_P1.Rda")

# prepare for left join
ee_data$caseid <- as.numeric(ee_data$personid)
know_data$caseid <- as.numeric(know_data$personid)
user_data_deliberations$caseid <- as.numeric(user_data_deliberations$caseid.x)

user_data_deliberations <- left_join(user_data_deliberations, ee_data, by="caseid")%>%
  left_join(., know_data, by="caseid")


#write_xlsx(user_data_deliberations, "data/temp/user_data_deliberation_regr_1_special.xlsx")

## recoding step 2
user_data_deliberations <- user_data_deliberations%>%
  mutate(gender = as.factor(gender),
         educ = as.factor(educ),
         firstvote = as.factor(firstvote),
         relationship = as.factor(relationship),
         online = online_clicks)

# gelman scale - divide by 2 standard deviations for better interpretability 
gelman_scale <- function(x){
  ( (x - mean(x, na.rm=T)) / (2*sd(x, na.rm=TRUE)))
}

user_data_deliberations[c("age", "hhincome", "polinterest", 
                         "demsatis", "leftright",
                         "understand_efficacy", "politicians_care_efficacy",
                         "dictator", "socialism", "national_pride",
                         "resp_prop_correct","online")] <- lapply(user_data_deliberations[c("age", 
                                                                                           "hhincome", "polinterest", 
                                                                                           "demsatis", "leftright",
                                                                                           "understand_efficacy", "politicians_care_efficacy",
                                                                                           "dictator", "socialism", "national_pride",
                                                                                           "resp_prop_correct","online")], gelman_scale )


#write_xlsx(user_data_deliberations, "data/temp/user_data_deliberation_regr_2_special.xlsx")


