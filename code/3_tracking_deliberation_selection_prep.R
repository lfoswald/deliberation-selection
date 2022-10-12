############### Preparation for Seletive Exposure Exploration ###################

source("code/tracking_packages.R")

######### Get Data + Classes from Measurement Script ###############################

class_data <- read_excel("data/final/data_LCA_c_V3.xlsx") 

# combine coded data with original data
deliberation_data <- readRDS(file="data/temp/data_deliberation.Rda")

# use politically relevant data for intermediate merging step
pol_rel_data <- readRDS(file="data/temp/filtered_big_relevant_df.Rda")
pol_rel_data <- pol_rel_data%>%
  dplyr::select(domain,index)

# additional pre-processing step to make left_join work properly
pol_rel_data$domain <- as.character(pol_rel_data$domain)
class_data$domain <- as.character(class_data$domain)

pol_rel_data <- left_join(pol_rel_data, class_data, by = "domain")

pol_rel_data$index <- as.numeric(pol_rel_data$index)
deliberation_data$index <- as.numeric(deliberation_data$index)

FINAL_data  <- left_join(deliberation_data, pol_rel_data, by = "index") 

# test
FINAL_data%>%
  filter(!is.na(info_pol))%>%
  glimpse()
# correct reduced to 493,714 from 516,158 political clicks
test_sample <- FINAL_data%>%
  group_by(caseid)%>%
  slice(1)
# 1282 cases - correct!
rm(deliberation_data)

names(FINAL_data)
# add summary dimensions
FINAL_data <- FINAL_data%>%
  mutate(information = info_pol+info_qual+info_loc,
         communication = com_pr+com_rec,
         participation = par_actor+par_orga+par_par,
         isolation = isolation_in+isolation_out,
         inclusivity = incl_gender+incl_age+incl_educ,
         heterogeneity = hom_pol_or+hom_vote1)


#saveRDS(FINAL_data, file="data/final/FINAL_data_P1b_V3.Rda")


##################################################################################

# Per person engagement measures: C1, C2, C3, information, communication

## Calculating per person outcome variables
FINAL_data <- readRDS("data/final/FINAL_data_P1b_V3.Rda")

# sample sizes (clicks / domain) - values Oct 11, 2021
#FINAL_data%>%group_by(domain.x)%>%glimpse() #56,102,429 / 198,084
#FINAL_data%>%filter(!is.na(info_pol))%>%group_by(domain.x)%>%glimpse() #493,714 / 69
#FINAL_data%>%filter(class==1)%>%group_by(domain.x)%>%glimpse() #11,560 / 15
#FINAL_data%>%filter(class==2)%>%group_by(domain.x)%>%glimpse() #8,347 / 20
#FINAL_data%>%filter(class==3)%>%group_by(domain.x)%>%glimpse() #473,807 / 34 

#mn_info <- mean(FINAL_data$information,na.rm=T)
#mn_com <- mean(FINAL_data$communication,na.rm=T)
#FINAL_data%>%filter(information > mn_info)%>%group_by(domain.x)%>%glimpse()  # 
#FINAL_data%>%filter(communication > mn_com)%>%group_by(domain.x)%>%glimpse() 

# Freq - how frequented is this domain?
# Duration - domain level duration
# duration - click duration
# online_duration - person level duration

# get person level engagement metrics
FINAL_pers_data <- FINAL_data%>%
  mutate(pol_rel_site = ifelse(!is.na(info_pol),1,0))%>% # what are political sites?
  group_by(caseid)%>%
  mutate(online_duration = sum(duration, na.rm=TRUE), # overall time online
         online_clicks = n())
# test - still 1282 cases 

### get engagement information (filter, then count clicks and duration per person)

# all political engagement
FINAL_pers_data_a <- FINAL_pers_data%>%
  filter(pol_rel_site == 1)%>%
  group_by(caseid)%>%
  mutate(pol_duration = sum(duration),
         pol_clicks = n())%>%
  dplyr::select(index,caseid,pol_duration,pol_clicks)

# C1 engagement
FINAL_pers_data_b <- FINAL_pers_data%>%
  filter(class == 1)%>%
  group_by(caseid)%>%
  mutate(c1_duration = sum(duration),
         c1_clicks = n())%>%
  dplyr::select(index,caseid,c1_duration,c1_clicks)

# C2 engagement
FINAL_pers_data_c <- FINAL_pers_data%>%
  filter(class == 2)%>%
  group_by(caseid)%>%
  mutate(c2_duration = sum(duration),
         c2_clicks = n())%>%
  dplyr::select(index,caseid,c2_duration,c2_clicks)

# C3 engagement
FINAL_pers_data_d <- FINAL_pers_data%>%
  filter(class == 3)%>%
  group_by(caseid)%>%
  mutate(c3_duration = sum(duration),
         c3_clicks = n())%>%
  dplyr::select(index,caseid,c3_duration,c3_clicks)

# information engagement
m_info <- mean(FINAL_pers_data$information, na.rm=T)
FINAL_pers_data_e <- FINAL_pers_data%>%
  filter(information > m_info)%>%
  group_by(caseid)%>%
  mutate(info_duration = sum(duration),
         info_clicks = n())%>%
  dplyr::select(index,caseid,info_duration,info_clicks)

# communication engagement
m_com <- mean(FINAL_pers_data$communication, na.rm=T)

FINAL_pers_data_f <- FINAL_pers_data%>%
  filter(communication > m_com)%>%
  group_by(caseid)%>%
  mutate(com_duration = sum(duration),
         com_clicks = n())%>%
  dplyr::select(index,caseid,com_duration,com_clicks)

# weird fuckn workaround to not make left_join do shit.....
FINAL_pers_data$index <- as.numeric(FINAL_pers_data$index)
FINAL_pers_data_a$index <- as.numeric(FINAL_pers_data_a$index)
FINAL_pers_data_b$index <- as.numeric(FINAL_pers_data_b$index)
FINAL_pers_data_c$index <- as.numeric(FINAL_pers_data_c$index)
FINAL_pers_data_d$index <- as.numeric(FINAL_pers_data_d$index)
FINAL_pers_data_e$index <- as.numeric(FINAL_pers_data_e$index)
FINAL_pers_data_f$index <- as.numeric(FINAL_pers_data_f$index)

# joining new information (per person clicks and duration on respective pol sites) together
FINAL_pers_data_full <- left_join(FINAL_pers_data, FINAL_pers_data_a, by = "index")%>%
  left_join(., FINAL_pers_data_b, by = "index")%>%
  left_join(., FINAL_pers_data_c, by = "index")%>%
  left_join(., FINAL_pers_data_d, by = "index")


rm(FINAL_pers_data)
rm(FINAL_pers_data_a)
rm(FINAL_pers_data_b)
rm(FINAL_pers_data_c)
rm(FINAL_pers_data_d)

# split pipeline (otherwise R fatal error...)
FINAL_pers_data_full <- left_join(FINAL_pers_data_full, FINAL_pers_data_e, by = "index")%>%
  left_join(., FINAL_pers_data_f, by = "index")


rm(FINAL_pers_data_e)
rm(FINAL_pers_data_f)
rm(FINAL_pers_data_full)


#test
pers_sample <- FINAL_pers_data_full%>%
  group_by(caseid.x)%>%
  slice(1)


## compress data to user level data
user_data_deliberation <- FINAL_pers_data_full%>%
  group_by(caseid.x)%>%
  mutate(online_duration = sum(duration, na.rm=TRUE), # do this again because it did not work before
         #get the information compressed into one row per individual
         pol_duration = mean(pol_duration, na.rm = TRUE), 
         pol_clicks = mean(pol_clicks, na.rm = TRUE),
         c1_duration = mean(c1_duration, na.rm = TRUE),
         c1_clicks = mean(c1_clicks, na.rm = TRUE),
         c2_duration = mean(c2_duration, na.rm = TRUE),
         c2_clicks = mean(c2_clicks, na.rm = TRUE),
         c3_duration = mean(c3_duration, na.rm = TRUE),
         c3_clicks = mean(c3_clicks, na.rm = TRUE),
         info_duration = mean(info_duration, na.rm = TRUE),
         info_clicks = mean(info_clicks, na.rm = TRUE),
         com_duration = mean(com_duration, na.rm = TRUE),
         com_clicks = mean(com_clicks, na.rm = TRUE),
  )%>%
  slice(1)%>%
  mutate_at(vars(pol_duration, pol_clicks, 
                 c1_duration, c1_clicks,
                 c2_duration, c2_clicks, 
                 c3_duration, c3_clicks,
                 info_duration, info_clicks,
                 com_duration, com_clicks
  ), 
  ~replace_na(.,0))%>% # no engagement means 0 clicks and 0 seconds
  dplyr::select(-domain.x,-host,-subdomain,-suffix,-used_at,-duration,
                -Freq,-categories,-na_count,-communication,-participation,
                -information,-caseid.y,-caseid.x.x, -caseid.y.y,-caseid)#keep caseid.x (original one!)


#saveRDS(FINAL_pers_data_full, file="data/temp/FINAL_pers_data_full_P1b_V3.Rda")
#write_xlsx(user_data_deliberation,"data/final/user_data_deliberation_P1b_V3.xlsx")

####### Specific domain engagement  #######################################################

FINAL_data$domain <- FINAL_data$domain.x

# sample sizes for specific domains
#FINAL_data%>%filter(domain=="zeit" & !is.na(information))%>%group_by(caseid)%>%glimpse() #16,609 / 318
#FINAL_data%>%filter(domain=="bild" & !is.na(information))%>%group_by(caseid)%>%glimpse() #36,771 / 434
#FINAL_data%>%filter(domain=="facebook" & !is.na(information))%>%group_by(caseid)%>%glimpse() #31,592 / 756 
#FINAL_data%>%filter(domain=="twitter" & !is.na(information))%>%group_by(caseid)%>%glimpse() #17,566 / 178 
#FINAL_data%>%filter(domain=="dedesboard"& !is.na(information))%>%group_by(caseid)%>%glimpse() #2,870 / 1 
#FINAL_data%>%filter(domain=="change"& !is.na(information))%>%group_by(caseid)%>%glimpse() #3,437 / 188 

# get person level engagement metrics
FINAL_pers_data_special <- FINAL_data%>%
  group_by(caseid)%>%
  mutate(online_duration = sum(duration, na.rm=TRUE), # overall time online
         online_clicks = n())

### get engagement information (filter, then count clicks and duration per person)

# Zeit
FINAL_pers_data_e <- FINAL_pers_data_special%>%
  filter(domain == "zeit" & !is.na(information))%>%
  group_by(caseid)%>%
  mutate(zeit_duration = sum(duration),
         zeit_clicks = n())%>%
  dplyr::select(index,caseid,zeit_duration,zeit_clicks)

# Bild 
FINAL_pers_data_f <- FINAL_pers_data_special%>%
  filter(domain == "bild"& !is.na(information))%>%
  group_by(caseid)%>%
  mutate(bild_duration = sum(duration),
         bild_clicks = n())%>%
  dplyr::select(index,caseid,bild_duration,bild_clicks)

# Facebook
FINAL_pers_data_g <- FINAL_pers_data_special%>%
  filter(domain == "facebook" & !is.na(information))%>%
  group_by(caseid)%>%
  mutate(facebook_duration = sum(duration),
         facebook_clicks = n())%>%
  dplyr::select(index,caseid,facebook_duration,facebook_clicks)

# Twitter
FINAL_pers_data_h <- FINAL_pers_data_special%>%
  filter(domain == "twitter"& !is.na(information))%>%
  group_by(caseid)%>%
  mutate(twitter_duration = sum(duration),
         twitter_clicks = n())%>%
  dplyr::select(index,caseid,twitter_duration,twitter_clicks)

# Change
FINAL_pers_data_i <- FINAL_pers_data_special%>%
  filter(domain == "change"& !is.na(information))%>%
  group_by(caseid)%>%
  mutate(change_duration = sum(duration),
         change_clicks = n())%>%
  dplyr::select(index,caseid,change_duration,change_clicks)

# weird fuckn workaround to not make left_join do shit.....
FINAL_pers_data_special$index <- as.numeric(FINAL_pers_data_special$index)
FINAL_pers_data_e$index <- as.numeric(FINAL_pers_data_e$index)
FINAL_pers_data_f$index <- as.numeric(FINAL_pers_data_f$index)
FINAL_pers_data_g$index <- as.numeric(FINAL_pers_data_g$index)
FINAL_pers_data_h$index <- as.numeric(FINAL_pers_data_h$index)
FINAL_pers_data_i$index <- as.numeric(FINAL_pers_data_i$index)
# joining new information (per person clicks and duration on respective pol sites) together
FINAL_pers_data_full_special <- left_join(FINAL_pers_data_special, FINAL_pers_data_e, by = "index")%>%
  left_join(., FINAL_pers_data_f, by = "index")%>%
  left_join(., FINAL_pers_data_g, by = "index")%>%
  left_join(., FINAL_pers_data_h, by = "index")%>%
  left_join(., FINAL_pers_data_i, by = "index")


## user level data
user_data_deliberation_special <- FINAL_pers_data_full_special%>%
  group_by(caseid.x)%>%
  mutate(online_duration = sum(duration, na.rm=TRUE), # do this again because it did not work before
         #get the information compressed into one row per individual
         zeit_duration = mean(zeit_duration, na.rm = TRUE), 
         zeit_clicks = mean(zeit_clicks, na.rm = TRUE),
         bild_duration = mean(bild_duration, na.rm = TRUE),
         bild_clicks = mean(bild_clicks, na.rm = TRUE),
         facebook_duration = mean(facebook_duration, na.rm = TRUE),
         facebook_clicks = mean(facebook_clicks, na.rm = TRUE),
         change_duration = mean(change_duration, na.rm = TRUE),
         change_clicks = mean(change_clicks, na.rm = TRUE),
         twitter_duration = mean(twitter_duration, na.rm = TRUE),
         twitter_clicks = mean(twitter_clicks, na.rm = TRUE))%>%
  slice(1)%>%
  mutate_at(vars(zeit_duration, zeit_clicks, 
                 bild_duration, bild_clicks,
                 facebook_duration, facebook_clicks, 
                 twitter_duration, twitter_clicks,
                 change_duration, change_clicks), 
            ~replace_na(.,0))%>% # no engagement means 0 clicks and 0 seconds
  dplyr::select(-domain,-host,-subdomain,-suffix,-used_at,-duration,
                -Freq,-categories,-na_count,-communication,-participation,
                -information,-caseid.y,-caseid.x.x, -caseid.y.y,-caseid.x.x.x, 
                -caseid.y.y.y)#keep caseid.x (original one!)

#write_xlsx(user_data_deliberation_special,"data/final/user_data_deliberation_special_P1.xlsx")

rm(FINAL_pers_data_special)
rm(FINAL_pers_data_e)
rm(FINAL_pers_data_f)
rm(FINAL_pers_data_g)
rm(FINAL_pers_data_h)
rm(FINAL_pers_data_i)
rm(FINAL_pers_data_full_special)
rm(FINAL_data)


