### LPA Prep + Correlation Plots ###

source("code/tracking_packages.R")

# data engagement with classes + info + com
user_data_deliberation <- read_excel("data/temp/user_data_deliberation_regr_2.xlsx")

# data on engagement with specific websites
user_data_deliberations <- read_excel("data/temp/user_data_deliberation_regr_2_special.xlsx")

# merge different types of engagement to one data frame
special_dat <- user_data_deliberations%>%
  dplyr::select(caseid, 
                zeit_clicks, zeit_duration, bild_clicks, bild_duration,
                facebook_clicks, facebook_duration, twitter_clicks, twitter_duration,
                change_clicks, change_duration)

special_dat$caseid <- as.numeric(special_dat$caseid)

user_data_deliberation <- left_join(user_data_deliberation, special_dat, by="caseid")

#write_xlsx(user_data_deliberation, "data/temp/user_data_deliberation_all.xlsx")


### Correlation Plots

### 1. Clicks 
corr_data <- user_data_deliberation%>%
  ungroup()%>%
  dplyr::select(gender , age , educ ,relationship , hhincome , polinterest, resp_prop_correct, 
                demsatis , leftright  , understand_efficacy, politicians_care_efficacy,
                dictator, socialism, national_pride, online_clicks ,pol_clicks ,
                c1_clicks, c2_clicks, c3_clicks, info_clicks, com_clicks, zeit_clicks,
                facebook_clicks, bild_clicks, twitter_clicks, change_clicks)%>%
  mutate_all(~as.numeric(as.character(.)))%>%
  plyr::rename(c("socialism" = "Socialism",
                 "dictator" = "Dictator", 
                 "national_pride" = "National Pride",
                 "resp_prop_correct" = "Political Knowledge",
                 "politicians_care_efficacy" = "Efficacy (Trust)",
                 "understand_efficacy" = "Efficacy (Understanding)",
                 "polinterest" = "Political Interest", 
                 "leftright" = "Political Orientation",
                 "hhincome"  = "Household Income",
                 "gender" = "Female", 
                 "demsatis" = "Satisfaction with Democracy",
                 "age" = "Age",
                 "relationship" = "Relationship",
                 "educ" = "Education",
                 "online_clicks" = "Overall Clicks",
                 "pol_clicks" = "Political Clicks",
                 "c1_clicks" = "Clicks C1 (Public Broadc.)",
                 "c2_clicks" = "Clicks C2 (Niche Forums)",
                 "c3_clicks" = "Clicks C3 (Information Hubs)",
                 "info_clicks" = "Clicks Pol. Information",
                 "com_clicks" = "Clicks Pol. Communication",
                 "zeit_clicks" = "Clicks Zeit",
                 "bild_clicks" = "Clicks Bild",
                 "facebook_clicks" = "Clicks Facebook",
                 "twitter_clicks" = "Clicks Twitter",
                 "change_clicks" = "Clicks Change"
                 
                 
  ))

corr <- round(cor(corr_data, use = "pairwise.complete.obs"), 1)
p.mat <- cor_pmat(corr_data, use = "pairwise.complete.obs")

corr_c_plot <- ggcorrplot(corr, method = "circle", type = "upper", 
                          outline.col = "white",
                          title = "Bivariate Correlations - Clicks", insig = "blank",
                          tl.cex = 8,
                          tl.srt = 90,
                          lab = T, lab_size = 3, show.legend = F)
corr_c_plot

#ggsave("output/corr_clicks_plot.pdf", width = 10, height = 10, dpi = 300, corr_c_plot)

### 2. Duration
corr_data_d <- user_data_deliberation%>%
  ungroup()%>%
  dplyr::select(gender , age , educ ,relationship , hhincome , polinterest, resp_prop_correct, 
                demsatis , leftright  , understand_efficacy, politicians_care_efficacy,
                dictator, socialism, national_pride, online_duration ,pol_duration ,
                c1_duration, c2_duration, c3_duration, info_duration, com_duration,
                zeit_duration,facebook_duration, bild_duration, twitter_duration, change_duration)%>%
  mutate_all(~as.numeric(as.character(.)))%>%
  plyr::rename(c("socialism" = "Socialism",
                 "dictator" = "Dictator", 
                 "national_pride" = "National Pride",
                 "resp_prop_correct" = "Political Knowledge",
                 "politicians_care_efficacy" = "Efficacy (Trust)",
                 "understand_efficacy" = "Efficacy (Understanding)",
                 "polinterest" = "Political Interest", 
                 "leftright" = "Political Orientation",
                 "hhincome"  = "Household Income",
                 "gender" = "Female", 
                 "demsatis" = "Satisfaction with Democracy",
                 "age" = "Age",
                 "relationship" = "Relationship",
                 "educ" = "Education",
                 "online_duration" = "Overall Duration",
                 "pol_duration" = "Political Duration",
                 "c1_duration" = "Duration C1 (Public Broadc.)",
                 "c2_duration" = "Duration C2 (Niche Forums)",
                 "c3_duration" = "Duration C3 (Information Hubs)",
                 "info_duration" = "Duration Pol. Information",
                 "com_duration" = "Duration Pol. Communication",
                 "zeit_duration" = "Duration Zeit",
                 "bild_duration" = "Duration Bild",
                 "facebook_duration" = "Duration Facebook",
                 "twitter_duration" = "Duration Twitter",
                 "change_duration" = "Duration Change"
  ))

corr_d <- round(cor(corr_data_d, use = "pairwise.complete.obs"), 1)
p.mat_d <- cor_pmat(corr_data_d, use = "pairwise.complete.obs")

corr_d_plot <- ggcorrplot(corr_d, method = "circle", type = "upper", 
                          outline.col = "white",
                          title = "Bivariate Correlations - Duration", insig = "blank",
                          tl.cex = 8,
                          tl.srt = 90,
                          lab = T, lab_size = 3, show.legend = F)

corr_d_plot

#ggsave("output/corr_duration_plot.pdf", width = 10, height = 10, dpi = 300, corr_d_plot)

