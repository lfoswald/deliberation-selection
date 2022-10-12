###### Latent Profile Analysis - P1b  #######################

source("code/tracking_packages.R")

# data engagement with classes + info + com
user_data_deliberation <- read_excel("data/temp/user_data_deliberation_all.xlsx")

set.seed(123)

# estimate multiple models + compare profiles
fit_stats <- user_data_deliberation %>%
  ungroup()%>%
  mutate(online_clicks = log(online_clicks+1),
         pol_clicks = log(pol_clicks+1),
         public_br_clicks = log(c1_clicks+1),
         niche_f_clicks = log(c2_clicks+1),
         change_clicks = log(change_clicks+1),
         twitter_clicks = log(twitter_clicks+1),
         facebook_clicks = log(facebook_clicks+1),
         zeit_clicks = log(zeit_clicks+1),
         bild_clicks = log(bild_clicks+1)
         )%>%
  dplyr::select(online_clicks,change_clicks,twitter_clicks,zeit_clicks,facebook_clicks,bild_clicks,
                pol_clicks,public_br_clicks,niche_f_clicks)%>%
  scale() %>%
  single_imputation() %>%
  estimate_profiles(1:18, 
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) %>%
  compare_solutions(statistics = c("LogLik","AIC", "BIC", "Entropy"))

fit_stats$fits%>%
  dplyr::select(Classes, LogLik, AIC, BIC)%>%
  slice(1:18)%>%
  xtable()%>%
  kable("latex")

# estimate selected models
LPA_profiles <- user_data_deliberation %>%
  ungroup()%>%
  mutate(online_clicks = log(online_clicks+1),
         pol_clicks = log(pol_clicks+1),
         public_br_clicks = log(c1_clicks+1),
         niche_f_clicks = log(c2_clicks+1),
         change_clicks = log(change_clicks+1),
         twitter_clicks = log(twitter_clicks+1),
         facebook_clicks = log(facebook_clicks+1),
         zeit_clicks = log(zeit_clicks+1),
         bild_clicks = log(bild_clicks+1)
  )%>%
  dplyr::select(online_clicks,change_clicks,twitter_clicks,zeit_clicks,facebook_clicks,bild_clicks,
                pol_clicks,public_br_clicks,niche_f_clicks)%>%
  scale() %>%
  single_imputation() %>%
  estimate_profiles(n_profiles = 10, models = 1)

# plot profiles 
LPA_profiles_df <- data.frame(get_estimates(LPA_profiles))
LPA_profiles_df$Class <- as.factor(LPA_profiles_df$Class)
LPA_profiles_df <- LPA_profiles_df%>%
  filter(Category == "Means")%>%
  filter(Class == 1 | Class == 2 | Class == 4 | Class == 5 |
           Class == 6 | Class == 8 | Class == 9)%>%
  mutate(Class = dplyr::recode(Class, `1` = "Niche forum \n users", `2` = "Quality \n journalism",
         `4` = "Petition \n activists" , `5` = "Inactive" , `6` ="Political \n Twitter",
         `8` = "Bild readers", `9` = "Zeit readers"))%>%
  mutate(Parameter = dplyr::recode(Parameter, 
          `bild_clicks` = "Bild", `zeit_clicks` = "Zeit",
          `change_clicks` = "Change",`facebook_clicks` = "Facebook",
          `niche_f_clicks` = "Niche forums", `online_clicks` = "Overall",
          `public_br_clicks` = "Quality journalism", `twitter_clicks` = "Twitter",
          `pol_clicks` = "Political"))

p2 <- ggplot(LPA_profiles_df, aes(x = Parameter, y = Estimate, colour = Class))+
  geom_line(aes(group = Class))+
  geom_point()+
  geom_errorbar(aes(ymin = Estimate-se, ymax = Estimate+se), width = 0.2)+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')+
  scale_color_manual(labels = c("Niche forum users", "Quality journalism",
                                "Petition activists" , "Inactive" , "Political Twitter",
                                "Bild readers", "Zeit readers"),
                     values = c("#628395" , "#C5D86D", "#DB2763" , "grey40", 
                                "#17377A" ,"#FC471E" , "#55DDE0"))+
  facet_grid(~Class)+
  theme_light()+
  xlab("")+
  ggtitle("User Profiles & Engagement Patterns")+
  guides(color = "none")+
  coord_flip()

p2

#ggsave("output/profiles_small_multiples.pdf", height = 6, width = 10,dpi = 300, p2)

# get model fit
model_fit <- get_fit(LPA_profiles)

# assign people to profiles 
profiles <- get_data(LPA_profiles)

user_data_original <- read_excel("data/final/user_data_deliberation_P1b_V3.xlsx")

user_data_deliberation$user_class <- profiles$Class
user_data_deliberation$age <- user_data_original$age

# relative sizes of profiles
df <- data.frame(table(user_data_deliberation$user_class)/length(user_data_deliberation$user_class))
df%>%
  arrange(Freq)

#write_xlsx(user_data_deliberation, "data/final/user_data_deliberation_P1b_V3_profiles.xlsx")
