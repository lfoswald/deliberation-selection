###### Latent Profile Analysis - Split Sample Validation

source("code/tracking_packages.R")

set.seed(123)

# data engagement with classes + info + com
user_data_deliberation <- read_excel("data/temp/user_data_deliberation_all.xlsx")

# split sample validation 
user_data_deliberation <- sample_n(user_data_deliberation, 600)

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

get_estimates(LPA_profiles)

# plot profiles
LPA_profiles_df <- data.frame(get_estimates(LPA_profiles))
LPA_profiles_df$Class <- as.factor(LPA_profiles_df$Class)
LPA_profiles_df <- LPA_profiles_df%>%
  filter(Category == "Means")%>%
  filter(Class == 1 | Class == 2 | Class == 3 | Class == 4 |
           Class == 6 | Class == 7 | Class == 8)%>%
  mutate(Class = dplyr::recode(Class, `1` = "Facebook users", `2` = "Petition \n activists",
         `4` = "Average" , `6` = "Bild readers" , `3` ="Quality journalism \n esp. Zeit \n very active",
         `8` = "Inactive", `7` = "Quality journalism \n esp. Zeit \n less active"))%>%
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
  scale_color_manual(labels = c("Facebook", "Petition activists",
                                "Quality Journalism \n active" , "Average" , "Bild readers",
                                "Quality Journalism \n less active", "Inactive"),
                     values = c("#628395" , "#C5D86D", "#DB2763" , "grey40", 
                                "#17377A" ,"#FC471E" , "#55DDE0"))+
  facet_grid(~Class)+
  theme_light()+
  xlab("")+
  ggtitle("User Profiles & Engagement Patterns - Split Sample Validation")+
  guides(color = "none")+
  coord_flip()

p2

#ggsave("output/profiles_small_multiples_split.pdf", height = 6, width = 10, dpi = 300, p2)

model_fit <- get_fit(LPA_profiles)

# assign people to profiles
profiles <- get_data(LPA_profiles)

user_data_original <- read_excel("data/final/user_data_deliberation_P1b_V3.xlsx")

user_data_deliberation$user_class <- profiles$Class
user_data_deliberation$age <- user_data_original$age

# difference on other variables?
user_data_deliberation%>%
  group_by(user_class)%>%
  summarise(age = mean(age, na.rm=T),
            gender = mean(as.numeric(gender), na.rm=T),
            relationship = mean(as.numeric(relationship),na.rm=T),
            income = mean(hhincome, na.rm=T),
            knowledge = mean(resp_prop_correct, na.rm=T),
            pol_orient = mean(leftright, na.rm=T),
            nationalism = mean(national_pride, na.rm=T),
            trust = mean(politicians_care_efficacy, na.rm=T),
            understanding = mean(understand_efficacy, na.rm=T))

# post-hoc test regression models
## some more recoding / scaling etc. (simon comments)
user_data_deliberation <- user_data_deliberation%>%
  mutate(gender = as.numeric(gender),
         educ = as.factor(educ),
         user_class = as.factor(user_class),
         firstvote = as.factor(firstvote),
         relationship = as.numeric(relationship))

# gelman scale - divide by 2 standard deviations for better interpretability 
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

# use "average" profile as reference category
user_data_deliberation$user_class <- relevel(user_data_deliberation$user_class, ref = "10")

# regression models
a_mod <- lm(age ~ user_class, data=user_data_deliberation)
b_mod <- lm(gender ~ user_class, data=user_data_deliberation)
d_mod <- lm(hhincome ~ user_class, data=user_data_deliberation)
e_mod <- lm(national_pride ~ user_class, data=user_data_deliberation)
f_mod <- lm(politicians_care_efficacy ~ user_class, data=user_data_deliberation)
g_mod <- lm(understand_efficacy ~ user_class, data=user_data_deliberation)
h_mod <- lm(resp_prop_correct ~ user_class, data=user_data_deliberation)
i_mod <- lm(leftright ~ user_class, data=user_data_deliberation)

# regression results
a_modFrame <- data.frame(Variable = rownames(summary(a_mod)$coef),
                                Coefficient = summary(a_mod)$coef[, 1],
                                p = summary(a_mod)$coef[,4],
                                SE = summary(a_mod)$coef[, 2],
                         Covariates = "Age")[-1,] # [-1,] removes itercept, delete to include
b_modFrame <- data.frame(Variable = rownames(summary(b_mod)$coef),
                             Coefficient = summary(b_mod)$coef[, 1],
                             p = summary(b_mod)$coef[,4],
                             SE = summary(b_mod)$coef[, 2],
                         Covariates = "Gender (female)")[-1,] 
d_modFrame <- data.frame(Variable = rownames(summary(d_mod)$coef),
                         Coefficient = summary(d_mod)$coef[, 1],
                         p = summary(d_mod)$coef[,4],
                         SE = summary(d_mod)$coef[, 2],
                         Covariates = "Income")[-1,] 
e_modFrame <- data.frame(Variable = rownames(summary(e_mod)$coef),
                         Coefficient = summary(e_mod)$coef[, 1],
                         p = summary(e_mod)$coef[,4],
                         SE = summary(e_mod)$coef[, 2],
                         Covariates = "Support for nationalism")[-1,] 
f_modFrame <- data.frame(Variable = rownames(summary(f_mod)$coef),
                         Coefficient = summary(f_mod)$coef[, 1],
                         p = summary(f_mod)$coef[,4],
                         SE = summary(f_mod)$coef[, 2],
                         Covariates = "Trust in politicians")[-1,] 
g_modFrame <- data.frame(Variable = rownames(summary(g_mod)$coef),
                         Coefficient = summary(g_mod)$coef[, 1],
                         p = summary(g_mod)$coef[,4],
                         SE = summary(g_mod)$coef[, 2],
                         Covariates = "Political understanding")[-1,] 
h_modFrame <- data.frame(Variable = rownames(summary(h_mod)$coef),
                         Coefficient = summary(h_mod)$coef[, 1],
                         p = summary(h_mod)$coef[,4],
                         SE = summary(h_mod)$coef[, 2],
                         Covariates = "Political knowledge")[-1,] 
i_modFrame <- data.frame(Variable = rownames(summary(i_mod)$coef),
                         Coefficient = summary(i_mod)$coef[, 1],
                         p = summary(i_mod)$coef[,4],
                         SE = summary(i_mod)$coef[, 2],
                         Covariates = "Political orientation \n (conservative)")[-1,] 

                      
# Combine these data.frames
allModelFrame <- data.frame(rbind(a_modFrame, b_modFrame,
                                  d_modFrame,e_modFrame,f_modFrame,
                                  g_modFrame,h_modFrame,i_modFrame))

# Specify confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


allModelFrame <- allModelFrame%>%
  filter(Variable == "user_class1" |Variable == "user_class2" |
           Variable == "user_class3" |Variable == "user_class6" |Variable == "user_class8" |
           Variable == "user_class7")%>%
  mutate(Significance = ifelse(p < 0.05, "sig.", "insig."))

# plot 
cp1 <- ggplot(allModelFrame, aes(x = Variable,colour = Covariates, 
                                 shape=Covariates, alpha = Significance)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_colour_manual(values = c("#17377A","#C5D86D", "#DB2763","#FC471E","black",
                                 "#628395","turquoise","darkred"))+
  scale_shape_manual(values = c(3, 7, 16, 17, 15, 21,22,23))+
  guides(colour = guide_legend(override.aes = list(shape = c(3, 7, 16, 17, 15, 21,22,23))))+
  coord_flip() + 
  theme_bw()+ 
  ggtitle("Post hoc tests - User Profiles")+
  xlab("User Profile")+
  scale_x_discrete(labels = c("Facebook", "Petition activists",
                              "Quality Journalism \n active"  , "Bild readers",
                              "Quality Journalism \n less active", "Inactive"))

cp1

#ggsave("output/post_hoc_tests_relevant_highlighted_split.pdf", width = 6, height = 7, dpi = 300, cp1)
