### Post-hoc tests, LPA P1b

source("code/tracking_packages.R")

user_data_deliberation <- read_excel("data/final/user_data_deliberation_P1b_V3_profiles.xlsx")

# recoding 1
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

# recoding 2
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

# collect regression results
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


# Combine data.frames
allModelFrame <- data.frame(rbind(a_modFrame, b_modFrame,
                                  d_modFrame,e_modFrame,f_modFrame,
                                  g_modFrame,h_modFrame,i_modFrame))

# Specify confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# collect profiles to plot 
allModelFrame <- allModelFrame%>%
  filter(Variable == "user_class1" |Variable == "user_class2" |Variable == "user_class4" |
           Variable == "user_class5" |Variable == "user_class6" |Variable == "user_class8" |
           Variable == "user_class9")%>%
  mutate(Significance = ifelse(p < 0.05, "sig.", "insig."))

# plot 
pht1 <- ggplot(allModelFrame, aes(x = Variable,colour = Covariates, 
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
  scale_x_discrete(labels = c("Niche forum users", "Quality journalism",
                              "Petition activists" , "Inactive" , "Political Twitter",
                              "Bild readers", "Zeit readers"))

pht1

#ggsave("output/post_hoc_tests_relevant_highlighted.pdf", width = 6, height = 8, dpi = 300, pht1)
