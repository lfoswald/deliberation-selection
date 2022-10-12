########### Selection - specific Domains ###################################

source("code/tracking_packages.R")

# load preprocessed data

user_data_deliberations <- read_excel("data/temp/user_data_deliberation_regr_2_special.xlsx")

### Regression models 

# clicks
zeit_c_mod <- lm(log(zeit_clicks+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                   demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
bild_c_mod <- lm(log(bild_clicks+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                   demsatis + leftright +  firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
facebook_c_mod <- lm(log(facebook_clicks+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                       demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                       dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
twitter_c_mod <- lm(log(twitter_clicks+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                      demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                      dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
change_c_mod <- lm(log(change_clicks+1) ~ gender + age + educ +relationship + hhincome + polinterest+ 
                     demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                     dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)

# duration
zeit_d_mod <- lm(log(zeit_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                   demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
bild_d_mod <- lm(log(bild_duration+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                   demsatis + leftright +  firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
facebook_d_mod <- lm(log(facebook_duration+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                       demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                       dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
twitter_d_mod <- lm(log(twitter_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                      demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                      dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)
change_d_mod <- lm(log(change_duration+1) ~ gender + age + educ +relationship + hhincome + polinterest+ 
                     demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                     dictator+ socialism+ national_pride+ resp_prop_correct+online, data=user_data_deliberations)


# collect model information into data frames 

# clicks
zeit_c_modFrame <- data.frame(Variable = rownames(summary(zeit_c_mod)$coef),
                              Coefficient = summary(zeit_c_mod)$coef[, 1],
                              SE = summary(zeit_c_mod)$coef[, 2],
                              Outcome = "Zeit")[-1,] # [-1,] removes itercept, delete to include
bild_c_modFrame <- data.frame(Variable = rownames(summary(bild_c_mod)$coef),
                              Coefficient = summary(bild_c_mod)$coef[, 1],
                              SE = summary(bild_c_mod)$coef[, 2],
                              Outcome = "Bild")[-1,] 
facebook_c_modFrame <- data.frame(Variable = rownames(summary(facebook_c_mod)$coef),
                                  Coefficient = summary(facebook_c_mod)$coef[, 1],
                                  SE = summary(facebook_c_mod)$coef[, 2],
                                  Outcome = "Facebook")[-1,] 
twitter_c_modFrame <- data.frame(Variable = rownames(summary(twitter_c_mod)$coef),
                                 Coefficient = summary(twitter_c_mod)$coef[, 1],
                                 SE = summary(twitter_c_mod)$coef[, 2],
                                 Outcome = "Twitter")[-1,] 
change_c_modFrame <- data.frame(Variable = rownames(summary(change_c_mod)$coef),
                                Coefficient = summary(change_c_mod)$coef[, 1],
                                SE = summary(change_c_mod)$coef[, 2],
                                Outcome = "Change")[-1,] 

# duration
zeit_d_modFrame <- data.frame(Variable = rownames(summary(zeit_d_mod)$coef),
                              Coefficient = summary(zeit_d_mod)$coef[, 1],
                              SE = summary(zeit_d_mod)$coef[, 2],
                              Outcome = "Zeit")[-1,] # [-1,] removes itercept, delete to include
bild_d_modFrame <- data.frame(Variable = rownames(summary(bild_d_mod)$coef),
                              Coefficient = summary(bild_d_mod)$coef[, 1],
                              SE = summary(bild_d_mod)$coef[, 2],
                              Outcome = "Bild")[-1,] 
facebook_d_modFrame <- data.frame(Variable = rownames(summary(facebook_d_mod)$coef),
                                  Coefficient = summary(facebook_d_mod)$coef[, 1],
                                  SE = summary(facebook_d_mod)$coef[, 2],
                                  Outcome = "Facebook")[-1,] 
twitter_d_modFrame <- data.frame(Variable = rownames(summary(twitter_d_mod)$coef),
                                 Coefficient = summary(twitter_d_mod)$coef[, 1],
                                 SE = summary(twitter_d_mod)$coef[, 2],
                                 Outcome = "Twitter")[-1,] 
change_d_modFrame <- data.frame(Variable = rownames(summary(change_d_mod)$coef),
                                Coefficient = summary(change_d_mod)$coef[, 1],
                                SE = summary(change_d_mod)$coef[, 2],
                                Outcome = "Change")[-1,] 


#########################################################################################

### Coefficient plots

# Specify confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# 1. Clicks

allModelFrame <- data.frame(rbind(zeit_c_modFrame, bild_c_modFrame, 
                                  facebook_c_modFrame, twitter_c_modFrame, change_c_modFrame))

allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online","firstvote6" ,
                                          "firstvote5", "firstvote4","firstvote3", "firstvote2","leftright",
                                          "socialism","dictator", "national_pride",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "demsatis","resp_prop_correct","polinterest",
                                          "relationship3","hhincome",
                                          "educ3","educ4","gender2","age"))

cp3 <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal8",limits=c("Zeit", "Bild","Facebook",
                                              "Twitter", "Change")) +
  scale_shape_manual(values = c(21, 22, 24, 16, 17),limits=c("Zeit", "Bild","Facebook",
                                                             "Twitter", "Change"))+
  scale_x_discrete(labels = c("socialism" = "Socialism",
                              "dictator" = "Dictator", 
                              "national_pride" = "National Pride",
                              "resp_prop_correct" = "Political \n Knowledge",
                              "politicians_care_efficacy" = "Efficacy \n (Trust)",
                              "understand_efficacy" = "Efficacy \n (Understanding)",
                              "polinterest" = "Political \n  Interest", 
                              "leftright" = "Political \n  Orientation",
                              "hhincome"  = "Household \n  Income",
                              "online" = "Online \n Activity",
                              "gender2" = "Female", 
                              "demsatis" = "Satisfaction with \n  Democracy",
                              "age" = "Age",
                              "relationship3" = "Single \n (vs. Relationship)",
                              "firstvote6" = "Afd",
                              "firstvote5" = "Linke",
                              "firstvote4" = "Grüne",
                              "firstvote3" = "FDP",
                              "firstvote2" = "SPD (vs. CDU)",
                              "educ3" = "Realschule",
                              "educ4" = "Abitur (vs. no degree \n or Hauptschule"))+
  coord_flip() + 
  theme_bw()+ 
  ggtitle("Selective Exposure - Specific Clicks")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16, 17)))) 

cp3

#ggsave("output/selection_cp3.pdf", width = 6, height = 12, dpi = 300, cp3)


#########################################################################################

# 2. Duration

allModelFramed <- data.frame(rbind(zeit_d_modFrame, bild_d_modFrame, 
                                   facebook_d_modFrame, twitter_d_modFrame, change_d_modFrame))

allModelFramed$Variable <- factor(allModelFramed$Variable, 
                                  levels=c("online","firstvote6" ,
                                           "firstvote5", "firstvote4","firstvote3", "firstvote2","leftright",
                                           "socialism","dictator", "national_pride",
                                           "politicians_care_efficacy","understand_efficacy", 
                                           "demsatis","resp_prop_correct","polinterest",
                                           "relationship3","hhincome",
                                           "educ3","educ4","gender2","age"))

cp4 <- ggplot(allModelFramed, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal8",limits=c("Zeit", "Bild","Facebook",
                                              "Twitter", "Change")) +
  scale_shape_manual(values = c(21, 22, 24, 16, 17),limits=c("Zeit", "Bild","Facebook",
                                                             "Twitter", "Change"))+
  scale_x_discrete(labels = c("socialism" = "Socialism",
                              "dictator" = "Dictator", 
                              "national_pride" = "National Pride",
                              "resp_prop_correct" = "Political \n Knowledge",
                              "politicians_care_efficacy" = "Efficacy \n (Trust)",
                              "understand_efficacy" = "Efficacy \n (Understanding)",
                              "polinterest" = "Political \n  Interest", 
                              "leftright" = "Political \n  Orientation",
                              "hhincome"  = "Household \n  Income",
                              "online" = "Online \n Activity",
                              "gender2" = "Female", 
                              "demsatis" = "Satisfaction with \n  Democracy",
                              "age" = "Age",
                              "relationship3" = "Single \n (vs. Relationship)",
                              "firstvote6" = "Afd",
                              "firstvote5" = "Linke",
                              "firstvote4" = "Grüne",
                              "firstvote3" = "FDP",
                              "firstvote2" = "SPD (vs. CDU)",
                              "educ3" = "Realschule",
                              "educ4" = "Abitur (vs. no degree \n or Hauptschule"))+
  coord_flip() + 
  theme_bw()+ 
  ggtitle("Selective Exposure - Engagement Duration")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16, 17)))) 

cp4

#ggsave("output/selection_cp4.pdf", width = 6, height = 12, dpi = 300, cp4)

# combine plots
combi_3 <- grid.arrange(cp3, cp4, nrow = 1)
#ggsave("output/combi_3_special.pdf", width = 12, height = 12, dpi = 300, combi_3 )

########################

# simplified in-text-model


########################

# simplified classes plot / main-text publication plot (duration)

zeit_d_mod <- lm(log(zeit_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                     demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                     dictator+ socialism+ national_pride+ resp_prop_correct , data=user_data_deliberations)
bild_d_mod <- lm(log(bild_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                 demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberations)
twitter_d_mod <- lm(log(twitter_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                 demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberations)
facebook_d_mod <- lm(log(facebook_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                 demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberations)
change_d_mod <- lm(log(change_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                 demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberations)


zeit_d_modFrame <- data.frame(Variable = rownames(summary(zeit_d_mod)$coef),
                              Coefficient = summary(zeit_d_mod)$coef[, 1],
                              p = summary(zeit_d_mod)$coef[,4],
                              SE = summary(zeit_d_mod)$coef[, 2],
                              Outcome = "Zeit")[-1,] # [-1,] removes itercept, delete to include
bild_d_modFrame <- data.frame(Variable = rownames(summary(bild_d_mod)$coef),
                              Coefficient = summary(bild_d_mod)$coef[, 1],
                              p = summary(bild_d_mod)$coef[,4],
                              SE = summary(bild_d_mod)$coef[, 2],
                              Outcome = "Bild")[-1,] 
facebook_d_modFrame <- data.frame(Variable = rownames(summary(facebook_d_mod)$coef),
                                  Coefficient = summary(facebook_d_mod)$coef[, 1],
                                  p = summary(facebook_d_mod)$coef[,4],
                                  SE = summary(facebook_d_mod)$coef[, 2],
                                  Outcome = "Facebook")[-1,] 
twitter_d_modFrame <- data.frame(Variable = rownames(summary(twitter_d_mod)$coef),
                                 Coefficient = summary(twitter_d_mod)$coef[, 1],
                                 p = summary(twitter_d_mod)$coef[,4],
                                 SE = summary(twitter_d_mod)$coef[, 2],
                                 Outcome = "Twitter")[-1,] 
change_d_modFrame <- data.frame(Variable = rownames(summary(change_d_mod)$coef),
                                Coefficient = summary(change_d_mod)$coef[, 1],
                                p = summary(change_d_mod)$coef[,4],
                                SE = summary(change_d_mod)$coef[, 2],
                                Outcome = "Change")[-1,] 


allModelFrame <- data.frame(rbind(zeit_d_modFrame, bild_d_modFrame, 
                                  facebook_d_modFrame, twitter_d_modFrame, change_d_modFrame))


allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online" , "afdvote","leftright",
                                          "socialism","dictator", "national_pride","demsatis",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "resp_prop_correct","polinterest",
                                          "hhincome", "abitur" ,"gender2","age"))

allModelFrame <- allModelFrame%>%
  mutate(Significance = ifelse(p < 0.5, "sig.", "insig."))

# plot duration info/com simplified
cp2_domains <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_jcolors(palette="pal8",limits=c("Zeit", "Bild","Facebook",
                                              "Twitter", "Change")) +
  scale_shape_manual(values = c(21,22, 24, 16,17),limits=c("Zeit", "Bild","Facebook",
                                                           "Twitter", "Change")) +
  scale_x_discrete(labels = c("socialism" = "Support for \n socialism",
                              "afdvote" = "AfD vote",
                              "dictator" = "Support for \n dictators", 
                              "national_pride" = "National pride",
                              "resp_prop_correct" = "Political \n knowledge",
                              "politicians_care_efficacy" = "Efficacy \n (trusting politicians)",
                              "understand_efficacy" = "Efficacy \n (understanding politics)",
                              "polinterest" = "Political \n  interest", 
                              "leftright" = "Political orientation \n (conservative)",
                              "hhincome"  = "Household \n  Income",
                              "gender2" = "Female", 
                              "demsatis" = "Satisfaction with \n  democracy",
                              "age" = "Age",
                              "online" = "Overall online \n engagement",
                              "abitur" = "Abitur"))+
  coord_flip() + 
  xlab("")+
  theme_bw()+ 
  ggtitle("Selective Exposure - Specific Domains")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16, 17)))) 

cp2_domains

#ggsave("output/selection_cp2_domains_reduced.pdf", width = 6, height = 8, dpi = 300, cp2_domains)



