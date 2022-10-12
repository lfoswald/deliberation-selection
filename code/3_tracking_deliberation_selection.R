##################### P1b Deliberative Systems ##################################

source("code/tracking_packages.R")

########### P1b - Actual Person Level Analyses ###################################

user_data_deliberation <- read_excel("data/temp/user_data_deliberation_regr_2.xlsx")

#### Regression models

online_c_mod <- lm(log(online_clicks+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                     demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct, data=user_data_deliberation)
online_d_mod <- lm(log(online_duration+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                     demsatis + leftright +  firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct, data=user_data_deliberation)
pol_c_mod <- lm(log(pol_clicks+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
pol_d_mod <- lm(log(pol_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
c1_c_mod <- lm(log(c1_clicks+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
c1_d_mod <- lm(log(c1_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
c2_c_mod <- lm(log(c2_clicks+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
c2_d_mod <- lm(log(c2_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
c3_c_mod <- lm(log(c3_clicks+1) ~ gender + age + educ + relationship +hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct + online, data=user_data_deliberation)
c3_d_mod <- lm(log(c3_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                  demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
info_c_mod <- lm(log(info_clicks+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                 demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
info_d_mod <- lm(log(info_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                 demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
com_c_mod <- lm(log(com_clicks+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                   demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
com_d_mod <- lm(log(com_duration+1) ~ gender + age + educ + relationship + hhincome + polinterest+ 
                   demsatis + leftright + firstvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)

###################################################################################

# Coefficient Plots - Selective Exposure

# Specify confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


### 1. Coefficient plot (classes, clicks)

online_c_modFrame <- data.frame(Variable = rownames(summary(online_c_mod)$coef),
                          Coefficient = summary(online_c_mod)$coef[, 1],
                          SE = summary(online_c_mod)$coef[, 2],
                          Outcome = "Overall")[-1,] # [-1,] removes itercept, delete to include
pol_c_modFrame <- data.frame(Variable = rownames(summary(pol_c_mod)$coef),
                          Coefficient = summary(pol_c_mod)$coef[, 1],
                          SE = summary(pol_c_mod)$coef[, 2],
                          Outcome = "Political")[-1,] 
c1_c_modFrame <- data.frame(Variable = rownames(summary(c1_c_mod)$coef),
                             Coefficient = summary(c1_c_mod)$coef[, 1],
                             SE = summary(c1_c_mod)$coef[, 2],
                             Outcome = "Public Broadc.")[-1,] 
c2_c_modFrame <- data.frame(Variable = rownames(summary(c2_c_mod)$coef),
                             Coefficient = summary(c2_c_mod)$coef[, 1],
                             SE = summary(c2_c_mod)$coef[, 2],
                             Outcome = "Niche Forums")[-1,] 
c3_c_modFrame <- data.frame(Variable = rownames(summary(c3_c_mod)$coef),
                             Coefficient = summary(c3_c_mod)$coef[, 1],
                             SE = summary(c3_c_mod)$coef[, 2],
                             Outcome = "Mainstream")[-1,] 
info_c_modFrame <- data.frame(Variable = rownames(summary(info_c_mod)$coef),
                            Coefficient = summary(info_c_mod)$coef[, 1],
                            SE = summary(info_c_mod)$coef[, 2],
                            Outcome = "Information")[-1,] 
com_c_modFrame <- data.frame(Variable = rownames(summary(com_c_mod)$coef),
                            Coefficient = summary(com_c_mod)$coef[, 1],
                            SE = summary(com_c_mod)$coef[, 2],
                            Outcome = "Communication")[-1,] 

allModelFrame <- data.frame(rbind(online_c_modFrame, pol_c_modFrame,
                                  c1_c_modFrame,c2_c_modFrame,c3_c_modFrame))

# reorder variables (rev because coords are flipped)
allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online","firstvote6" ,
                                          "firstvote5", "firstvote4","firstvote3", "firstvote2","leftright",
                                          "socialism","dictator", "national_pride",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "demsatis","resp_prop_correct","polinterest",
                                          "relationship3","hhincome",
                                          "educ3","educ4","gender2","age"))
# Plot classes, clicks
cp1 <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2, shape = Outcome),
                             lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Overall", "Political","Public Broadc.",
                                              "Niche Forums", "Mainstream")) +
  scale_shape_manual(values = c(21, 22, 24, 16, 17),limits=c("Overall", "Political","Public Broadc.",
                                                             "Niche Forums", "Mainstream"))+
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
  ggtitle("Selective Exposure - Clicks")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16, 17)))) 

cp1

#ggsave("output/selection_cp1.pdf", width = 6, height = 12, dpi = 300, cp1)


##########################################################################################

# 2. Coefficient plot (information/communication, clicks)

allModelFrame <- data.frame(rbind(online_c_modFrame, pol_c_modFrame,
                                  info_c_modFrame,com_c_modFrame))

# reorder variables (rev because coords are flipped)
allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online","firstvote6" ,
                                          "firstvote5", "firstvote4","firstvote3", "firstvote2","leftright",
                                          "socialism","dictator", "national_pride",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "demsatis","resp_prop_correct","polinterest",
                                          "relationship3","hhincome","educ3","educ4","gender2","age"))

# plot information/communication, clicks
cp_ic <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Overall", "Political",
                                              "Information", "Communication")) +
  scale_shape_manual(values = c(21, 22, 24, 16, 17),limits=c("Overall", "Political",
                                                             "Information", "Communication")) +
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
  ggtitle("Selective Exposure - Clicks")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16)))) 

cp_ic

#ggsave("output/selection_cp_ic.pdf", width = 6, height = 12, dpi = 300, cp_ic)

# combine clicks plots
combi_1 <- grid.arrange(cp1, cp_ic, nrow = 1)
#ggsave("output/combi_1_clicks.pdf", width = 12, height = 12, dpi = 300, combi_1 )

######################################################################################

# 3. Coefficient Plot (duration, classes)

online_d_modFrame <- data.frame(Variable = rownames(summary(online_d_mod)$coef),
                                Coefficient = summary(online_d_mod)$coef[, 1],
                                p = summary(online_d_mod)$coef[,4],
                                SE = summary(online_d_mod)$coef[, 2],
                                Outcome = "Overall")[-1,]
pol_d_modFrame <- data.frame(Variable = rownames(summary(pol_d_mod)$coef),
                             Coefficient = summary(pol_d_mod)$coef[, 1],
                             SE = summary(pol_d_mod)$coef[, 2],
                             Outcome = "Political")[-1,]
c1_d_modFrame <- data.frame(Variable = rownames(summary(c1_d_mod)$coef),
                              Coefficient = summary(c1_d_mod)$coef[, 1],
                              SE = summary(c1_d_mod)$coef[, 2],
                              Outcome = "Public Broadc")[-1,]
c2_d_modFrame <- data.frame(Variable = rownames(summary(c2_d_mod)$coef),
                              Coefficient = summary(c2_d_mod)$coef[, 1],
                              SE = summary(c2_d_mod)$coef[, 2],
                              Outcome = "Niche Forums")[-1,]
c3_d_modFrame <- data.frame(Variable = rownames(summary(c3_d_mod)$coef),
                             Coefficient = summary(c3_d_mod)$coef[, 1],
                             SE = summary(c3_d_mod)$coef[, 2],
                             Outcome = "Mainstream")[-1,]
info_d_modFrame <- data.frame(Variable = rownames(summary(info_d_mod)$coef),
                            Coefficient = summary(info_d_mod)$coef[, 1],
                            p = summary(info_d_mod)$coef[,4],
                            SE = summary(info_d_mod)$coef[, 2],
                            Outcome = "Information")[-1,]
com_d_modFrame <- data.frame(Variable = rownames(summary(com_d_mod)$coef),
                            Coefficient = summary(com_d_mod)$coef[, 1],
                            p = summary(com_d_mod)$coef[,4],
                            SE = summary(com_d_mod)$coef[, 2],
                            Outcome = "Communication")[-1,]

allModelFramed <- data.frame(rbind(online_d_modFrame, pol_d_modFrame, 
                                  c1_d_modFrame, c2_d_modFrame, c3_d_modFrame))

# reorder variables
allModelFramed$Variable <- factor(allModelFramed$Variable, 
                                 levels=c("online","firstvote6" ,
                                          "firstvote5", "firstvote4","firstvote3", "firstvote2","leftright",
                                          "socialism","dictator", "national_pride",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "demsatis","resp_prop_correct","polinterest",
                                          "relationship3","hhincome",
                                        "educ3","educ4","gender2","age"))


# plot duration, classes

cp2 <- ggplot(allModelFramed, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                     lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                      lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Overall", "Political","Public Broadc.",
                                              "Niche Forums", "Mainstream")) +
  scale_shape_manual(values = c(21, 22, 24, 16, 17),limits=c("Overall", "Political","Public Broadc.",
                                                             "Niche Forums", "Mainstream"))+
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
  ggtitle("Selective Exposure - Duration")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16, 17)))) 

cp2

#ggsave("output/selection_cp2.pdf", width = 6, height = 12, dpi = 300, cp2)


# Coefficient Plot (information/communication, duration)

allModelFrame <- data.frame(rbind(online_d_modFrame, pol_d_modFrame,
                                  info_d_modFrame,com_d_modFrame))

allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online","firstvote6" ,
                                          "firstvote5", "firstvote4","firstvote3", "firstvote2","leftright",
                                          "socialism","dictator", "national_pride",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "demsatis","resp_prop_correct","polinterest",
                                          "relationship3","hhincome","educ3","educ4","gender2","age"))

cp2_ic <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                   fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Overall", "Political",
                                              "Information", "Communication")) +
  scale_shape_manual(values = c(21, 22, 24, 16),limits=c("Overall", "Political",
                                                             "Information", "Communication")) +
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
  ggtitle("Selective Exposure - Duration")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16)))) 

cp2_ic

#ggsave("output/selection_cp2_ic.pdf", width = 6, height = 12, dpi = 300, cp2_ic)

# combine duration plots
combi_2 <- grid.arrange(cp2, cp2_ic, nrow = 1)
#ggsave("output/combi_2_duration.pdf", width = 12, height = 12, dpi = 300, combi_2 )

#############################################################################################

# simplified info-com / main-text publication plot (duration)

online_d_mod <- lm(log(online_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                     demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                     dictator+ socialism+ national_pride+ resp_prop_correct , data=user_data_deliberation)
info_d_mod <- lm(log(info_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                   demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
com_d_mod <- lm(log(com_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                  demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)


online_d_modFrame <- data.frame(Variable = rownames(summary(online_d_mod)$coef),
                                Coefficient = summary(online_d_mod)$coef[, 1],
                                p = summary(online_d_mod)$coef[,4],
                                SE = summary(online_d_mod)$coef[, 2],
                                Outcome = "Overall engagement")[-1,]
info_d_modFrame <- data.frame(Variable = rownames(summary(info_d_mod)$coef),
                                Coefficient = summary(info_d_mod)$coef[, 1],
                                p = summary(info_d_mod)$coef[,4],
                                SE = summary(info_d_mod)$coef[, 2],
                                Outcome = "Political information")[-1,]
com_d_modFrame <- data.frame(Variable = rownames(summary(com_d_mod)$coef),
                                Coefficient = summary(com_d_mod)$coef[, 1],
                                p = summary(com_d_mod)$coef[,4],
                                SE = summary(com_d_mod)$coef[, 2],
                                Outcome = "Political communication")[-1,]

allModelFrame <- data.frame(rbind(online_d_modFrame,info_d_modFrame,com_d_modFrame))

allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online" , "afdvote1","leftright",
                                          "socialism","dictator", "national_pride","demsatis",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "resp_prop_correct","polinterest",
                                          "hhincome", "abitur" ,"gender2","age"))

allModelFrame <- allModelFrame%>%
  mutate(Significance = ifelse(p < 0.5, "sig.", "insig."))

# plot duration info/com simplified
cp2_ic <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Overall engagement", 
                                              "Political information", "Political communication")) +
  scale_shape_manual(values = c(21, 24, 16),limits=c("Overall engagement", 
                                                     "Political information", "Political communication")) +
  scale_x_discrete(labels = c("socialism" = "Support for \n socialism",
                              "afdvote1" = "AfD vote",
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
  ggtitle("Information vs. Communication Sites")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 24, 16)))) 

cp2_ic

#ggsave("output/selection_cp2_ic_reduced_raw.pdf", width = 6, height = 8, dpi = 300, cp2_ic)

########################

# simplified classes plot / main-text publication plot (duration)

online_d_mod <- lm(log(online_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                     demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                     dictator+ socialism+ national_pride+ resp_prop_correct , data=user_data_deliberation)
c1_d_mod <- lm(log(c1_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                   demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                   dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
c2_d_mod <- lm(log(c2_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                  demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                  dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)
c3_d_mod <- lm(log(c3_duration+1) ~ gender + age + abitur  +hhincome + polinterest+ 
                 demsatis + leftright +  afdvote + understand_efficacy+ politicians_care_efficacy+
                 dictator+ socialism+ national_pride+ resp_prop_correct+ online, data=user_data_deliberation)


online_d_modFrame <- data.frame(Variable = rownames(summary(online_d_mod)$coef),
                                Coefficient = summary(online_d_mod)$coef[, 1],
                                p = summary(online_d_mod)$coef[,4],
                                SE = summary(online_d_mod)$coef[, 2],
                                Outcome = "Overall engagement")[-1,]
c1_d_modFrame <- data.frame(Variable = rownames(summary(c1_d_mod)$coef),
                              Coefficient = summary(c1_d_mod)$coef[, 1],
                              p = summary(c1_d_mod)$coef[,4],
                              SE = summary(c1_d_mod)$coef[, 2],
                              Outcome = "Quality Local Information")[-1,]
c2_d_modFrame <- data.frame(Variable = rownames(summary(c2_d_mod)$coef),
                             Coefficient = summary(c2_d_mod)$coef[, 1],
                             p = summary(c2_d_mod)$coef[,4],
                             SE = summary(c2_d_mod)$coef[, 2],
                             Outcome = "Niche Forums")[-1,]
c3_d_modFrame <- data.frame(Variable = rownames(summary(c3_d_mod)$coef),
                            Coefficient = summary(c3_d_mod)$coef[, 1],
                            p = summary(c3_d_mod)$coef[,4],
                            SE = summary(c3_d_mod)$coef[, 2],
                            Outcome = "Mainstream Hubs")[-1,]

allModelFrame <- data.frame(rbind(online_d_modFrame,c1_d_modFrame,c2_d_modFrame,c3_d_modFrame))

allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("online" , "afdvote1","leftright",
                                          "socialism","dictator", "national_pride","demsatis",
                                          "politicians_care_efficacy","understand_efficacy", 
                                          "resp_prop_correct","polinterest",
                                          "hhincome", "abitur" ,"gender2","age"))

allModelFrame <- allModelFrame%>%
  mutate(Significance = ifelse(p < 0.5, "sig.", "insig."))

# plot duration info/com simplified
cp2_classes <- ggplot(allModelFrame, aes(x = Variable,colour = Outcome)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Overall engagement", 
                                              "Quality Local Information", "Niche Forums", "Mainstream Hubs")) +
  scale_shape_manual(values = c(21,22, 24, 16),limits=c("Overall engagement", 
                                                     "Quality Local Information", "Niche Forums", "Mainstream Hubs")) +
  scale_x_discrete(labels = c("socialism" = "Support for \n socialism",
                              "afdvote1" = "AfD vote",
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
  ggtitle("Latent Classes of Sites")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16)))) 

cp2_classes

#ggsave("output/selection_cp2_classes_reduced.pdf", width = 6, height = 8, dpi = 300, cp2_classes)

combi_reduced <- grid.arrange(cp2_classes, cp2_domains, cp2_ic, nrow = 1)
ggsave("output/combi_reduced.pdf", width = 18, height = 12, dpi = 300, combi_reduced  )

