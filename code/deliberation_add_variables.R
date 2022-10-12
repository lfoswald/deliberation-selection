# Build scales to add to selection models

source("code/tracking_packages.R")


# load survey data (for extremism and efficacy)
load("data/raw/ger_survey_2017_wide.RData")

# load knowledge data (kindly provided by Sebastian Ramirez-Ruiz)
load("data/raw/ger_knowledge_df.RData")

add_survey_items <- waves_df_wide%>%
  dplyr::select(personid, 
            # extremism (nationalism)
               extr_proud.2,extr_patriotism.2,extr_dictator.2,
            # extremism (socialism)
               extr_socialism.2,
            # political efficacy 
              # positive
               efficacy_iwellinformed.4,
               efficacy_iknowpolitics.4,
               efficacy_politicianscarepplikeme.4, 
              # negative
               efficacy_polquestionsdifficult.4 ,                  
               efficacy_problemscomplicated.4, 
               efficacy_partieswantvotes.4)

# recode
add_survey_items_rc <- add_survey_items%>%
  mutate(across(where(is.numeric), ~na_if(., 977)))%>%
  dplyr::rename_with(~str_remove(., '.2'))%>%
  dplyr::rename_with(~str_remove(., '.4'))%>%
  # reverse code negative efficacy items (5 point likert scale)
  mutate(efficacy_polquestionsdifficult.r = 6 - efficacy_polquestionsdifficult,
         efficacy_problemscomplicated.r = 6 - efficacy_problemscomplicated,
         efficacy_partieswantvotes.r = 6 - efficacy_partieswantvotes)


efficacy.df <- add_survey_items_rc%>%
  dplyr::select(efficacy_iwellinformed,
                efficacy_iknowpolitics,
                efficacy_politicianscarepplikeme, 
                efficacy_polquestionsdifficult.r ,                  
                efficacy_problemscomplicated.r, 
                efficacy_partieswantvotes.r)


extremism.df <- add_survey_items_rc%>%
  dplyr::select(extr_proud, extr_patriotism, extr_dictator,
                extr_socialism)

# itemanalysis
sjt.itemanalysis(efficacy.df, show.shapiro = TRUE)
sjt.itemanalysis(extremism.df, show.shapiro = TRUE)



####### EFFICACY ########################
# Exploratory Factor Analysis

# Parallel analysis
PFX <- fa.parallel(efficacy.df,fa="fa")

# Scree plot
efficacy.df.nona <- data.frame(na.omit(efficacy.df))
fit <- princomp(efficacy.df.nona, cor=TRUE)
summary(fit) 
plot(fit,type="lines") 
loadings(fit)

# Force 3 factor solution
PFA2 <- fa(efficacy.df,3) 
PFA2
fa.diagram(PFA2)

# final scales (efficacy)
add_survey_items_rc <- add_survey_items_rc%>%
  mutate(understand_efficacy = (efficacy_iwellinformed + efficacy_iknowpolitics + efficacy_polquestionsdifficult.r)/3,
         politicians_care_efficacy = (efficacy_politicianscarepplikeme + efficacy_partieswantvotes.r)/2)
# drop "problems in the world are too complicated to be solved by politics."


####### EXREMISM ########################
# Exploratory Factor Analysis

# Parallel analysis
PFX <- fa.parallel(extremism.df,fa="fa")

# Scree plot
extremism.df.nona <- data.frame(na.omit(extremism.df))
fit <- princomp(extremism.df.nona, cor=TRUE)
summary(fit) 
plot(fit,type="lines") 
loadings(fit)

# Force 2 factor solution
PFA2 <- fa(extremism.df,2) 
PFA2
fa.diagram(PFA2)

# final scales (extremism)
add_survey_items_rc <- add_survey_items_rc%>%
  mutate(national_pride = (extr_proud + extr_patriotism)/2,
         dictator = extr_dictator,
         socialism = extr_socialism)

#### save extra df to merge with user data ######
extr_effic_extra_scales <- add_survey_items_rc%>%
  dplyr::select(personid, understand_efficacy, politicians_care_efficacy,
                national_pride, dictator, socialism)
#saveRDS(extr_effic_extra_scales, file="extr_effic_extra_scales_P1.Rda")


##### KNOWLEDGE ############################
# total, factual, elite(text), elite(picture), event
extr_knowledge_scales <- ger_knowledge_df%>%
  dplyr::select(personid, resp_prop_correct, resp_prop_correct_factual,
                resp_prop_correct_elite_text,resp_prop_correct_elite_pic,
                resp_prop_correct_event)

sjt.itemanalysis(extr_knowledge_scales[,-1], show.shapiro = TRUE)
# keep together

#saveRDS(extr_knowledge_scales, file="extr_knowledge_scales_P1.Rda")
