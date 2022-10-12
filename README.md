# Examining Online Deliberation with URL Tracking Data
https://osf.io/preprints/socarxiv/yfmzh/

This repository consists of the following R scripts:

## Meta Scripts
- `tracking_packages.R` (installs relevant packages, called in various scripts)
- `scrape_forums.R` (was used to compile part of the dictionary (scraping the top forums in Germany) for the automated filtering of URLs for politically relevant topics)
- `deliberation_add_variables.R` (constructs scales - knowledge, efficacy and extremism - using item analyses and EFA to enrich survey data)

## 1. Preprocessing
`1_tracking_deliberation_preprocessing.R`
- includes the combination of URL tracking data with survey data 

`1_demography_descr.R`
- describe sample
- plot distribution of activity on map

`1_political_filtering.R`
- runs the automated filtering run to select politically relevant sites (run overnight!)
- groups selected clicks to domains for manual cross validation (really political content?) + coding of information, communication, participation

(Steps 2. belong to https://osf.io/preprints/socarxiv/68udg/ and can be found at https://osf.io/atj5u/)

## 3. Exploring User Characteristics with Digital Trace Data (Tip of the Iceberg Paper)

`3_tracking_deliberation_selection_prep.R`
- integrates LCA results with politically relevant clicks and overall tracking data
- compiles person level engagement metrics for classes and information/communication sites (duration, clicks)
- repeats procedure (individual egagement + regressions) for specific sites (Bild, Zeit, Facebook, Twitter, Change)

`3_tracking_regression_prep.R`
- includes external scales (efficiency and political knowledge)
- recodes variables / scales variables
- repeats procedure for specific domains (Bild, Zeit, Facebook, Twitter, Change)

`3_tracking_deliberation_selection.R`
- calculates multiple regression models to reveal potential selection mechanisms (classes vs. info/comm, clicks vs. duration)
- constructs coefficient plots

`3_tracking_deliberation_selection_domains.R`
- regression models and coefficient plots for specific sites (Bild, Zeit, Facebook, Twitter, Change)

`3_tracking_deliberation_engagement.R`
- creates barplots for users vs. non-users of a group of websites
- creates density plots (ridge plot) of engagement with different classes, info/com sites, and specific sites

`3_tracking_LPA_prep_corr_plots.R`
- merges all user-level engagement data
- constructs correlation plots (clicks & duration)

`3_tracking_deliberation_LPA.R`
- runs latent profile analysis (tidyLPA) and plots profiles
- enriches user level data with profile information 

`3_tracking_post-hoc_tests.R`
- explores differences between profiles with post-hoc tests

`3_tracking_deliberation_LPA_split_sample.R`
- runs latent profile analysis (tidyLPA) and plots profiles with split sample
- enriches user level data with profile information 
- explores differences between profiles with post-hoc tests

