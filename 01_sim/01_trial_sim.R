# install.packages(c("tidyverse", "reshape2", "meta", "metafor", "R2jags", "forestplot))


library(tidyverse)
library(reshape2)
source("./03_functions/smdm_obs_functions.R")
# Treatment 1 - Best available care
# Treatment 2 - NAP procedure
# Outcome = Neurodevelopmental impairment 

# Truth
baseline_p <- 0.05
trt_effect <- 1.7 # Odds of adverse event is increased by treatment
bias_rct <- 0 # RCTs are unbiased
bias_obs_high <- -0.08 # High quality NRS small bias from 1.7 to 1.62
bias_obs_mod <- -0.3 #  Decrease OR from 1.7 to 1.4 
bias_obs_low <- -0.7 #  Decrease OR from 1.7 to 1




trials <- function(trt_effect = trt_effect){
# Simulate RCTs -------------------------------------------------------------- -
rct_ids <- c("douglas_2008", "filteau_2006", "marchand_2009")
rct_size <- c(120, 80, 100)


rct_df <- trial_loop(study_id = rct_ids,
           n = rct_size,
           baseline_p = baseline_p,
           trt_effect = trt_effect,
           bias = bias_rct,
           design = "rct")

# Simulate high quality nrs -------------------------------------------------- -
obs_high_ids <- c("franklin_2018", "zee_2018")
obs_high_size <- c(700, 500)


obs_high_df <- trial_loop(study_id = obs_high_ids,
                     n = obs_high_size,
                     baseline_p = baseline_p,
                     trt_effect = trt_effect,
                     bias = bias_obs_high,
                     design = "obs_high")


# Simulate moderate quality nrs ---------------------------------------------- -
obs_mod_ids <- c("lil_2018", "branch_2015", "george_2014")
obs_mod_size <-c(700, 550, 930)


obs_mod_df <- trial_loop(study_id = obs_mod_ids,
                          n = obs_mod_size,
                          baseline_p = baseline_p,
                          trt_effect = trt_effect,
                          bias = bias_obs_mod,
                          design = "obs_mod")

# Simulate low quality nrs ---------------------------------------------- -
obs_low_ids <- c("jones_2011", "bradfield_2015", "blanchford_2015", "arnold_2011",
                  "lalonde_2012")
obs_low_size <- c(1000, 1200, 800, 950, 734)


obs_low_df <- trial_loop(study_id = obs_low_ids,
                          n = obs_low_size,
                          baseline_p = baseline_p,
                          trt_effect = trt_effect,
                          bias = bias_obs_low,
                          design = "obs_low")


# Merge into one dataframe --------------------------------------------------- -

bind_rows(rct_df, obs_high_df, obs_mod_df, obs_low_df)
}



 # smdm_data <- trials(trt_effect = 1.7)
 # write.csv(smdm_data, file = "./02_data/smdm_rct_nrs.csv", row.names = FALSE)
