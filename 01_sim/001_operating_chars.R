source("./01_sim/01_trial_sim.R")
library(meta)
library(metafor)
library(beepr)

#============================================================================= =
#
#
# Loop over analysis scenarios
#
#
#============================================================================= =
sims <- ma_sim(nsims = 100, true_effect = 1.7)
#beep(sound = 8)

#============================================================================= =
#
#
# Calculate operating characteristics
#
#
#============================================================================= =

sims %>% mutate(scen = as_factor(scen)) %>% group_by(scen) %>% 
  summarise(ror = mean(ror),
              cov = mean(cov),
              power = ifelse(trt_effect > 1, mean(sig), NA),
              alpha = ifelse(trt_effect == 1, mean(sig), NA)) %>% arrange(scen)
  

