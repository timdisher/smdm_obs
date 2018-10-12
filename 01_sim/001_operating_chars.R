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

n_sims <- 1000

sims_b <- ma_sim(nsims = n_sims, true_effect = 1.7) # for power
#write.csv(sims_b, file = "./01_sim/operating_chars_power.csv")
beep(sound = 8)

sims_a <- ma_sim(nsims = n_sims, true_effect = 1) # for alpha
#write.csv(sims_a, file = "./01_sim/operating_chars_alpha.csv")

beep(3)
#============================================================================= =
#
#
# Calculate operating characteristics
#
#
#============================================================================= =

op_chars <- . %>% mutate(scen = as_factor(scen)) %>% group_by(scen) %>% 
  summarise(ror = mean(ror),
            cov = mean(cov),
            opp = mean(sig)) %>% arrange(scen)


alpha <- sims_a %>% op_chars

beta <- sims_b %>% op_chars %>% select(-c(ror, cov))


merged <- alpha %>% left_join(beta, by = c("scen"), suffix = c("_alpha","_beta"))
