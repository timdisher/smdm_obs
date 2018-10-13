source("./01_sim/01_trial_sim.R")
library(meta)
library(metafor)
library(beepr)

#Read in pre-made sims if available (Takes about 20 minutes total to run them)

# sims_b <- read.csv("./01_sim/operating_chars_power.csv")
# sims_a <- read.csv("./01_sim/operating_chars_alpha.csv")

#============================================================================= =
#
#
# Loop over analysis scenarios
#
#
#============================================================================= =

n_sims <- 1000

sims_b <- ma_sim(nsims = n_sims, true_effect = 1.7) # for power
write.csv(sims_b, file = "./01_sim/operating_chars_power.csv")
beep(sound = 8)

sims_a <- ma_sim(nsims = n_sims, true_effect = 1) # for alpha
write.csv(sims_a, file = "./01_sim/operating_chars_alpha.csv")

beep(3)
#============================================================================= =
#
#
# Calculate operating characteristics
#
#
#============================================================================= =


alpha <- sims_a %>% mutate(scen = as_factor(scen)) %>% group_by(scen) %>% 
  summarise(alpha = mean(sig)) %>% arrange(scen)

beta <- sims_b %>% mutate(scen = as_factor(scen)) %>% group_by(scen) %>% 
  summarise(ror = mean(ror),
            cov = mean(cov),
            power = mean(sig)) %>% arrange(scen)




model_levels = c("rcts", "rcts_high", "rcts_mod", "rcts_low",
                 "reg", "var", "bias", "var_bias",
                 "three_lvl", "three_lvl_var", "three_lvl_bias")


merged <- beta %>% 
  left_join(alpha, by = c("scen")) %>%
  mutate(scen = factor(scen, levels = model_levels )) %>% arrange(scen)

write.csv(merged, file = "./05_output_data/operating_chars.csv")
