#============================================================================= =
#
# Project: SMDM RCT + NRS half-day workshop
# Date: 2018-10-14
# Script: Bayesian models
#   - [X] Uncertain bias
#   - [X] Three-level uncertain bias
#
#============================================================================= =
#
source("./01_sim/03_freq_models.R")
library(R2jags)

bayes_df <- df %>% mutate(na = 2,
                          t_1 = 1,
                          t_2 = 2)


# Model 1: Uncertain bias adjustment------------------------------------------ -


m1_model <- "./04_models/rct_obs_bias.txt"
m1_binom_pars <- c("or", "sd", "totresdev")

m1_data_list = list(
  t = select(bayes_df, t_1, t_2) %>% as.matrix(),
  r = select(bayes_df, r_1, r_2) %>% as.matrix(),
  n = select(bayes_df, n_1, n_2) %>% as.matrix(),
  na = select(bayes_df, na)[[1]],
  nsRCT = nrow(df %>% filter(design == "rct")),
  nsOBShigh = nrow(df %>% filter(design == "obs_high")),
  nsOBSmod = nrow(df %>% filter(design == "obs_mod")),
  nsOBSlow = nrow(df %>% filter(design == "obs_low")),
  nt = 2,
  b_rct = bias_rct,
  b_obs_high_m = bias_obs_high,
  b_obs_high_p = 0.1^-2,
  b_obs_mod_m = bias_obs_mod,
  b_obs_mod_p = 0.1^-2,
  b_obs_low_m = bias_obs_low,
  b_obs_low_p = 0.1^-2
  
)

bayes_m1 <- jags.parallel(m1_data_list, NULL, m1_binom_pars,
              model.file = m1_model,
              n.chains = 3, n.iter = 60000, n.burnin = 40000, n.thin = 1)

#traceplot(bayes_m1$BUGSoutput)



# Model 2: Three-level with  bias adjustment---------------------------------- -


m2_model <- "./04_models/rct_obs_bias_3lvl.txt"
m2_binom_pars <- c("or", "sdOA", "totresdev")


df_rct <- filter(bayes_df, design == "rct")
df_obs_high <- filter(bayes_df, design == "obs_high")
df_obs_mod <- filter(bayes_df, design == "obs_mod")
df_obs_low <- filter(bayes_df, design == "obs_low")


m2_data_list = list(
  
#Overall parameters
b_rct = bias_rct,
b_obs_high_m = bias_obs_high*0.8,
b_obs_high_p = 0.1^-2,
b_obs_mod_m = bias_obs_mod*0.6,
b_obs_mod_p = 0.1^-2,
b_obs_low_m = bias_obs_low*0.5,
b_obs_low_p = 0.1^-2,
nt = 2, 

#RCTs
nsRCT = nrow(df_rct),
naRCT = select(df_rct, na)[[1]],
ntRCT = 2,
tRCT = select(df_rct, t_1, t_2) %>% as.matrix(),
rRCT = select(df_rct, r_1, r_2) %>% as.matrix(),
nRCT = select(df_rct, n_1, n_2) %>% as.matrix(),

#Obs high
nsOBShigh = nrow(df_obs_high),
naOBShigh = select(df_obs_high, na)[[1]],
ntOBShigh = 2,
tOBShigh = select(df_obs_high, t_1, t_2) %>% as.matrix(),
rOBShigh = select(df_obs_high, r_1, r_2) %>% as.matrix(),
nOBShigh = select(df_obs_high, n_1, n_2) %>% as.matrix(),

#Obs mod
nsOBSmod = nrow(df_obs_mod),
naOBSmod = select(df_obs_mod, na)[[1]],
ntOBSmod = 2,
tOBSmod = select(df_obs_mod, t_1, t_2) %>% as.matrix(),
rOBSmod = select(df_obs_mod, r_1, r_2) %>% as.matrix(),
nOBSmod = select(df_obs_mod, n_1, n_2) %>% as.matrix(),  

#Obs low
nsOBSlow = nrow(df_obs_low),
naOBSlow = select(df_obs_low, na)[[1]],
ntOBSlow = 2,
tOBSlow = select(df_obs_low, t_1, t_2) %>% as.matrix(),
rOBSlow = select(df_obs_low, r_1, r_2) %>% as.matrix(),
nOBSlow = select(df_obs_low, n_1, n_2) %>% as.matrix()
)

bayes_m2 <- jags.parallel(m2_data_list,NULL,m2_binom_pars,
                          model.file = m2_model,
                          n.chains = 3, n.iter = 80000, n.burnin = 40000, n.thin = 1)

#traceplot(bayes_m2$BUGSoutput)
