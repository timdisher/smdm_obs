source("./01_sim/01_trial_sim.R")
library(meta)
library(forcats)




df <- read_csv("./02_data/smdm_rct_nrs.csv") %>% mutate(design = as_factor(design))

ma_list <- list(
# Limit to rcts -------------------------------------------------------------- -
rcts = naive_ma(filter = c("rct"), data = df),

# Include high quality Obs --------------------------------------------------- -

rcts_high = naive_ma(filter = c("rct", "obs_high"), data = df),

# Include mod quality obs ---------------------------------------------------- -

rcts_mod = naive_ma(filter = c("rct", "obs_high", "obs_mod"), data = df),

# Include all quality obs ---------------------------------------------------- -

rcts_low = naive_ma(filter = c("rct", "obs_high", "obs_mod", "obs_low"), data = df)

)

#Quick forest plots for each ------------------------------------------------- -

map(ma_list, forest)

