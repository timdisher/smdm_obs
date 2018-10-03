source("./01_sim/01_trial_sim.R")
library(meta)



set.seed(5678)
df <- trials() 


ma_list <- list(
# Limit to rcts -------------------------------------------------------------- -
rcts = naiive_ma(filter = c("rct")),

# Include high quality Obs --------------------------------------------------- -

rcts_high = naiive_ma(filter = c("rct", "obs_high")),

# Include mod quality obs ---------------------------------------------------- -

rcts_mod = naiive_ma(filter = c("rct", "obs_high", "obs_mod")),

# Include all quality obs ---------------------------------------------------- -

rcts_low = naiive_ma(filter = c("rct", "obs_high", "obs_mod", "obs_low"))

)

#Quick forest plots for each ------------------------------------------------- -

map(ma_list, forest)
