source("./01_sim/01_trial_sim.R")
library(meta)



set.seed(42)
df <- trials() 


ma_list <- list(
# Limit to rcts -------------------------------------------------------------- -
rcts = naive_ma(filter = c("rct")),

# Include high quality Obs --------------------------------------------------- -

rcts_high = naive_ma(filter = c("rct", "obs_high")),

# Include mod quality obs ---------------------------------------------------- -

rcts_mod = naive_ma(filter = c("rct", "obs_high", "obs_mod")),

# Include all quality obs ---------------------------------------------------- -

rcts_low = naive_ma(filter = c("rct", "obs_high", "obs_mod", "obs_low"))

)

#Quick forest plots for each ------------------------------------------------- -

map(ma_list, forest)
