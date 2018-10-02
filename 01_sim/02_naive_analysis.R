source("./01_sim/01_trial_sim.R")
library(meta)


naiive <- function(data = df, filter){
  
  filtered <- data %>% filter(design %in% filter)
  
  out <- metabin(event.e = r_2, n.e = n_2, event.c = r_1, 
                 n.c = n_1, studlab = study_id,
                 sm = "OR", byvar = design, data = filtered)
  
  print(out)
  out
}

# data <- read_csv("./02_data/smdm_rct_nrs.csv") # For shared analysis
df <- trials() # For full simulation




ma_list <- list(
# Limit to rcts -------------------------------------------------------------- -
rcts = naiive(filter = c("rct")),

# Include high quality Obs --------------------------------------------------- -

rcts_high = naiive(filter = c("rct", "obs_high")),

# Include mod quality obs ---------------------------------------------------- -

rcts_mod = naiive(filter = c("rct", "obs_high", "obs_mod")),

# Include all quality obs ---------------------------------------------------- -

rcts_low = naiive(filter = c("rct", "obs_high", "obs_mod", "obs_low"))

)

#Quick forest plots for each ------------------------------------------------- -
map(ma_list, forest)





