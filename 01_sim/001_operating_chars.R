source("./01_sim/01_trial_sim.R")
library(meta)

start <- Sys.time()
nsims <- 100
trt_effect <- 1

sim_list <- rep(list(NA), nsims)
for(i in 1:nsims){
  
  df <- trials() # Simulate a set of trials
  
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
  
  extract <- function(data){
   temp <- summary(data)$random[c("TE", "lower", "upper", "p")]
   t(do.call(rbind, temp)) %>% as.data.frame()
   }
   
  sim_list[[i]] <- map(ma_list, extract) %>% do.call(rbind, .) %>% rownames_to_column(var = "scen")
  
}

trt_effect
full_sims <- do.call(rbind, sim_list) %>% mutate_at(vars(TE:upper), exp) %>%
  mutate(true = trt_effect,
         ror = TE/true,
         sig = p <= 0.05,
         cov = true >= lower & true <= upper)

end <- Sys.time()
(time <- end - start)

fct_order <- c("rcts", "rcts_high","rcts_mod", "rcts_low")

full_sims %>% mutate(scen = factor(scen, levels = fct_order)) %>% group_by(scen) %>% 
  summarise(ror = mean(ror),
              cov = mean(cov),
              power = ifelse(trt_effect > 1, mean(sig), NA),
              alpha = ifelse(trt_effect == 1, mean(sig), NA)) %>% arrange(scen)
  

