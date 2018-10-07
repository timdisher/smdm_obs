source("./01_sim/01_trial_sim.R")
library(meta)
library(metafor)

start <- Sys.time()
nsims <- 1000
trt_effect <- 1.7

sim_list <- rep(list(NA), nsims)
for(i in 1:nsims){
  
 
  df <- trials() %>% mutate(design = factor(design, levels = c("rct", "obs_high",
                                            "obs_mod", "obs_low"))) # Simulate a set of trials

  ma_list = list(   
    # Limit to rcts -------------------------------------------------------------- -
    rcts = naive_ma(filter = c("rct")),
    
    # Include high quality Obs --------------------------------------------------- -
    
    rcts_high = naive_ma(filter = c("rct", "obs_high")),
    
    # Include mod quality obs ---------------------------------------------------- -
    
    rcts_mod = naive_ma(filter = c("rct", "obs_high", "obs_mod")),
    
    # Include all quality obs ---------------------------------------------------- -
    
    rcts_low = naive_ma(filter = c("rct", "obs_high", "obs_mod", "obs_low"))
    
  )    
    reg <- metareg(ma_list$rcts_low, ~ design)
    
  extract <- function(data){
   temp <- summary(data)$random[c("TE", "lower", "upper", "p")]
   t(do.call(rbind, temp)) %>% as.data.frame()
   }
   
  reg <- data.frame(TE = reg$beta,
             lower = reg$ci.lb,
             upper = reg$ci.ub,
             p = reg$pval) %>% slice(1) %>% mutate(scen = "reg") %>% 
    select(scen, everything())

  sim_list[[i]] <- map(ma_list, extract) %>% do.call(rbind, .) %>% 
    rownames_to_column(var = "scen") %>% rbind(reg)
  
}

full_sims <- do.call(rbind, sim_list) %>% mutate_at(vars(TE:upper), exp) %>%
  mutate(true = trt_effect,
         ror = TE/true,
         sig = p <= 0.05,
         cov = true >= lower & true <= upper)

end <- Sys.time()
(time <- end - start)

fct_order <- c("rcts", "rcts_high","rcts_mod", "rcts_low", "reg")

full_sims %>% mutate(scen = factor(scen, levels = fct_order)) %>% group_by(scen) %>% 
  summarise(ror = mean(ror),
              cov = mean(cov),
              power = ifelse(trt_effect > 1, mean(sig), NA),
              alpha = ifelse(trt_effect == 1, mean(sig), NA)) %>% arrange(scen)
  

