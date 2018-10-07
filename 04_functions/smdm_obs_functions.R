# sim_binom
# 
# Simulate a binomial outcome in a two armed trial
# 
# @param n = sample size per arm
# @param baseline_p = baseline probability (must be on [0,1] interval)
# @param trt_effect = treatment effect (OR)
# @param bias = bias on treatment effect (log scale)
# 
# @return = A data frame with the study information in winBUGS format
# 
# @examples
# 
# sim_binom(n = 150, 
#           baseline_p = 0.03,
#           trt_effect = 1.2,
#           bias = 0,
#           study_id = "douglas_2015",
#           design = "rct")
# 
sim_binom <- function(n, baseline_p, trt_effect, bias, study_id, design){
  
  arm2_log <- qlogis(baseline_p) + log(trt_effect) + bias
  arm2_p <- plogis(arm2_log)
  
  
  arm1 <- tibble(t = 0,
                 out = rbinom(n, 1, baseline_p))
  
  
  arm2 <- tibble(t = 1,
                 out = rbinom(n, 1, arm2_p))
  
  
  out <- bind_rows(arm1, arm2) %>% group_by(t) %>% 
    summarize(r = sum(out),
              n = n()) %>% 
    mutate(arm = 1:2,
           study_id = study_id) %>%
    gather(var, value, t:n) %>% 
    dcast(study_id ~ var + arm, value.var = "value") %>% mutate(design = design) %>%
    select(study_id, design, t_1, n_1, r_1, t_2, n_2, r_2)
  
  out
}


# trial_loop
# 
# Create multiple trials with desired variables
# 
# @param n
# @param baseline_p
# @param trt_effect
# @param bias
# @param study_id
# @param design
#
# @return
# Data frame of studies with the desired true variables
# 
# @examples
# study_id <- c("douglas_2015", "filteau_2011", "marchand_2013")
# n <- c(30, 150, 200)
# baseline_p = 0.03
# trt_effect = 1.2
# bias = 0
# design = "rct"
# trial_loop(study_id = study_id, n = n, baseline_p = baseline_p,
#            trt_effect = trt_effect, bias = bias, design = design)
trial_loop <- function(study_id, n, baseline_p, trt_effect, bias,
                       design){
  
  
  trial_list <- rep(list(NA), length(study_id))
  for(i in seq_along(study_id)){
    
    trial_list[[i]] <- sim_binom(n = n[[i]], baseline_p = baseline_p, trt_effect = trt_effect,
                                 bias = bias, study_id = study_id[[i]], design = design)  
    
  }
  
  out <- do.call(rbind, trial_list)
  
  out
}



# naive_ma
#
# Run naiive meta-analysis of RCT and NRS
# 
# @param data A dataframe
# @param filter A string to filter design by
# 
# @return A meta-analysis of the filtered data, including sub-group breakdowns
#   if multiple study types are included
# 
# @examples
# naiive_ma(data = df, filter = "rct")
naive_ma <- function(data = df, filter){
  
  filtered <- data %>% filter(design %in% filter)
  
  out <- metabin(event.e = r_2, n.e = n_2, event.c = r_1, 
                 n.c = n_1, studlab = study_id,
                 sm = "OR", byvar = design, data = filtered)
  
  out
}

