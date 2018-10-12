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
naive_ma <- function(data = data, filter){
  
  filtered <- data %>% filter(design %in% filter)
  
  out <- metabin(event.e = r_2, n.e = n_2, event.c = r_1, 
                 n.c = n_1, studlab = study_id,
                 sm = "OR", byvar = design, data = filtered)
  
  out
}

# extract
# 
# Extracts relevant characteristics from meta results
extract <- function(data){
  temp <- summary(data)$random[c("TE", "lower", "upper", "p")]
  t(do.call(rbind, temp)) %>% as.data.frame()
}


# extract
# 
# Extracts relevant characteristics from metafor results
extract_alt <- function(data){
  
  out <- data.frame(TE = data$beta,
                    lower = data$ci.lb,
                    upper = data$ci.ub,
                    p = data$pval) %>% slice(1)
  
  out 
}

# ma_sim
# 
# Simulate operating characteristics for designs
# 
# 
ma_sim <- function(nsims, true_effect){

start <- Sys.time()
sim_list <- rep(list(NA), nsims)
for(i in 1:nsims){
  
  
  temp <- trials(trt_effect = true_effect) %>% mutate(design = factor(design, levels = c("rct", "obs_high",
                                                               "obs_mod", "obs_low")))
  
  effects <- naive_ma(filter = c("rct", "obs_high", "obs_mod", "obs_low"), data = temp)
  
   df <- temp %>% mutate(TE = effects$TE,
           seTE = effects$seTE,
           se_w = case_when(design == "rct" ~ sqrt(seTE^2/1), #no inflation for RCTs
                            design == "obs_high" ~ sqrt(seTE^2/0.8), # Inflate by 20%
                            design == "obs_mod" ~ sqrt(seTE^2/0.6), # Inflate by 40%
                            TRUE ~ seTE/0.5),
           te_bp = case_when(design == "rct" ~ TE + 0,
                             design == "obs_high" ~ TE + bias_obs_high*-1,
                             design == "obs_mod" ~ TE + bias_obs_mod*-1,
                             TRUE ~ TE + bias_obs_low*-1),
           
           te_bi = case_when(design == "rct" ~ TE + 0,
                             design == "obs_high" ~ TE + bias_obs_high*-1*0.8,
                             design == "obs_mod" ~ TE + bias_obs_mod*-1*0.6,
                             TRUE ~ TE + bias_obs_low*-1*0.5),
           var = seTE^2,
           var_w = se_w^2
    )# Simulate a set of trials
  
  ma_list = list(   
    # Limit to rcts -------------------------------------------------------------- -
    rcts = naive_ma(filter = c("rct"), data = df),
    
    # Include high quality Obs --------------------------------------------------- -
    
    rcts_high = naive_ma(filter = c("rct", "obs_high"), data = df),
    
    # Include mod quality obs ---------------------------------------------------- -
    
    rcts_mod = naive_ma(filter = c("rct", "obs_high", "obs_mod"), data = df),
    
    # Include all quality obs ---------------------------------------------------- -
    
    rcts_low = naive_ma(filter = c("rct", "obs_high", "obs_mod", "obs_low"), data = df),
    
    # Variance adjustment----------------------------------------------------- -
    var = metagen(TE, seTE = se_w, sm = "OR", data = df),
    
    # Bias adjustment--------------------------------------------------------- -
    bias = metagen(TE = te_bp, seTE, sm = "OR", data = df),
    
    # Variance and bias  adjustment------------------------------------------- -
    var_bias = metagen(TE = te_bi, seTE = se_w, sm = "OR", data = df)
    
  )    
  
  
  
  
  ma_sens <- list(
    
    reg = metareg(ma_list$rcts_low, ~ design),
    three_lvl = rma.mv(TE, var, random = ~ 1 | design/study_id, data= df,
                       control = list(iter.max = 10e8,
                                      rel.tol = 1e-7)),
    three_lvl_var = rma.mv(TE, var_w, random = ~ 1 | design/study_id, data= df,
                           control = list(iter.max = 10e8,
                                          rel.tol = 1e-7)),
    three_lvl_bias = rma.mv(te_bi, var_w, random = ~ 1 | design/study_id, data= df,
                            control = list(iter.max = 10e8,
                                           rel.tol = 1e-7))
  )
  
  
  alt_res <- map(ma_sens, extract_alt) %>% do.call(rbind, .) %>% 
    rownames_to_column(var = "scen")
  
  
  
  sim_list[[i]] <- map(ma_list, extract) %>% do.call(rbind, .) %>% 
    rownames_to_column(var = "scen") %>% rbind(alt_res)
  
}
  out <- do.call(rbind, sim_list) %>% mutate_at(vars(TE:upper), exp) %>%
    mutate(true = true_effect,
           ror = TE/true,
           sig = p <= 0.05,
           cov = true >= lower & true <= upper)
  
  end <- Sys.time()
  time <- end - start
  
  print(time)
  out
}
