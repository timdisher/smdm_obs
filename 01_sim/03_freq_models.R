#============================================================================= =
#
# Project: SMDM RCT + NRS half-day workshop
# Date: 2018-10-14
# Script: Frquentist models
#   - [X] m_1: Meta-regression (sub groups)
#   - [X] m_2: Variance inflation
#   - [X] m_3: Bias adjustment 
#   - [X] m_4: Variance inflation + bias adjustment
#   - [X] m_5: Three-level model 
#   - [X] m_6: THree-level model with variance inflation
#   - [X] m_7: Bias adjustment + variance inflation three-level model
#
#============================================================================= =
source("./01_sim/02_naive_analysis.R")


library(metafor) #For three-level models

# Working example data ------------------------------------------------------- -
#  - Full dataset
#  - Extract estimated treatment effects and standard error
#  - add inflation term (w) for models 2 and 2.1
#  - add bias term (b) for models 3 and 3.1
#    - Perfect knowledge of bias in example case
#    - TE biased by subtracting so we add the same amount back
ex <- df %>% mutate(TE = ma_list$rcts_low$TE,
                    seTE = ma_list$rcts_low$seTE,
                    se_w = case_when(design == "rct" ~ sqrt(seTE^2/1), #no inflation for RCTs
                                     design == "obs_high" ~ sqrt(seTE^2/0.8), # Inflate by 20%
                                     design == "obs_mod" ~ sqrt(seTE^2/0.5), # Inflate by 50%
                                     TRUE ~ sqrt(seTE^2/0.3)),
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
                    )

# Model 1: Meta-regression on design ----------------------------------------- -


m_1 <- metareg(ma_list$rcts_low, ~ design)


# Model 2: Deterministic variance inflation----------------------------------- -


m_2 <- metagen(TE, seTE = se_w, sm = "OR", data = ex)



# Model 3: Deterministic bias adjustment-------------------------------------- -


m_3 <- metagen(TE = te_bp, seTE, sm = "OR", data = ex)

# Model 4: Deterministic bias and variance adjustment------------------------- -


m_4 <- metagen(TE = te_bi, seTE = se_w, sm = "OR", data = ex)


# Model 5: Three-level model-------------------------------------------------- -


# Uses variance instead of standard error
m_5 <- rma.mv(TE, var, random = ~ 1 | design/study_id, data= ex)

# Model 6: Three-level with viariance inflation------------------------------- -


m_6 <- rma.mv(TE, var_w, random = ~ 1 | design/study_id, data= ex)


# Model 7: Three-level with bias adjustment------------------------------- -

m_7 <- rma.mv(te_bi, var_w, random = ~ 1 | design/study_id, data= ex)

