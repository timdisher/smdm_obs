# Based on Turner model (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2667303/)


# Lower and upper should capture a 67% confidence interval, i.e. belief that
# true bias adjustment is twice as likely to be in the interval than outside it
# If any estimates fall on right side of line (upper or both upper and lower)
# then take log(1/est) instead of log(est)
# High quality obs trials

lower_high = 0.8
upper_high = 1

u_high = (log(lower_high) + log(upper_high))/2
sd_high = (log(upper_high) - log(lower_high))/2 

hist(rnorm(1000, u_high, sd_high))

# Mod quality obs trials

lower_mod <- 0.4
upper_mod <- 0.8

u_mod = (log(lower_mod) + log(upper_mod))/2
sd_mod = (log(upper_mod) - log(lower_mod))/2

hist(rnorm(1000, u_mod, sd_mod))


# low quality obs trials

lower_low <- 0.3
upper_low <- 0.5

u_low = (log(lower_low) + log(upper_low))/2
sd_low = (log(upper_low) - log(lower_low))/2

hist(rnorm(1000, u_low, sd_low))


ex <- ex %>% mutate(se_wu = case_when(design == "rct" ~ seTE,
                                        design == "obs_high" ~ sqrt(seTE^2 + sd_high^2),
                                        design == "obs_mod" ~ sqrt(seTE^2 + sd_mod^2),
                                        TRUE ~ sqrt(seTE^2 + sd_low^2)),
            
                    te_bi = case_when(design == "rct" ~ TE + 0,
                                      design == "obs_high" ~ TE + u_high*-1,
                                      design == "obs_mod" ~ TE + u_mod*-1,
                                      TRUE ~ TE + u_low*-1),
                    var_wu = se_wu^2)

