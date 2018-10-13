# Forest plot of all scenarios
source("./01_sim/04_bayes_models.R")
library(tidybayes)
library(forestplot)

models <- list(rcts = ma_list$rcts,
               all = ma_list$rcts_low, 
               metareg = m_1, 
               var = m_2, 
               bias = m_3,
               var_bias = m_4,
               three_lvl = m_5, 
               three_lvl_var = m_6, 
               three_lvl_bias_var = m_7, 
               bayes_bias = bayes_m1, 
               bayes_three_lvl_bias_var = bayes_m2)

fp_data <- rep(list(NA), length(models))
for(i in seq_along(fp_data)){
  
  if (i %in% c(1:2, 4:6)){
    fp_data[[i]] <- extract(models[[i]]) %>% mutate(model = names(models)[[i]]) %>%
      mutate_at(., vars(TE:upper), exp)
    
  }
  
  if (i %in% c(3, 7:9)){
    fp_data[[i]] <- extract_alt(models[[i]])%>% mutate(model = names(models)[[i]])%>%
      mutate_at(., vars(TE:upper), exp)
    
  }
  
  if (i %in% c(10,11)){
    
   fp_data[[i]] <- spread_draws(as.mcmc.list(models[[i]]$BUGSoutput),or[i,v]) %>% 
      median_qi(or) %>% ungroup() %>% rename(TE = or,
                               lower = .lower,
                               upper = .upper) %>% select(TE:upper) %>%
      mutate(p = NA,
             model = names(models)[[i]])
  }
}

fp_data <- do.call(rbind, fp_data)


model_names <- c("RCTs only", "All studies", "Meta-regression (m_1)",
                 "Variance inflation (m_2)", "Perfect Bias adjustment (m_3)",
                 "Imperfect bias adjustment with variance inflation (m_4)",
                 "Frequentist 3-level model (m_5)", 
                 "3-level model with variance inflation (m_6)",
                 "3-level model with imperfect bias adjustment with variance inflation",
                 "Bayesian perfect uncertain bias adjustment",
                 "Bayesian 3-level with uncertain imperfect bias adjustment")
tabletext <- cbind(c("Model",model_names),
                        c("Odds ratio (95% CI/CrI)",
                        paste(formatC(fp_data$TE, digits = 2, format = "f"), " (",
                              formatC(fp_data$lower, digits = 2, format = "f"), " to ",
                              formatC(fp_data$upper, digits = 2, format = "f"), ")", sep = "")))

forestplot(tabletext,
           mean = c(NA, fp_data$TE),
           lower = c(NA, fp_data$lower),
           upper = c(NA, fp_data$upper),
           new_page = TRUE,
           align = c("l","l"),
           is.summary=c(TRUE,rep(FALSE,length(fp_data$model))),
           xlog= TRUE,
           xticks = c(seq(from = 0.2, to = 5, by = 0.5)),
           txt_gp = forestplot::fpTxtGp(ticks = grid::gpar(cex = 1)),
           graphwidth = grid::unit(12, units = "cm"),
           #clip = c(-15, 15),
           grid = c(0.53),
           graph.pos = 2,
           boxsize = 0.3,
           mar = grid::unit(c(10,1,1,1), "mm"), #Increase bottom margin if "favours" clip
           vertices = FALSE,
           title = "Model Comparison",
           fn.ci_norm = forestplot::fpDrawCircleCI)
