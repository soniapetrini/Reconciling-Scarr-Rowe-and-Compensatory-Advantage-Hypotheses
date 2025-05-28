setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/algorithmic fairness")
source("code/funs.R")


# Simulation for PPV and NPV behavior in Polygenic Prediction across SES groups
# Based on the paper "Algorithmic Fairness in Polygenic Prediction"

library(tidyverse)
library(pROC)
library(caret)

# Set seed for reproducibility
set.seed(123)





################# SIMULATION FUNCTION ################# 

# Function to generate simulated data
simulate_data <- function(
    
  n_samples          = 5000,
  baseline_education = 12,
  heterosk           = T,
  pgi_effect         = 0.7,      # Base effect of PGI on education
  SES_effect         = 1.6,      # Base effect of SES on education
  interaction_effect = 0.57,     # Strength of SES*PGI interaction
  ses_split          = 0.5       # Proportion of high SES individuals
  
) {
  
  ###### INDEP VARIABLES ###### 
  
  # Generate SES (binary: high=1, low=0)
  SES <- rbinom(n_samples, 1, ses_split)
  
  # Generate continuous PGI score (normally distributed)
  pgi_education <- rnorm(n_samples, 0, 1)
  
  # Generate birth year (centered around 1960 with 20 year SD)
  birth_year <- round(rnorm(n_samples, 1939, 0.5))
  
  # Generate sex (binary: female=1, male=0)
  sex <- rbinom(n_samples, 1, 0.5)
  
  # Generate 10 genetic principal components for control
  genetic_pcs <- matrix(rnorm(n_samples * 10), ncol = 10) 
  colnames(genetic_pcs) <- paste0("pc", 1:10)
  
  
  
  
  ###### EDUCATION ###### 
  
  education <- baseline_education + (pgi_effect*pgi_education) + (SES_effect*SES) + (interaction_effect*pgi_education*SES)
  
  # Add control effects
  education <- education + (0.55 * (birth_year-1939)) + (0.83 * sex)
  
  # Create the data frame
  df <- data.frame(
    education = education,
    pgi_education = pgi_education,
    SES = SES,
    sex = sex,
    birth_year = birth_year,
    genetic_pcs
  ) 
  
  
  ###### NOISE ###### 
  
  # Get indices for noise
  df <- df %>%
    group_by(SES) %>%
    arrange(SES,pgi_education) %>%
    mutate(i = row_number()) %>% ungroup()
  
  # Add noise depending on SES and PGI
  set.seed(123)
  if (heterosk) {
    df <- df %>%
      arrange(SES,pgi_education) %>%
      mutate(sigma = ifelse(SES==0, i^0.5, (n_samples*ses_split-i)^0.5),
             noise = rnorm(n_samples,0,sqrt(sigma))) %>%
      mutate(education = education + noise) %>% 
      select(-sigma, -i, -noise)
  } else {
    noise = rnorm(n_samples, 0, 1.5)
    education = education + noise
    
  }
  
  ###### FLOOR AND CEILING ###### 
  
  # Add Floor and Ceiling effects
  df <- df %>% 
    mutate(education = ifelse(education<12,12, education)) %>%
    mutate(education = ifelse(education>20,20, education)) 
  
  # Add labels
  df <- df %>%
    mutate(SES = factor(SES,levels = c(1,0), labels = c("high SES","low SES")))
  
  
  return(df)
  
}











################# RUN SIMULATIONS ################# 


# -- TEST REGRESSION --

# Parameters grid -------
heterosk <- F
base_EA_grid <- if(heterosk) seq(9,12,0.5) else seq(11,13,0.5)
inter_effect <- c(0.6)

# Default Parameters Simulation  -------
sim_df   <- simulate_data(heterosk = heterosk)
data     <- dichotomize(sim_df, resid=T)
data$SES <- factor(data$SES, levels = c("low SES", "high SES"))

# Simple OLS regression  -------
formula <- as.formula("education ~ pgi_education + sex + birth_year + SES*pgi_education")
model_ols <- lm(formula, data = data)
summary(model_ols)
model_ols$coefficients




# -- VARYING PREVALENCE --

rows <- lapply(base_EA_grid, function(base_EA) {
    
    # simulate data
    sim_df <- simulate_data(baseline_education = base_EA,
                            interaction_effect = inter_effect,
                            heterosk = heterosk)
    
    # compute prevalence
    prevs <- compute_prevalence(sim_df)
    
    results <- lapply(METRICS, compute_group_metrics, data=sim_df)
    results <- bind_rows(results)
    
    # compute metrics
    results %>% 
      mutate(interaction=inter_effect,
             prev_lowSES=prevs[names(prevs)=="low SES"],
             prev_highSES=prevs[names(prevs)=="high SES"]) %>% 
      mutate(base_EA=base_EA)
})
data_plot <- bind_rows(rows)

data_plot <- data_plot %>% 
  mutate(metric = factor(metric, levels = c("NPV","PPV"), labels = metrics.labs))






# plot: evolution ---------

dup_labels <- function(x) data_plot$prev_highSES[match(x, data_plot$prev_lowSES)]
levels.labs <- c("high SES" = "#2196F3", "low SES" = "#F44336")

ggplot(data_plot) + 
  geom_line(aes(x=prev_lowSES, y=value, color=group),
            linewidth=1) +
  geom_ribbon(aes(x=prev_lowSES, y=value, fill=group, 
                  ymin=ci_lower,ymax=ci_upper),alpha=0.3) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_vline(xintercept = 0.26, linetype = "dashed",color="red", linewidth=2) +
  theme_minimal() +
  #scale_x_continuous(breaks=unique(data_plot$prev_lowSES), 
  #                   sec.axis = dup_axis(
  #                     name = "high SES prevalence of high EA",
  #                     labels = dup_labels), 
  #) +
  xlab("EA prevalence (low-SES)") +
  facet_wrap(~metric) +
  scale_color_manual(values = levels.labs) +
  scale_fill_manual(values = levels.labs) +
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    axis.title.x = element_text(margin = margin(t = 20)),       # Bottom x-axis title spacing
    #axis.title.x.top = element_text(margin = margin(b = 20)),   # Top x-axis title spacing
    axis.title.y = element_text(margin = margin(r = 20))        # Y-axis title spacing
  ) +
  ylim(c(0,1))



het_lab <- ifelse(heterosk,"_heterosk","_homosk")
ggsave(paste0("plots/simulation",het_lab,".png"), width = 20, height = 15)







# Parameters grid -------
base_EA_grid <- 11:13
interaction_grid <- c(0.6)
heterosk <- F

rows <- lapply(base_EA_grid, function(base_EA) {
  rows <- lapply(interaction_grid, function(inter_effect) {
    
    # simulate data
    sim_df <- simulate_data(baseline_education = base_EA,
                            interaction_effect = inter_effect,
                            heterosk = heterosk)
    
    # compute prevalence
    prevs <- sapply(c("high SES","low SES"), function(g) {
      group_df <- sim_df %>% filter(SES == g)
      group_df <- group_df %>%
        create_predictor() %>%
        group_by(edu_real) %>%
        summarize(count = n()) %>% 
        mutate(prev = count/nrow(group_df))
      prevalence=round(group_df$prev[group_df$edu_real==1],2)
      paste0(g,":",prevalence,"  ")
      prevalence
    })
    
    # compute metrics
    data_plot_metrics(sim_df) %>% 
      mutate(interaction=inter_effect,
             prev_lowSES=prevs[names(prevs)=="low SES"],
             prev_highSES=prevs[names(prevs)=="high SES"])
  })
  do.call(rbind, rows) %>% mutate(base_EA=base_EA)
})
data_plot <- do.call(rbind, rows)

data_plot <- data_plot %>% 
  mutate(metric = factor(metric, labels = metrics.labs))






# plot: evolution ---------

# find change in significance 
sig_change <- data_plot %>% 
  arrange(metric, prev_lowSES, interaction) %>% 
  group_by(metric, prev_lowSES) %>% 
  mutate(has_stars    = ifelse(stars=="",0,1)) %>%
  group_by(metric, prev_lowSES, has_stars) %>%
  mutate(row_number = row_number()) %>%
  group_by(metric, prev_lowSES) %>%
  mutate(first_change = 
           ifelse(row_number==max(row_number) & row_number!=14,T,F)) %>%
  mutate(first_change_x = ifelse(first_change,interaction,NA))
         

# plot
ggplot(data_plot) + 
  geom_line(aes(x=interaction, y=value, color=group),
            linewidth=2) +
  geom_ribbon(aes(x=interaction, y=value, fill=group, 
                  ymin=ci_lower,ymax=ci_upper),alpha=0.3) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  facet_grid(metric~prev_lowSES, scales="free_y") +
  #geom_vline(data = sig_change,
  #           aes(xintercept = first_change_x),
  #           linetype = "dashed", color = "red") +
  theme_minimal()


ggsave(paste0("plots/simulation.pdf"), width = 20, height = 15)


















