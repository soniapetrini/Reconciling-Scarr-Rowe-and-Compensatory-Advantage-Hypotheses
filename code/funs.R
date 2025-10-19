rm(list=ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


# =========================================================
#                      ✨ RECONCILING HYPOTHESES ✨
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(ggplot2)
  library(magrittr)
  library(tidyverse)
  library(purrr)
  library(ggpubr)
  library(knitr)
  library(kableExtra)
  library(xtable)
  library(PRROC)
  library(magrittr)
  library(ROSE)
  library(sjPlot)
  library(caret)
  library(lmtest)
  library(ggeffects)
  library(ggmosaic)
  library(boot)
  library(glue)  
  library(parallel)
  library(readxl)
  library(modelsummary)
})



# =========================================================
#                      ✨ GLOBALS ✨
# =========================================================
source("code/AH_results.R")

data_dir = "~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/data/"
SES_vars = c("father_edu", "father_occu")
DEMO     = c("birth_year", "sex")
PC_vars  = paste0("PC",seq(1:20))
METRICS  = c("NPV","PPV")
DATASETS = c("WLS","ELSA","Add Health")
OUTCOMES = c("education","high_school","college_enroll","college","graduate_school")

# PGI thresholding 
bottom_pgi    <- 0.40
top_pgi       <- 0.60
threshold.set <- c(0.2,0.4,0.6,0.8)



# =========================================================
#                      ✨ LABELS ✨
# =========================================================

SES.colors <- c("Low SES"  = "#F0AE05", "High SES" = "#52d8da")

metrics.labs <- c("NPV" = "Negative Predictive\nValue",
                  "PPV" = "Positive Predictive\nValue",
                  "TNR" = "P (Low PGI)",
                  "FNR" = "p (underachieve)",
                  "FPR" = "p (overachieve)",
                  "TPR" = "P (High PGI)"
                  )


outcome.labs <- c("education"       = "Educational attainment",
                  "cognitive"       = "Cognitive ability",
                  "high_school"     = "High School",
                  "college"         = "College",
                  "graduate_school" = "Graduate completed"
                  )

neg.outcome.labs <- c("education"       = "Low Educational attainment",
                      "cognitive"       = "Low Cognitive ability",
                      "high_school"     = "No High School",
                      "college"         = "No College",
                      "graduate_school" = "Graduate not completed"
                      )






## =========================================================
##                      ✨ FUNCTIONS ✨
## =========================================================


# ~~~~~ 🌿 DATA PREPARATION ~~~~~

negative_to_na <- function(x) if_else(x<0, NA, x, missing = NA)


get_data <- function(ds) readRDS(paste0("data/",ds,"/df.rds"))


add_stars <- function(p_values) {
  # add significance stars
  ifelse(p_values < 0.001, "***",
         ifelse(p_values < 0.01, "**",
                ifelse(p_values < 0.05, "*", "")))
}


dichotomize <- function(data, outcome, predictor, fun_pred, fun_out=NULL, resid=T) {
  
  # - Residualise out of PGIs
  if (resid) {
    # Add control variables
    PC_present = PC_vars[PC_vars %in% names(data)]
    pcs        = paste(PC_present, collapse = " + ")
    demo       = paste(DEMO,    collapse = " + ")
    
    if (length(unique(data$sex))==1) {
      demo = "birth_year"
    }
    
    # Run regression
    formula <- as.formula(paste(predictor," ~", pcs, " + ", demo))
    model   <- lm(formula, data = data)
    # Residualise and scale
    data[predictor] = as.vector(scale(residuals(model), center = FALSE))
  } else {
    # Scale
    data[predictor] = as.vector(scale(data[predictor], center = FALSE))
  }
  
  
  # - PREDICTOR
  if (fun_pred=="no middle") {
    # If middle 20% is removed
    data %<>% mutate(
      breaks = cut(get(predictor), 
                   breaks = quantile(get(predictor), 
                                     probs = c(0,bottom_pgi,top_pgi,1)), 
                   include.Lowest = TRUE, labels = FALSE)
    )
    # Create binary
    data %<>% mutate(high_PRED = case_when(breaks == 1 ~ 0,
                                           breaks == 2 ~ NA,
                                           breaks == 3 ~ 1))
  } else if (is.numeric(fun_pred)) {
    # If a quantile is given
    data %<>% mutate(
      breaks = cut(get(predictor), 
                   breaks = quantile(get(predictor), 
                                     probs = c(0,fun_pred,1)), 
                   include.Lowest = TRUE, labels = FALSE))
      
    # Create binary
    data %<>% mutate(high_PRED = case_when(breaks == 1 ~ 0,
                                           breaks == 2 ~ 1))
  }
  
  

  # - OUTCOME
  
  # Create binary
  if (all(data[[outcome]] %in% c(0, 1)))  {
    # If a binary variable is given
    data %<>% mutate(high_OUT = get(outcome))
    
  } else if (fun_out=="no middle") {
    # If middle 20% is removed
    data %<>% mutate(
      breaks = cut(get(outcome), 
                   breaks = quantile(get(outcome), probs = c(0,0.4,0.6,1)), 
                   include.Lowest = TRUE, labels = FALSE)
    )
    # Create binary
    data %<>% mutate(high_OUT = case_when(breaks == 1 ~ 0,
                                          breaks == 2 ~ NA,
                                          breaks == 3 ~ 1))
 } else if (is.numeric(fun_out)) {
   # If a numeric threshold is given (adjust classes if necessary)
   data %<>% mutate(high_OUT = ifelse(get(outcome) >= fun_out, 1, 0))
   
 } else if (!all(data[[outcome]] %in% c(0, 1)) & is.function(fun_out)) {
   # If a function is given and outcome is not binary
   fun_out = get(fun_out)
   thresh  = fun_out(data[[outcome]])
   # Create binary
   data %<>% mutate(high_OUT = ifelse(get(outcome) > thresh, 1, 0))
   
 }
  
  # Return data
  data %<>% na.omit(data)
}




# ~~~~~ 📊 COMPUTATIONS ~~~~~

calculate_metrics <- function(data, metric) {
  
  # Get two vectors
  EA  <- data[["high_OUT"]]
  PGI <- data[["high_PRED"]]
  
  if (metric == "PPV") {
    # Calculate positive predictive value (precision)
    high_PRED_high_OUT <- sum(PGI == 1 & EA == 1)
    high_PRED          <- sum(PGI == 1)
    metric_value       <- high_PRED_high_OUT / high_PRED
    num   <- high_PRED_high_OUT
    denom <- high_PRED
    
  } else if (metric == "NPV") {
    # Calculate negative predictive value
    Low_PGI_Low_EA <- sum(PGI == 0 & EA == 0)
    Low_PGI        <- sum(PGI == 0)
    metric_value   <- Low_PGI_Low_EA / Low_PGI
    num   <- Low_PGI_Low_EA
    denom <- Low_PGI
    
  } else if (metric == "TPR") {
    # Calculate true positive rate
    high_PRED_high_OUT <- sum(PGI == 1 & EA == 1)
    high_OUT        <- sum(EA == 1)
    metric_value   <- high_PRED_high_OUT / high_OUT
    num   <- high_PRED_high_OUT
    denom <- high_OUT
    
  } else if (metric == "TNR") {
    # Calculate true negative rate
    Low_PGI_Low_EA <- sum(PGI == 0 & EA == 0)
    Low_EA         <- sum(EA == 0)
    metric_value   <- Low_PGI_Low_EA / Low_EA
    num   <- Low_PGI_Low_EA
    denom <- Low_EA
    
  } else if (metric == "FNR") {
    # Calculate false negative rate
    high_PRED_low_OUT <- sum(PGI == 0 & EA == 1)
    low_OUT           <- sum(EA == 1)
    metric_value      <- high_PRED_low_OUT / low_OUT
    num <- high_PRED_low_OUT
    denom <- low_OUT
    
  } else if (metric == "FPR") {
    # Calculate false positive rate
    high_PRED_low_OUT <- sum(PGI == 1 & EA == 0)
    low_OUT           <- sum(EA == 0)
    metric_value      <- high_PRED_low_OUT / low_OUT
    num <- high_PRED_low_OUT
    denom <- low_OUT
    
  } else {
    stop(paste0("metric must be either of ", paste(METRICS, collapse = ", ") ))
  }
  
  return(list(metric=metric_value, num=num, denom=denom))
}







# Main function 
compute_group_metrics_boot <- function(data, metrics, outcome, predictor, fun_pred, R, fun_out=NULL, resid=T) {
  
  boot_fun <- function(data, indices) {
    
    # Boot sample
    data <- data[indices,]
    
    # Create SES var based on boot sample median
    data <- data %>%
      mutate(SES = if_else(SES_cont >= median(SES_cont), "High SES", "Low SES"))
  
    # Filter by group
    High_data <- filter(data, SES == "High SES")
    Low_data  <- filter(data, SES == "Low SES")
    
    # Create binary variables for predictor and outcome 
    High_data <- dichotomize(High_data, outcome, predictor, fun_pred, fun_out,resid)
    Low_data  <- dichotomize(Low_data, outcome, predictor, fun_pred, fun_out,resid)
    
    # Compute all required metrics
    all_metrics <- lapply(metrics, function(metric) {
      # Compute required metric on each group
      metric_Low  <- calculate_metrics(Low_data, metric)$metric
      metric_High <- calculate_metrics(High_data, metric)$metric
      c(metric_Low, metric_High, metric_Low-metric_High)
    })
  
    # Return estimates
    do.call(c, all_metrics)
  }
  
  
  # ---- Run bootstrapping ----
  boot_res <- boot(data = data, statistic = boot_fun, R = R)
  
  # Boot results
  boot_runs <- as.data.frame(boot_res$t)
  
  # Loop for all metrics  
  res <- lapply(seq_along(metrics), function(i) {
    
    # Column indices for this metric
    idx <- (i - 1) * 3 + 1:3
    
    # Extract bootstrapped estimates
    boot_mat <- boot_runs[, idx]
    colnames(boot_mat) <- c("Low SES", "High SES", "diff")
    
    # remove iterations if na
    boot_mat <- na.omit(boot_mat)
    
    # Point estimate
    est <- colMeans(boot_mat)[c("Low SES", "High SES")]
    
    # CIs from bootstrapped samples
    ci_low  <- apply(boot_mat[, c("Low SES", "High SES")], 2, quantile, probs = 0.025)
    ci_high <- apply(boot_mat[, c("Low SES", "High SES")], 2, quantile, probs = 0.975)
    
    # p-value for difference
    p_val <- 2 * min(mean(boot_mat$diff > 0), mean(boot_mat$diff < 0))  # two-tailed
    
    # Return results
    data.frame(
      metric    = rep(metrics[i], 2),
      group     = c("Low SES", "High SES"),
      value     = round(est[1:2], 2),
      ci_lower  = round(ci_low, 2),
      ci_upper  = round(ci_high, 2),
      p_value   = rep(p_val, 2),
      stars     = rep(add_stars(p_val), 2),
      group_var = "SES",
      outcome   = outcome,      # Replace with actual outcome variable
      predictor = predictor     # Replace with actual predictor
    )
  })
  
  # Combine
  res <- bind_rows(res)
  row.names(res) <- NULL
  
  # Add outcome information
  res <- res %>%
    mutate(high_OUT = ifelse(metric %in% c("TPR","FNR"), 1, 0))
  
  # Add sample size by group
  N_groups <- lapply(c("Low SES", "High SES"), function(g) {
    group_data <- filter(data, SES == g)
    group_data <- dichotomize(group_data,outcome,predictor,fun_pred, fun_out)
    group_data %>% group_by(high_OUT) %>% 
      summarise(n = n()) %>% mutate(group = g)
  }) %>% bind_rows
  
  # Combine and return
  merge(res,N_groups)
}






# ~~~~~ 🦄 VISUALIZATION ~~~~~

plot_rates <- function(results) {
  
  # Get the available metrics from your data
  available_metrics <- unique(results$metric)
  
  # Outcome labels
  out.lab.pos <- outcome.labs[results$outcome]
  out.lab.neg <- neg.outcome.labs[results$outcome]
  results <- results %>% mutate(out = ifelse(metric=="TPR", out.lab.pos, out.lab.neg))
  
  
  # Convert to factors for plotting
  results <- results %>%
    mutate(metric = factor(metric, levels = available_metrics, labels = metrics.labs[available_metrics]),
           group  = factor(group, levels=c("Low SES","High SES")))

  
  # Create the plot
  ggplot(results, aes(x = metric, y = value, group=group)) +
    geom_bar(aes(fill=group), stat="identity", position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  color="#747170",
                  position = position_dodge(width = 0.6),
                  width = 0.15, linewidth = 1) +
    # Axis labels
    labs(x="", y="") +
    # Add value labels left
    geom_text(data=filter(results, group == "Low SES"),
              aes(label = sprintf("%.2f", value)), 
              position = position_dodge(width = 0.6),
              vjust = -2.5,
              hjust = 1.5,
              size = 4,
              color = "black") +
    # Add value label right
    geom_text(data=filter(results, group == "High SES"),
              aes(label = sprintf("%.2f", value)), 
              position = position_dodge(width = 0.6),
              vjust = -2.5,
              hjust = -0.5,
              size = 4,
              color = "black") +
    # Add stars
    geom_text(aes(y = y_position, label = stars),
              color = "black",
              size = 8,
              position = position_dodge(width = 0)) +
    # Add secondary axis
    scale_y_continuous(
                       #"P (Low PGI | High EA)\n", 
                       #sec.axis = sec_axis(~ . * 1, 
                       #                    name = "P (High PGI | Low EA)\n",
                       #                    breaks = seq(0, 1, 0.2)),
                       limits = c(0, 1), 
                       breaks = seq(0, 1, 0.2),
                       labels = scales::label_number(accuracy = 0.1)
                       ) +
    theme_minimal() +
    theme(text = element_text(size=15),
          axis.title.y = element_text(size=12),
          legend.position = "bottom",
          legend.spacing.x = unit(20.0, 'cm')
          ) +
    scale_fill_discrete(name="") +
    facet_wrap(~out)

}



plot_perc <- function(results, show_metrics=c("FNR", "FPR"), adjust_pvalues=T) {
  
  # Outcome labels
  #out.lab.pos <- outcome.labs[unique(results$outcome)]
  #out.lab.neg <- neg.outcome.labs[unique(results$outcome)]
  out.lab.pos <- "Completed"
  out.lab.neg <- "Not completed"

  # Add labels for predicted value (PGI), and real value (EA)
  results <- results %>% 
    mutate(real = case_when(metric %in% c("TPR","FNR") ~ out.lab.pos,
                            metric %in% c("TNR","FPR") ~ out.lab.neg,
                            TRUE            ~ NA_character_)
    )
  
  # Indicator of metrics for plot
  counts <- results %>%
    mutate(show     = ifelse(metric %in% show_metrics, "1", "0"))
  
  # Remove values for metrics not shown
  counts <- counts %>%
    mutate(value_label = ifelse(metric %in% show_metrics, value, ""))
  
  # Adjust stars
  if(adjust_pvalues) counts$stars <- counts$stars.adj
  
  # Remove stars for a random group
  counts <- counts %>%
    mutate(stars = ifelse(group == "High SES", "", stars))
  
  # Convert to factors for plotting
  counts <- counts %>%
    mutate(group = factor(group, levels = c("Low SES", "High SES")))
  
  # Plot  
  ggplot(counts, aes(fill=group, y=value, x=group, alpha=show)) + 
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~real, scales="free_x") +
    geom_errorbar(data = counts %>% filter(show == 1),
                  aes(ymin = ci_lower, ymax = ci_upper), 
                  color="#635856",
                  width = 0.15, linewidth = 1) +
    scale_alpha_manual(name="", values=c(0.4, 1)) +
    theme_minimal() +
    # Axis labels
    labs(x="") +
    # Value labels in bars
    geom_text(aes(label = value_label, y=5), 
              size = 13,
              color = "#635856") +
    # Add stars
    geom_text(aes(y=77, x=1.5, label = stars),
              color = "#635856", 
              size  = 13, inherit.aes = F) +
    # Add 0.5 line
    geom_hline(yintercept=50, 
               linetype="dashed", color = "#635856", 
               alpha=0.6, linewidth=1) +
    # Add horizontal comparison lines between groups
    geom_segment(data=filter(counts, stars != ""),
                 x = 1, xend = 2, y = 75, yend = 75,
                 color = "#635856", linewidth = 0.5,
                 inherit.aes = FALSE) +
    # Add bracket ends (optional)
    geom_segment(data=filter(counts, stars != ""),
                 x = 1, xend = 1, y = 73, yend = 75,
                 color = "#635856", linewidth = 0.5,
                 inherit.aes = FALSE) +
    geom_segment(data=filter(counts, stars != ""),
                 x = 2, xend = 2, y = 73, yend = 75,
                 color = "#635856", linewidth = 0.5,
                 inherit.aes = FALSE) +
    #theme_minimal() +
    theme(legend.position = "bottom",
          legend.key.size = unit(2, "lines"),
          legend.text = element_text(size = 24, color = "#635856"),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x  = element_blank(),
          text            = element_text(size=24, color = "#635856"),
          axis.title.y    = element_text(color = "#635856"),
          strip.text      = element_text(size=26,color = "#635856")) +
    scale_fill_manual(name="", values=SES.colors) +
    guides(alpha = "none") +
    # Y axes names
    scale_y_continuous(limits = c(0, 100), 
                       name   = "p (overachieve)",
                       sec.axis = sec_axis(~ ., name = "p (underachieve)")
    ) 
  
}




RunRegression <- function(ds, which_model, outcome, predictor, keller) {
  
  # - 1 - Prepare
  data = get_data(ds)
  data[predictor] = as.vector(scale(data[predictor]))
  data$SES = factor(data$SES, levels = c("Low SES", "High SES"))
  
  
  # ---  Create all interactions
  controls          <- c(DEMO, PC_vars)
  pred_interactions <- paste0(predictor, "*", controls)
  ses_interactions  <- paste0("SES*", controls)
  
  # Combine in formula
  formula <- paste0(outcome, " ~ ", predictor, "*SES + ",
                    paste(controls, collapse = " + "))
  if (keller) formula <- paste0(formula, " + ",
                                paste(pred_interactions, collapse = " + "), " + ",
                                paste(ses_interactions, collapse = " + "))
  formula = as.formula(formula)
  
  
  # - 2 - Run models
  
  if (which_model=="LPM") {
    # LPM
    model <- lm(formula, data = data)
  } else if (which_model=="Logistic") {
    # LOGIT
    model <- glm(formula, data = data, family = binomial(link = "logit"))
  } else {
    cat(which_model, "is not an available option, try: 'LPM' or 'Logistic' ")
    stop()
  }
  
  # Return model
  return(model)
}




PlotRegression <- function(models_ds, model_name) {
  
  # Get model
  model <- models_ds[grepl(model_name, names(models_ds))][[1]]
  
  # Coefficient of interaction
  coefs <- coef(summary(model))[paste0(predictor,":SESHigh SES"), ]
  
  if(model_name=="Logistic") {
    coef       <- exp(coefs["Estimate"]) %>% round(2)
    coef_label <- "OR:"
    pvalue     <- coefs["Pr(>|z|)"]
  } else {
    coef       <- coefs["Estimate"] %>% round(3)
    coef_label <- "beta:"
    pvalue     <- coefs["Pr(>|t|)"]
  }
  
  # Label
  label = paste(coef_label, coef, add_stars(pvalue))
  
  # Get predictions
  preds <- ggpredict(model, terms = c(paste(predictor,"[all]"), "SES"))
  
  # Plot
  plot(preds) +
    annotate("label",
             label=label, 
             label.size = 0,
             x = -1.5, y = 0.85,
             hjust = 0, size=7) +
    labs(title="", x = "PGI", y = "Predicted probability\n") +
    theme_minimal() +
    theme(text         = element_text(size=24),
          title        = element_text(size=18),
          axis.title.y = element_text(size=22),
          panel.grid.minor = element_blank()) +
    scale_fill_manual(values=SES.colors, name="") +
    scale_color_manual(values=SES.colors, name="") +
    xlim(c(-2,2)) +
    ylim(c(0,1))
  
}

