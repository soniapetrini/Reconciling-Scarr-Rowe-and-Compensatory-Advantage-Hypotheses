
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
})



# GLOBALS ####################################################
data_dir = "~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/data/"
SES_vars = c("father_edu", "father_occu")
DEMO     = c("birth_year", "sex")
PC_vars  = paste0("pc",seq(1:10))
METRICS  = c("NPV","PPV")
DATASETS = c("WLS","ELSA","SOEP")
OUTCOMES = c("education","high_school","college","graduate_school", "heigh")
bottom_pgi <- 0.40
top_pgi    <- 0.60


# LABELS ####################################################
metrics.labs <- c(
                  #"NPV" = "Low PGI  ",
                  #"PPV" = "High PGI ",
                  "NPV" = "Negative Predictive\nValue",
                  "PPV" = "Positive Predictive\nValue",
                  "TNR" = "P (Low PGI)",
                  "FNR" = "False Negative\nRate",
                  "FPR" = "P (High PGI)",
                  "TPR" = "P (High PGI)"
                  )


groups.labs <- c("Low SES"  = "#fbaca7", "High SES" = "#52d8da")

outcome.labs <- c("education"         = "Educational attainment",
                  "cognitive"         = "Cognitive ability",
                  "high_school"       = "High School completed",
                  "college"           = "College",
                  "graduate_school"   = "Graduate completed"
                  )

neg.outcome.labs <- c("education"         = "Low Educational attainment",
                      "cognitive"         = "Low Cognitive ability",
                      "high_school"       = "High School not completed",
                      "college"           = "No College",
                      "graduate_school"   = "Graduate not completed"
)


# FUNCTIONS ####################################################

negative_to_na <- function(x) if_else(x<0, NA, x, missing = NA)


get_data <- function(ds) readRDS(paste0("data/",ds,"/df.rds"))


add_stars <- function(p_values) {
  # add significance stars
  ifelse(p_values < 0.001, "***",
         ifelse(p_values < 0.01, "**",
                ifelse(p_values < 0.05, "*", "")))
}




calc_tpr <- function(pred_prob, actual, threshold) {
  pred_class <- ifelse(pred_prob >= threshold, 1, 0)
  tp <- sum(pred_class == 1 & actual == 1)
  fn <- sum(pred_class == 0 & actual == 1)
  tp / (tp + fn)
}

# Function to calculate FPR for a given threshold  
calc_tnr <- function(pred_prob, actual, threshold) {
  pred_class <- ifelse(pred_prob >= threshold, 1, 0)
  fp <- sum(pred_class == 1 & actual == 0)
  tn <- sum(pred_class == 0 & actual == 0)
  tn / (fp + tn)
}


dichotomize <- function(data, outcome, predictor, fun_out, fun_pred, resid=T) {
  
  # - Residualise out of PGIs
  if (resid) {
    # Add control variables
    pcs     = paste(PC_vars, collapse = " + ")
    demo    = paste(DEMO,    collapse = " + ")
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
    # If a numeric threshold is given (adjust classes if necessary)
    data %<>% mutate(high_PRED = ifelse(get(predictor) >= fun_pred, 0, 1))
    
  } else {
    # If a function is given
    fun_pred = get(fun_pred)
    thresh   = fun_pred(data[[predictor]])
    # Create binary
    data %<>% mutate(high_PRED = ifelse(get(predictor) >= thresh, 1, 0))
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
   
 } else {
   # If a function is given
   fun_out = get(fun_out)
   thresh  = fun_out(data[[outcome]])
   # Create binary
   data %<>% mutate(high_OUT = ifelse(get(outcome) > thresh, 1, 0))
   
 }
  
  # Return data
  data %<>% na.omit(data)
}




calculate_metrics <- function(data, metric) {
  
  # Get two vectors
  EA  <- data[["high_OUT"]]
  PGI <- data[["high_PRED"]]
  
  if (metric == "PPV") {
    # Calculate positive predictive value (precision)
    high_PRED_high_OUT <- sum(PGI == 1 & EA == 1)
    high_PRED          <- sum(PGI == 1)
    metric_value       <- round(high_PRED_high_OUT / high_PRED, 2)
    num   <- high_PRED_high_OUT
    denom <- high_PRED
    
  } else if (metric == "NPV") {
    # Calculate negative predictive value
    Low_PGI_Low_EA <- sum(PGI == 0 & EA == 0)
    Low_PGI        <- sum(PGI == 0)
    metric_value   <- round(Low_PGI_Low_EA / Low_PGI, 2)
    num   <- Low_PGI_Low_EA
    denom <- Low_PGI
    
  } else if (metric == "TPR") {
    # Calculate true positive rate
    high_PRED_high_OUT <- sum(PGI == 1 & EA == 1)
    high_OUT        <- sum(EA == 1)
    metric_value   <- round(high_PRED_high_OUT / high_OUT, 2)
    num   <- high_PRED_high_OUT
    denom <- high_OUT
    
  } else if (metric == "TNR") {
    # Calculate true negative rate
    Low_PGI_Low_EA <- sum(PGI == 0 & EA == 0)
    Low_EA         <- sum(EA == 0)
    metric_value   <- round(Low_PGI_Low_EA / Low_EA, 2)
    num   <- Low_PGI_Low_EA
    denom <- Low_EA
    
  } else if (metric == "FNR") {
    # Calculate false negative rate
    high_PRED_low_OUT <- sum(PGI == 0 & EA == 1)
    low_OUT           <- sum(EA == 1)
    metric_value      <- round(high_PRED_low_OUT / low_OUT, 2)
    num <- high_PRED_low_OUT
    denom <- low_OUT
    
  } else if (metric == "FPR") {
    # Calculate false positive rate
    high_PRED_low_OUT <- sum(PGI == 1 & EA == 0)
    low_OUT           <- sum(EA == 0)
    metric_value      <- round(high_PRED_low_OUT / low_OUT, 2)
    num <- high_PRED_low_OUT
    denom <- low_OUT
    
  } else {
    stop(paste0("metric must be either of ", paste(METRICS, collapse = ", ") ))
  }
  
  return(list(metric=metric_value, num=num, denom=denom))
}




# Function to calculate a metric with Wilson CIs
compute_a_metric <- function(data, metric) {
  
  # Compute metrics
  res <- calculate_metrics(data, metric)
  metric_value <- res$metric
  num          <- res$num
  denom        <- res$denom
  
  # Calculate confidence interval using Wilson method
  z <- 1.96
  denominator <- 1 + z^2/denom
  center      <- (metric_value + z^2/(2*denom))/denominator
  halfwidth   <- z * sqrt(metric_value*(1-metric_value)/denom + z^2/(4*denom^2))/denominator
  
  return(data.frame(metric = metric,
              value = metric_value, 
              ci_lower = round(max(0, center - halfwidth),2),
              ci_upper = round(min(1, center + halfwidth),2),
              n = denom))
  
}



compute_group_metrics <- function(data, metric, outcome, predictor, fun_out, fun_pred) {
  
  # Split by group
  High_data <- filter(data, SES == "High SES")
  Low_data  <- filter(data, SES == "Low SES")
  
  # Create binary variables for predictor and outcome 
  High_data <- dichotomize(High_data, outcome, predictor, fun_out, fun_pred)
  Low_data  <- dichotomize(Low_data, outcome, predictor, fun_out, fun_pred)

  # Compute prevalence
  prev_High <- mean(High_data$high_OUT) %>% round(2)
  prev_Low  <- mean(Low_data$high_OUT) %>% round(2)
  
  # Compute required metric on each group
  result_High <- compute_a_metric(High_data, metric)
  result_Low  <- compute_a_metric(Low_data, metric)
  
  # Retreive numerator from proportion and denominator
  num_High = round(result_High$value * result_High$n)
  num_Low  = round(result_Low$value * result_Low$n)
  
  # Table with counts
  table_2x2 <- matrix(
    c(num_High, result_High$n - num_High,
      num_Low,  result_Low$n  - num_Low),
    nrow = 2,
    byrow = TRUE
  )
  
  ## Fisher's Exact Test  test for the significance of between-group differences
  test_result <- fisher.test(table_2x2)
  
  # Combine main results
  result_High %<>% mutate(group = "High SES", prevalence = prev_High)
  result_Low  %<>% mutate(group = "Low SES",  prevalence = prev_Low)
  results <- rbind.data.frame(result_High, result_Low) 
  
  # Add significance test
  results %>% 
    mutate(p_value    = test_result$p.value,
           y_position = max(results$ci_upper) + 0.12,
           stars      = add_stars(p_value),
           group_var  = "SES",
           outcome    = outcome,
           predictor  = predictor)
  
}




# IMPLEMENT BOOTSTRAPPING

compute_group_metrics_boot <- function(data, metrics, outcome, predictor, fun_out, fun_pred, R) {
  
  boot_fun <- function(data, indices) {
    
    # Boot sample
    data <- data[indices,]
    
    # Create SES var based on boot sample median
    data <- data %>%
      mutate(SES = if_else(SES_cont >= median(SES_cont), "High SES", "Low SES"))
    
    # Create binary variables for predictor and outcome 
    data <- dichotomize(data, outcome, predictor, fun_out, fun_pred)
    
    # Filter by group
    High_data <- filter(data, SES == "High SES")
    Low_data  <- filter(data, SES == "Low SES")
    
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
    group_data <- dichotomize(group_data,outcome,predictor,fun_out,fun_pred)
    group_data %>% group_by(high_OUT) %>% 
      summarise(n = n()) %>% mutate(group = g)
  }) %>% bind_rows
  
  # Combine and return
  merge(res,N_groups)
}




# helper dictionary

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



plot_perc <- function(results, show_metrics, adjust_pvalues) {
  
  # Outcome labels
  out.lab.pos <- outcome.labs[unique(results$outcome)]
  out.lab.neg <- neg.outcome.labs[unique(results$outcome)]

  # Add labels for predicted value (PGI), and real value (EA)
  results <- results %>% 
    mutate(real = case_when(metric == "TPR" ~ out.lab.pos,
                            metric == "TNR" ~ out.lab.neg,
                            metric == "FPR" ~ out.lab.neg,
                            metric == "FNR" ~ out.lab.pos,
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
    geom_bar(position="fill", stat="identity") +
    facet_wrap(~real, scales="free_x") +
    geom_errorbar(data = counts %>% filter(show == 1),
                  aes(ymin = ci_lower, ymax = ci_upper), 
                  color="#635856",
                  width = 0.15, linewidth = 1) +
    scale_alpha_manual(name="", values=c(0.4, 1)) +
    # Axis labels
    labs(x="") +
    # Value labels
    geom_text(aes(label = value_label), 
              vjust = -3.5,
              size = 6,
              color = "#635856") +
    # Add stars
    geom_text(aes(y=0.9, x=1.5, label = stars),
              color = "#635856", 
              size  = 10, inherit.aes = F) +
    # Add 0.5 line
    geom_hline(yintercept=0.5, 
               linetype="dashed", color="black", 
               alpha=0.6, linewidth=1) +
    # Add horizontal comparison lines between groups
    geom_segment(data=filter(counts, stars != ""),
                 x = 1, xend = 2, y = 0.88, yend = 0.88,
                 color = "#635856", linewidth = 0.5,
                 inherit.aes = FALSE) +
    # Add bracket ends (optional)
    geom_segment(data=filter(counts, stars != ""),
                 x = 1, xend = 1, y = 0.86, yend = 0.88,
                 color = "#635856", linewidth = 0.5,
                 inherit.aes = FALSE) +
    geom_segment(data=filter(counts, stars != ""),
                 x = 2, xend = 2, y = 0.86, yend = 0.88,
                 color = "#635856", linewidth = 0.5,
                 inherit.aes = FALSE) +
    #theme_minimal() +
    theme(legend.position = "bottom",
          legend.key.size = unit(2, "lines"),
          legend.text = element_text(size = 20),
          axis.text.x = element_blank(),
          axis.ticks.x  = element_blank(),
          text            = element_text(size=20),
          axis.title.y    = element_text(color = "#635856"),
          strip.text      = element_text(color = "#635856")) +
    scale_fill_discrete(name="") +
    guides(alpha = "none") +
    # Fix axis display
    scale_y_continuous(limits = c(0, 1), 
                       name   = "Sensitivity",
                       sec.axis = sec_axis(~ ., name = "Specificity")
    ) 
  
}




