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



# GLOBALS ####################################################
data_dir = "~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/data/"
SES_vars = c("father_edu", "father_occu")
DEMO     = c("birth_year", "sex")
PC_vars  = paste0("pc",seq(1:10))
METRICS  = c("NPV","PPV")
DATASETS = c("WLS","ELSA","SOEP")




# LABELS ####################################################
metrics.labs <- c(
                  #"NPV" = "Low PGI  ",
                  #"PPV" = "High PGI ",
                  "NPV" = "Negtaive Predictive\nValue",
                  "PPV" = "Positive Predictive\nValue",
                  "TNR" = "True Negative\nRate",
                  "FNR" = "False Negative\nRate",
                  "TPR" = "True Positive\nRate"
                  )

groups.labs <- c("low SES"  = "#fbaca7", "high SES" = "#52d8da")

outcome.labs <- c("education" = "educational attainment",
                  "cognitive" = "cognitive ability",
                  "college"   = "college attendancy")

# FUNCTIONS ####################################################

negative_to_na <- function(x) if_else(x<0, NA, x, missing = NA)


get_data <- function(ds) readRDS(paste0("data/",ds,"/df.rds"))


add_stars <- function(p_values) {
  # add significance stars
  ifelse(p_values < 0.01, "***",
         ifelse(p_values < 0.05, "**",
                ifelse(p_values < 0.1, "*", "")))
}



summary_stats <- function(ds) {
  # Get data
  df <- get_data(ds)
  df <- dichotomize(df)
  
  # Summary statistics by group
  df %>% group_by(SES) %>%
    summarize(
      Dataset = ds,
      n = n(),
      mean_education = mean(education, na.rm = TRUE),
      sd_education   = sd(education, na.rm = TRUE),
      sex            = sum(sex=="female")/n(),
      mean_yob       = as.integer(mean(birth_year)),
      sd_yob         = as.integer(sd(birth_year))
    ) %>% ungroup()
}







dichotomize <- function(data, 
                        outcome="education", predictor="pgi_education", 
                        fun_out="median",    fun_pred ="median", 
                        resid=T) {
  
  cat("outcome: ", outcome, "- threshold:", fun_out, "\n")
  cat("predictor: ", predictor, "- threshold:", fun_pred,  "\n")
  
  # - Residualise out of PGIs
  if (resid) {
    # Variables
    pcs     = paste(PC_vars, collapse = " + ")
    demo    = paste(DEMO,    collapse = " + ")
    # Regression
    formula <- as.formula(paste(predictor," ~", pcs, " + ", demo))
    model   <- lm(formula, data = data)
    # Residualise and scale
    data[[predictor]] = as.vector(scale(residuals(model), center = FALSE))
  } else {
    # Scale
    data[[predictor]] = as.vector(scale(data[[predictor]], center = FALSE))
  }
  
  
  # - PREDICTOR
  if (fun_pred=="no middle") {
    # If middle 20% is removed
    data %<>% mutate(
      breaks = cut(get(predictor), 
                   breaks = quantile(get(predictor), probs = c(0,0.4,0.6,1)), 
                   include.lowest = TRUE, labels = FALSE)
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
                   include.lowest = TRUE, labels = FALSE)
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
    high_PRED         <- sum(PGI == 1)
    metric_value     <- round(high_PRED_high_OUT / high_PRED, 2)
    num   <- high_PRED_high_OUT
    denom <- high_PRED
    
  } else if (metric == "NPV") {
    # Calculate negative predictive value
    low_PGI_low_EA <- sum(PGI == 0 & EA == 0)
    low_PGI        <- sum(PGI == 0)
    metric_value   <- round(low_PGI_low_EA / low_PGI, 2)
    num   <- low_PGI_low_EA
    denom <- low_PGI
    
  } else if (metric == "TPR") {
    # Calculate true positive rate
    high_PRED_high_OUT <- sum(PGI == 1 & EA == 1)
    high_OUT        <- sum(EA == 1)
    metric_value   <- round(high_PRED_high_OUT / high_OUT, 2)
    num   <- high_PRED_high_OUT
    denom <- high_OUT
    
  } else if (metric == "TNR") {
    # Calculate true negative rate
    low_PGI_low_EA <- sum(PGI == 0 & EA == 0)
    low_EA         <- sum(EA == 0)
    metric_value   <- round(low_PGI_low_EA / low_EA, 2)
    num   <- low_PGI_low_EA
    denom <- low_EA
    
  } else if (metric == "FNR") {
    # Calculate false negative rate
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




# Function to calculate the chosen metric
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



compute_group_metrics <- function(data, metric, 
                                  dichot="whole",
                                  outcome="education", predictor="pgi_education", 
                                  fun_out="median",    fun_pred ="median") {
    
  # Dichotomize variables on full sample
  if (dichot=="whole") {
    data <- dichotomize(data, outcome, predictor, fun_out, fun_pred)
  }
  
  # Split by group
  group1_data <- filter(data, SES == "high SES")
  group2_data <- filter(data, SES == "low SES")
  
  # Dichotomize variables by group
  if (dichot=="group") {
    group1_data <- dichotomize(group1_data, outcome, predictor, fun_out, fun_pred)
    group2_data <- dichotomize(group2_data, outcome, predictor, fun_out, fun_pred)
  }
  
  
  # Prevalence
  prev_high <- mean(group1_data$high_OUT) %>% round(2)
  prev_low  <- mean(group2_data$high_OUT) %>% round(2)
  
  # Compute metrics on each
  result_group1 <- compute_a_metric(group1_data, metric)
  result_group2 <- compute_a_metric(group2_data, metric)
  
  
  # Test the significance of between-group differences
  p_group1 = result_group1$value * result_group1$n
  p_group2 = result_group2$value * result_group2$n
  
  test_result <- prop.test(x = c(p_group1,        p_group2), 
                           n = c(result_group1$n, result_group2$n))
  
  # Combine main results
  result_group1 %<>% mutate(group = "high SES", prevalence = prev_high)
  result_group2 %<>% mutate(group = "low SES",  prevalence = prev_low)
  results <- rbind.data.frame(result_group1, result_group2) 
  
  
  # Add significance test
  results %>% 
    mutate(p_value    = test_result$p.value,
           y_position = max(results$ci_upper) + 0.09,
           stars      = add_stars(p_value),
           group_var  = "SES")
  
}




compute_all_metrics <- function(data, metrics=METRICS) {
  results_list <- lapply(metrics, compute_group_metrics, data=data)
  bind_rows(results_list)
}






plot_rates <- function(results) {
  
  # Get the available metrics from your data
  available_metrics <- unique(results$metric)
  y_lab <- ifelse("PPV" %in% available_metrics, "Predictive Value\n", "Rate")
  
  # Convert to factors for plotting
  results <- results %>%
    mutate(metric = factor(metric, levels = available_metrics, labels = metrics.labs[available_metrics]),
           group  = factor(group, levels=c("low SES","high SES")))
  
  # Create the plot
  ggplot(results, aes(x = metric, y = value, group=group)) +
    geom_bar(aes(fill=group), stat="identity", position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  color="#747170",
                  position = position_dodge(width = 0.6),
                  width = 0.15, linewidth = 1) +
    labs(x="", y=y_lab) +
    # Add value labels left
    geom_text(data=filter(results, group == "low SES"),
              aes(label = sprintf("%.2f", value)), 
              position = position_dodge(width = 0.6),
              vjust = -2.5,
              hjust = 1.5,
              size = 4,
              color = "black") +
    # Add value label right
    geom_text(data=filter(results, group == "high SES"),
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
    scale_y_continuous(limits = c(0, 1), 
                       breaks = seq(0, 1, 0.2),
                       labels = scales::label_number(accuracy = 0.1)) +
    theme_minimal() +
    theme(text = element_text(size=18),
          legend.position = "bottom",
          legend.spacing.x = unit(20.0, 'cm')) +
    scale_fill_discrete(name="")

}




create_metrics_table <- function(df) {
  
  # Calculate metrics for both ds
  results <- lapply(c("WLS","SOEP"), function(ds) {
    
    grouping_var = "SES"
    df <- readRDS(paste0("data/",ds,"/df.rds"))
    
    results <- lapply(METRICS, function(met) {
      calculate_group_metrics(df, grouping_var, metric = met) %>%
        mutate(grouping_var = grouping_var)
    })
    results <- do.call(rbind, results)
  })
  
  # Combine all results
  all_results <- do.call(rbind, results)
  
  
  # Create LaTeX table
  latex_table <- "\\begin{table}[htbp]
\\centering
\\usepackage{makecell}
\\caption{Group Differences in Classification Metrics}
\\begin{tabular}{llccc}
\\toprule
& & \\multicolumn{2}{c}{\\textbf{Metric}} \\\\
\\cmidrule(lr){3-4}
\\textbf{Grouping} & \\textbf{Group} & \\textbf{TPR} & \\textbf{FPR} \\\\
\\midrule\n"
  
  # Add rows for each grouping variable
  for (ds in c("WLS","SOEP")) {
    # Get results for this grouping variable
    results_subset <- all_results[all_results$grouping_var == ds,]
    groups <- unique(results_subset$group)
    
    # Get significance stars
    tpr_stars <- significance_tests$stars[significance_tests$grouping_var == ds & 
                                            significance_tests$metric == "TPR"]
    fpr_stars <- significance_tests$stars[significance_tests$grouping_var == ds & 
                                            significance_tests$metric == "FPR"]
    
    # Add multirow for grouping variable
    group_label <- if(ds == "sex") {
      "\\makecell[l]{Biological\\\\Sex}"
    } else {
      "\\makecell[l]{Socioeconomic\\\\Status}"
    }
    
    # First row of the group
    first_group <- groups[1]
    tpr_row <- results_subset[results_subset$group == first_group & results_subset$metric == "TPR",]
    fpr_row <- results_subset[results_subset$group == first_group & results_subset$metric == "FPR",]
    
    latex_table <- paste0(latex_table,
                          "\\multirow{2}{*}{", group_label, "} & ",
                          ifelse(ds == "sex", 
                                 ifelse(first_group == "male", "Male", "Female"),
                                 ifelse(first_group == "high SES", "High SES", "Low SES")),
                          " & ",
                          sprintf("%.2f", tpr_row$value)," [",
                          sprintf("%.2f", tpr_row$ci_lower),", ",
                          sprintf("%.2f", tpr_row$ci_upper),"]",
                          tpr_stars," & ",
                          sprintf("%.2f", fpr_row$value)," [",
                          sprintf("%.2f", fpr_row$ci_lower),", ",
                          sprintf("%.2f", fpr_row$ci_upper),"]",
                          fpr_stars," \\\\\n")
    
    # Second row of the group
    second_group <- groups[2]
    tpr_row <- results_subset[results_subset$group == second_group & results_subset$metric == "TPR",]
    fpr_row <- results_subset[results_subset$group == second_group & results_subset$metric == "FPR",]
    
    latex_table <- paste0(latex_table,
                          "& ",
                          ifelse(ds == "sex", 
                                 ifelse(second_group == "male", "Male", "Female"),
                                 ifelse(second_group == "high SES", "High SES", "Low SES")),
                          " & ",
                          sprintf("%.2f", tpr_row$value)," [",
                          sprintf("%.2f", tpr_row$ci_lower),", ",
                          sprintf("%.2f", tpr_row$ci_upper),"] & ",
                          sprintf("%.2f", fpr_row$value)," [",
                          sprintf("%.2f", fpr_row$ci_lower),", ",
                          sprintf("%.2f", fpr_row$ci_upper),"] \\\\\n")
    
    # Add midrule between grouping variables
    if (ds != tail(c("WLS","SOEP"), 1)) {
      latex_table <- paste0(latex_table, "\\midrule\n")
    }
  }
  
  # Close the table
  latex_table <- paste0(latex_table, "\\bottomrule
\\end{tabular}
\\caption*{\\footnotesize Note: Values shown as estimate [95\\% CI]. $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$}
\\end{table}")
  
  return(latex_table)
}



compute_prevalence <- function(df) {
  sapply(c("high SES","low SES"), function(g) {
    group_df <- df %>% filter(SES == g)
    group_df <- group_df %>%
      dichotomize() %>%
      group_by(high_OUT) %>%
      summarize(count = n()) %>% 
      mutate(prev = count/nrow(group_df))
    prevalence=round(group_df$prev[group_df$high_OUT==1],2)
    paste0(g,":",prevalence,"  ")
    prevalence
  })
}
