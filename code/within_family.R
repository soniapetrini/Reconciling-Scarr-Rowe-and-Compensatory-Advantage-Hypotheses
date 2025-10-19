setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")



# =========================================================
#                  ✨ WITHIN FAMILIES ✨
# =========================================================

source("code/funs.R")



# ~~~~~ 🌿 REDEFINE FUNCTIONS ~~~~~

dichotomize <- function(data, outcome, predictor, fun_pred, fun_out=NULL, resid=T) {
  
  
  # ~~~~ PREDICTOR ~~~~ 
  
  # - Residualise and Standardize
  if (resid) {
    # Add control variables
    pcs     = paste(PC_vars, collapse = " + ")
    demo    = paste(DEMO,    collapse = " + ")
    
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
  
  
  # - Dichotomize
  
  # If middle 20% is removed
  if (fun_pred=="no middle") {
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
    
  # If a numeric threshold is given (adjust classes if necessary)
  } else if (is.numeric(fun_pred)) {
    data %<>% mutate(high_PRED = ifelse(get(predictor) >= fun_pred, 0, 1))
    
  # If a function is given
  } else if (is.function(fun_pred)){
    fun_pred = get(fun_pred)
    thresh   = fun_pred(data[[predictor]])
    # Create binary
    data %<>% mutate(high_PRED = ifelse(get(predictor) >= thresh, 1, 0))
    
  } else if (fun_pred == "within"){
    # If within families
    
    tol <- 1e-8  # tolerance for floating-point equality
    data <- data %>%
      group_by(familyID) %>%
      # keep families where there is *some* within-family variation
      filter(diff(range(get(predictor))) > tol) %>%
      ungroup()
    
    data %<>% 
      group_by(familyID) %>%
      mutate(PGI_wf = get(predictor) - mean(get(predictor), na.rm = TRUE),
             high_PRED = as.integer(PGI_wf > 0)) %>%
      ungroup()
  }
  
  
  
  # ~~~~ OUTCOME ~~~~ 
  
  # - Dichotomize
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


# Main function 
compute_group_metrics_boot <- function(data, metrics, outcome, predictor, fun_pred, R, fun_out=NULL) {
  
  boot_fun <- function(data, indices) {
    
    # -------------------
    
    # Boot sample
    #data <- data[indices,]
    
    # ----- HERE -----
    
    # Cluster re-sampling
    # get family ID of sampled individuals
    sampled_families <- unique(data$familyID)[indices]
    
    # get the sibling of each sampled individual
    data <- data[data$familyID %in% sampled_families, ]
    
    # -------------------
    
    # Create SES var based on boot sample median
    data <- data %>%
      mutate(SES = if_else(SES_cont >= median(SES_cont), "High SES", "Low SES"))
    
    # Filter by group
    High_data <- filter(data, SES == "High SES")
    Low_data  <- filter(data, SES == "Low SES")
    
    # Create binary variables for predictor and outcome 
    High_data <- dichotomize(High_data, outcome, predictor, fun_pred, fun_out)
    Low_data  <- dichotomize(Low_data, outcome, predictor, fun_pred, fun_out)
    
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
    group_data <- dichotomize(group_data,outcome,predictor,fun_pred, fun_out)
    group_data %>% group_by(high_OUT) %>% 
      summarise(n = n()) %>% mutate(group = g)
  }) %>% bind_rows
  
  # Combine and return
  merge(res,N_groups)
}






# ~~~~~ 🌿 COMPUTE WITHIN-FAMILY ~~~~~

# Settings
set.seed(123)
outcome   <- "college"
predictor <- "pgi_education"
fun_pred  <- "within"
n_boot    <- 1000

# Get data
ds <- "WLS"
data <- readRDS(paste0("data/",ds,"/siblings.rds"))

# Set metrics and compute them
metrics <- c("TPR","TNR","FPR","FNR")
results <- compute_group_metrics_boot(data, metrics, outcome, predictor, fun_pred, n_boot)

# Write
#saveRDS(results, paste0("results/within/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd"))


# Add status variable
results <- results %>%
  mutate(dataset=ds, status=ifelse(high_OUT==1,"College","No College"))


# Plot
adjust_pvalues <- ifelse("stars.adj" %in% colnames(results),T,F)
plot_perc(results, c("FNR", "FPR"), adjust_pvalues)



























