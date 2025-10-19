rm(list=ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")


# =========================================================
#                      ✨ BOOTSTRAPPING ✨
# =========================================================

# Set
fun_preds <- c(0.2, 0.4, 0.5, 0.6, 0.8,"no middle")
fun_preds <- 0.5
n_boot    <- 1000
datasets  <- c("ELSA","WLS")
outcomes  <- c("college", "high_school")
outcomes  <- c("college")
predictor <- "pgi_education"
metrics   <- c("TPR","TNR","FPR","FNR")





#################### COMPUTE #################### 

set.seed(123)      
# === Log
cat("======== Running with ========\n") 
cat("* outcome:", outcomes, "\n")
cat("* threshold:", fun_preds, "\n")
cat("* n_boot:", n_boot, "\n")


# Run all
lapply(datasets, function(ds) {
  
  # === Log
  cat("* data:", ds, "\n")
  
  # Get data
  data <- get_data(ds)
  
  # Compute for all outcomes
  lapply(outcomes, function(outcome) {
    
    # === Log
    cat("* outcome:", outcome, "\n")
    
    # Compute for all thresholds
    mclapply(fun_preds, function(fun_pred) {
      
      if(fun_pred != "no middle") fun_pred <- as.numeric(fun_pred)
      
      # === Compute
      results <- compute_group_metrics_boot(data, metrics, outcome, predictor, fun_pred, n_boot, fun_out = NULL,resid=T)
      
      # === Write
      saveRDS(results, paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd"))
      
      
    }, mc.cores=4)
    
  })
  
})



