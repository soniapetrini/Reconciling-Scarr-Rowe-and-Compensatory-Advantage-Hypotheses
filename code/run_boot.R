setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")



# =========================================================
#                      ✨ BOOTSTRAPPING ✨
# =========================================================

source("code/funs.R")

# Settings
set.seed(123)
outcome   <- "college"
predictor <- "pgi_education"
fun_pred  <- "median"
n_boot    <- 10000

# Run all
mclapply(c("WLS","ELSA"), function(ds) {
  
  # Get data
  data <- get_data(ds)
  
  # Set metrics and compute them
  metrics <- c("TPR","TNR","FPR","FNR")
  results <- compute_group_metrics_boot(data, metrics, outcome, predictor, fun_pred, n_boot)

  # Write
  saveRDS(results, paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd"))
}, mc.cores=4)
