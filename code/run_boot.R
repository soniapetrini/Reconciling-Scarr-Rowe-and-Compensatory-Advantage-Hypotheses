setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


source("code/funs.R")


# BOOTSTRAPPING ######################################################


ds        <- "WLS"
outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"


# Get data
data <- get_data(ds)

set.seed(123)
n_boot    <- 1000
metrics <- c("TPR","TNR","FPR","FNR")
results <- compute_group_metrics_boot(data, metrics, outcome, predictor, fun_out, fun_pred, n_boot)
results

# Write
saveRDS(results, paste0("results/",ds,"_",outcome,"_",n_boot,".rsd"))

metrics <- c("TPR", "TNR")
adjust_pvalues <- F
plot_perc(results, metrics, adjust_pvalues)