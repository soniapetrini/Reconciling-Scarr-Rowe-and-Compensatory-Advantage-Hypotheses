setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


source("code/funs.R")

outcomes  <- c("high_school","college")
predictor <- "pgi_education"
fun_pred  <- 0.5
n_boot    <- 10000
datasets <- c("WLS","ELSA")

#################### COMPUTE #################### 

set.seed(123)      
# === Log
cat("======== Running with ========\n") 
cat("* outcome:", outcomes, "\n")
cat("* threshold:", fun_pred, "\n")
cat("* n_boot:", n_boot, "\n")



lapply(datasets, function(ds) {
  
  # === Log
  cat("* data:", ds, "\n")
  
  # Get data
  data <- get_data(ds)
  
  # Compute for all outcomes
  mclapply(outcomes, function(outcome) {
    
    # === Log
    cat("* outcome:", outcome, "\n")
    
    # Split by gender
    mclapply(c("female","male"), function(which_sex) {
      
      data <- filter(data, sex == which_sex)
      
      # Set metrics and compute them
      metrics <- c("TPR","TNR","FPR","FNR")
      results <- compute_group_metrics_boot(data, metrics, outcome, predictor, fun_pred, n_boot)
      
      # Write
      saveRDS(results, paste0("results/gender/",ds,"_",outcome,"_",fun_pred,"_",n_boot,"_",which_sex,".rsd"))
      
    }, mc.cores=2)
    
  }, mc.cores=2)
  

})













