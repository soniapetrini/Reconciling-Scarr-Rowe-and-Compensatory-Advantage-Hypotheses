setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


source("code/funs.R")


# BOOTSTRAPPING ######################################################
set.seed(123)
outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "median"
n_boot    <- 10000


mclapply(c("WLS","ELSA"), function(ds) {
  
  # Get data
  data <- get_data(ds)
  
  # Split by gender
  mclapply(c("female","male"), function(which_sex) {
    
    data <- filter(data, sex == which_sex)
    
    # Set metrics and compute them
    metrics <- c("TPR","TNR","FPR","FNR")
    results <- compute_group_metrics_boot(data, metrics, outcome, predictor, fun_out, fun_pred, n_boot)
    
    # Write
    saveRDS(results, paste0("results/gender/",ds,"_",outcome,"_",fun_pred,"_",n_boot,"_",which_sex,".rsd"))
    
  }, mc.cores=2)
  

}, mc.cores=2)



# RESULTS TABLE ######################################################
source("code/funs.R")


outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "median"
n_boot    <- 10000

# Read results
results <- lapply(c("WLS","ELSA"), function(ds) {
  
  # By gender
  lapply(c("female","male"), function(which_sex) {
    
    # Read
    results <- readRDS(paste0("results/gender/",ds,"_",outcome,"_",fun_pred,"_",n_boot,"_",which_sex,".rsd"))
    results %>% mutate(dataset = ds, sex = which_sex) %>% 
      filter(metric %in% c("FPR", "FNR"))
  
  })
})

results <- bind_rows(results)

# Set labels
results <- results %>% 
  mutate(status  = ifelse(metric=="FNR", "College", "No College"),
         group   = factor(group, levels=c("Low SES", "High SES")),
         dataset = factor(dataset, levels=c("WLS", "ELSA")),
         sex     = factor(sex, levels = c("female","male"))
  )


# Add Add Health manually (n_boot=10000, "median")
results_AH <- data.frame(
  group      = c("High SES","High SES","Low SES","Low SES",
                 "High SES","High SES","Low SES","Low SES"),
  high_OUT   = c(0,1,0,1,0,1,0,1),
  metric     = c("TNR","TPR","TNR","TPR","TNR","TPR","TNR","TPR"),
  value      = c(0.63,0.58,0.57,0.66,0.55,0.55,0.54,0.67),
  ci_lower   = c(0.59,0.55,0.54,0.61,0.52,0.52,0.52,0.60),
  ci_upper   = c(0.67,0.60,0.59,0.71,0.59,0.58,0.56,0.73),
  p_value    = c(0.0072,0.0034,0.0072,0.0034,0.4806,0.0028,0.4806,0.0028),
  stars      = c("**","**","**","**","","**","","**"),
  group_var  = rep("SES", 8),
  outcome    = rep("college", 8),
  predictor  = rep("pgi_education", 8),
  n          = c(357,603,676,288,375,417,599,162),
  dataset    = rep("AH", 8),
  sex        = c("female","female","female","female",
                 "male","male","male","male"),
  status     = c("No College","College","No College","College",
                 "No College","College","No College","College")
)


results_AH <- data.frame(
  group = c("High SES", "High SES", "Low SES", "Low SES", 
                "High SES", "High SES", "Low SES", "Low SES"),
  high_OUT = c(0, 1, 0, 1, 0, 1, 0, 1),
  metric = c("FPR", "FNR", "FPR", "FNR", "FPR", "FNR", "FPR", "FNR"),
  value = c(0.37, 0.42, 0.43, 0.34, 0.45, 0.45, 0.46, 0.33),
  ci_lower = c(0.33, 0.40, 0.41, 0.29, 0.41, 0.42, 0.44, 0.27),
  ci_upper = c(0.41, 0.45, 0.46, 0.39, 0.48, 0.48, 0.48, 0.40),
  p_value = c(0.0072, 0.0034, 0.0072, 0.0034, 0.4806, 0.0028, 0.4806, 0.0028),
  stars = c("**", "**", "**", "**", "", "**", "", "**"),
  group_var = c("SES", "SES", "SES", "SES", "SES", "SES", "SES", "SES"),
  outcome = c("college", "college", "college", "college", 
              "college", "college", "college", "college"),
  predictor = rep("pgi_education", 8),
  n = c(357, 603, 676, 288, 375, 417, 599, 162),
  dataset = rep("AH", 8),
  sex = c("female", "female", "female", "female", 
          "male", "male", "male", "male"),
  status = c("No College", "College", "No College", "College",
                "No College", "College", "No College", "College")
)

results <- bind_rows(results, results_AH)



# Keep pvalues and adjust
pvalues <- results %>% 
  filter(group=="High SES") %>%
  select(dataset, outcome, sex, status, metric, p_value, stars) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues




# MAIN PLOT ######################################################

ds <- "ELSA"

# - Perc plot

# By gender
results <- lapply(c("female","male"), function(which_sex) {
  readRDS(paste0("results/gender/",ds,"_",outcome,"_",fun_pred,"_",n_boot,"_",which_sex,".rsd")) %>%
    mutate(dataset=ds, 
           status=ifelse(high_OUT==1,"College","No College"),
           sex = which_sex)

})

results <- bind_rows(results)

# Correct stars
pvalues_ds <- filter(pvalues, dataset == ds) %>% select(dataset, status, sex, stars.adj)
results    <- merge(results, pvalues_ds, by=c("dataset","status","sex"))
adjust_pvalues <- ifelse("stars.adj" %in% colnames(results),T,F)

# Plot
plot_perc(results, c("FNR", "FPR"), adjust_pvalues) + facet_grid(sex~real, scales="free_x")

# Save
ggsave(paste0("plots/",ds,"_",outcome,"_gender.pdf"), width = 8, height =9)





