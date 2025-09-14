
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")




# =========================================================
#                     ✨ RESULTS TABLE ✨
# =========================================================

source("code/funs.R")


outcome   <- "college"
predictor <- "pgi_education"
fun_pred  <- "median"
n_boot    <- 10000

# Read results
results <- lapply(c("WLS","ELSA"), function(ds) {
  results <- readRDS(paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd"))
  results %>% mutate(dataset = ds) %>% 
    filter(metric %in% c("FNR", "FPR"))
})

# Add Add Health manually 
results_AH <- if (fun_pred == "median") {
  # (n_boot=10000, "median")
  data.frame(
    group = c("High SES", "High SES", "High SES", "High SES",
              "Low SES", "Low SES", "Low SES", "Low SES"),
    high_OUT = c(0, 0, 1, 1, 0, 0, 1, 1),
    metric = c("FPR", "TNR", "TPR", "FNR",
               "FPR", "TNR", "TPR", "FNR"),
    value = c(0.41, 0.59, 0.57, 0.43,
              0.44, 0.56, 0.66, 0.34),
    ci_lower = c(0.38, 0.57, 0.55, 0.41,
                 0.43, 0.54, 0.62, 0.30),
    ci_upper = c(0.43, 0.62, 0.59, 0.45,
                 0.46, 0.57, 0.70, 0.38),
    p_value = c(0.0094, 0.0094, 0.0000, 0.0000,
                0.0094, 0.0094, 0.0000, 0.0000),
    stars = c("**", "**", "***", "***",
              "**", "**", "***", "***"),
    group_var = rep("SES", 8),
    outcome = rep("college", 8),
    predictor = rep("pgi_education", 8),
    n = c(732, 732, 1020, 1020,
          1275, 1275, 450, 450),
    dataset   = rep("AH", 4)
  )
} else if (fun_pred == "no middle") {
  # (n_boot=10000, "no middle")
  data.frame(
    group = c("High SES", "High SES", "High SES", "High SES",
              "Low SES", "Low SES", "Low SES", "Low SES"),
    high_OUT = c(0, 0, 1, 1, 0, 0, 1, 1),
    metric = c("FPR", "TNR", "TPR", "FNR",
               "FPR", "TNR", "TPR", "FNR"),
    value = c(0.39, 0.61, 0.58, 0.42,
              0.43, 0.57, 0.69, 0.31),
    ci_lower = c(0.36, 0.58, 0.56, 0.40,
                 0.42, 0.55, 0.65, 0.26),
    ci_upper = c(0.42, 0.64, 0.60, 0.44,
                 0.45, 0.58, 0.74, 0.35),
    p_value = c(0.0084, 0.0084, 0.0000, 0.0000,
                0.0084, 0.0084, 0.0000, 0.0000),
    stars = c("**", "**", "***", "***",
              "**", "**", "***", "***"),
    group_var = rep("SES", 8),
    outcome = rep("college", 8),
    predictor = rep("pgi_education", 8),
    n = c(597, 597, 805, 805,
          1025, 1025, 355, 355),
    dataset   = rep("AH", 4)
  )
  
} else {break}



# filter only Falses from AH
results_AH_false <- results_AH %>% filter(metric %in% c("FNR", "FPR"))

# Combine all datasets
results <- bind_rows(results)
results <- bind_rows(results, results_AH_false)
results


# Set labels
results <- results %>% 
  mutate(status  = ifelse(metric=="FNR", "College", "No College"),
         group   = factor(group, levels=c("Low SES", "High SES")),
         dataset = factor(dataset, levels=c("WLS", "ELSA","AH")))

# Keep pvalues and adjust
pvalues <- results %>% 
  filter(group=="High SES") %>%
  select(dataset, outcome, status, metric, p_value, stars) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues


# Reshape
results <- results %>% 
  select(dataset, status, group, value, ci_lower, ci_upper, n) %>%
  pivot_wider(names_from = group, values_from = c(value, ci_lower, ci_upper, n))

# Compute difference
results <- results %>%
  mutate(diff = `value_Low SES` - `value_High SES`)

# Combine with pvalues and adjust
results <- left_join(results, pvalues, by=c("dataset","status"))

# Add prevalence
results <- results %>% 
  group_by(dataset) %>% 
  mutate(`p_Low SES` = round(`n_Low SES`/sum(`n_Low SES`),2),
         `p_High SES` = round(`n_High SES`/sum(`n_High SES`), 2))

# Reorder rows
results <- results %>% arrange(dataset, status)

# Print in Latex
latex_table <- results %>%
  mutate(
    row_text = glue(
      "& {status} & ${round(`value_Low SES`, 2)}$  $[{`ci_lower_Low SES`},{`ci_upper_Low SES`}]$ & {`n_Low SES`} & {round(`p_Low SES`,2)} & ${round(`value_High SES`, 2)}$ $[{`ci_lower_High SES`}, {`ci_upper_High SES`}]$ & {`n_High SES`} & {round(`p_High SES`,2)} & ${round(diff, 2)}^{stars.adj}$ \\\\"
    )) %>% pull(row_text)
latex_table








# =========================================================
#                     ✨ MAIN PLOT ✨  
# =========================================================

# Assumes that pvalues have been adjusted in previous code:

lapply(c("WLS","ELSA", "AH"), function(ds) {
  
  # Read results
  results <- if (ds == "AH") {
    results_AH
  } else {
    readRDS(paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd")) 
  }
  
  # Add status variable
  results <- results %>%
    mutate(dataset=ds, status=ifelse(high_OUT==1,"College","No College"))
  
  # Correct stars
  pvalues_ds <- filter(pvalues, dataset == ds) %>% select(dataset, status, stars.adj)
  results    <- merge(results, pvalues_ds, by=c("dataset","status"))
  adjust_pvalues <- ifelse("stars.adj" %in% colnames(results),T,F)
  
  # Plot
  plot_perc(results, c("FNR", "FPR"), adjust_pvalues)
  
  # Save
  ggsave(paste0("plots/",ds,"_",outcome,"_",fun_pred,".pdf"), width = 7, height =7)

})



