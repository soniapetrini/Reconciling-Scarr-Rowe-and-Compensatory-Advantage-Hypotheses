
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


source("code/funs.R")


# MAIN TABLE ######################################################

outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "median"


# Compute for both datasets

results <- lapply(c("WLS","ELSA"), function(ds) {
  
  df <- get_data(ds)
  
  # Compute all metrics
  metrics <- c("TPR","TNR")
  results <- lapply(metrics, function(metric) {
    compute_group_metrics(df, metric, outcome, predictor, fun_out, fun_pred) 
  })
  # Combine and return
  bind_rows(results) %>% mutate(dataset = ds)
  
})

# -- Add Health
# Some college
results_AH <- data.frame(metric   = c(rep("TPR",2), rep("TNR",2)),
                         value    = c(0.54, 0.63, 0.60, 0.58),
                         ci_lower = c(0.51, 0.59, 0.55, 0.55),
                         ci_upper = c(0.57, 0.67, 0.65, 0.61),
                         n        = c(985, 504, 409, 890),
                         group    = c("High SES", "Low SES", "High SES", "Low SES"),
                         prevalence = c(0.71, 0.36, 0.71, 0.36),
                         p_value  = c(rep(0.001096568, 2), rep(0.535818372,2)),
                         y_position = c(rep(0.79, 2), rep(0.77,2)),
                         stars    = c(rep("***",2), rep("",2)),
                         group_var = rep("SES", 4),
                         outcome  = rep(outcome, 4),
                         predictor  = rep(predictor, 4),
                         dataset       = rep("AH", 4)
)
# Completed college
results_AH <- data.frame(metric   = c(rep("TPR",2), rep("TNR",2)),
                         value    = c(0.58, 0.70, 0.61, 0.57),
                         ci_lower = c(0.55, 0.65, 0.57, 0.54),
                         ci_upper = c(0.61, 0.75, 0.65, 0.60),
                         n        = c(799, 359, 589, 1035),
                         group    = c("High SES", "Low SES", "High SES", "Low SES"),
                         prevalence = rep(c(0.58, 0.26),2),
                         p_value  = c(rep(0.0001333648, 2), rep(0.1284525659,2)),
                         #p_value  = c(rep(0.001333648, 2), rep(0.209,2)), # median
                         y_position = c(rep(0.87, 2), rep(0.77,2)),
                         stars    = c(rep("***",2), rep("",2)),
                         group_var = rep("SES", 4),
                         outcome  = rep(outcome, 4),
                         predictor  = rep(predictor, 4),
                         dataset       = rep("AH", 4)
)


# Combine all datasets
results <- bind_rows(results)

# Add Add Health
results <- bind_rows(results, results_AH)

# Set labels
results <- results %>% 
  mutate(status = ifelse(metric=="TPR", "College", "No College"),
         group = factor(group, levels=c("Low SES", "High SES")))

# Keep pvalues and adjust
pvalues <- results %>% 
  filter(group=="High SES") %>%
  select(dataset, outcome, status, metric, p_value, stars, n) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues

# Reshape
results <- results %>% 
  select(dataset, outcome, status, group, value, ci_lower, ci_upper, n) %>%
  pivot_wider(names_from = group, values_from = c(value, ci_lower, ci_upper, n))

# Compute difference
results <- results %>%
  mutate(diff = `value_Low SES` - `value_High SES`)

# Combine with pvalues and adjust
results <- left_join(results, pvalues, by=c("dataset","outcome","status"))

# Add prevalence
results <- results %>% 
  group_by(dataset) %>% 
  mutate(`p_Low SES` = round(`n_Low SES`/sum(`n_Low SES`),2),
         `p_High SES` = round(`n_High SES`/sum(`n_High SES`), 2))

# Print in Latex
latex_table <- results %>%
  mutate(
    row_text = glue(
      "& {status} & ${round(`value_Low SES`, 2)}$  $[{`ci_lower_Low SES`},{`ci_upper_Low SES`}]$ & {`n_Low SES`} & {round(`p_Low SES`,2)} & ${round(`value_High SES`, 2)}$ $[{`ci_lower_High SES`}, {`ci_upper_High SES`}]$ & {`n_High SES`} & {round(`p_High SES`,2)} & ${round(diff, 2)}^{stars.adj}$ \\\\"
    )) %>% pull(row_text)
latex_table





# MAIN PLOT ######################################################

ds        <- "WLS"
outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"


# --- Prepare
data <- get_data(ds)

## Fake predictor as baseline
#set.seed(123)
#data$fake_predictor <- rnorm(nrow(data))
#predictor <- "fake_predictor"

# - Perc plot
metrics <- c("TPR","TNR","FPR","FNR")
results <- lapply(metrics, 
                  compute_group_metrics, 
                  data=data,
                  outcome=outcome, predictor=predictor, 
                  fun_out=fun_out, fun_pred=fun_pred)
results <- bind_rows(results) %>% mutate(outcome=outcome)

# Correct stars
pvalues_ds <- filter(pvalues, dataset == ds) %>% select(p_value, stars.adj)
results    <- merge(results, pvalues_ds, by="p_value")

# Plot
show_metrics <- c("TPR", "TNR")
adjust_pvalues <- ifelse("stars.adj" %in% colnames(results),T,F)
plot_perc(results, show_metrics, adjust_pvalues)

# Save
ggsave(paste0("plots/",ds,"_",outcome,".pdf"), width = 8, height =7)








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
results <- compute_group_metrics_boot(data, metric, outcome, predictor, fun_out, fun_pred, n_boot)
results

# Write
saveRDS(results, paste0("results/",ds,"_",outcome,"_",n_boot,".rsd"))

metrics <- c("TPR", "TNR")
adjust_pvalues <- F
plot_perc(results, metrics, adjust_pvalues)




# RESULTS TABLE ######################################################
outcome <- "college"
n_boot  <- 1000
metrics <- c("TPR", "TNR")

results <- lapply(c("WLS","ELSA"), function(ds) {
  results <- readRDS(paste0("results/",ds,"_",outcome,"_",n_boot,".rsd"))
  results %>% mutate(dataset = ds) %>% 
    filter(metric %in% metrics)
})

# Combine all datasets
results <- bind_rows(results)

# Set labels
results <- results %>% 
  mutate(status = ifelse(metric=="TPR", "College", "No College"),
         group = factor(group, levels=c("Low SES", "High SES")))

# Keep pvalues and adjust
pvalues <- results %>% 
  filter(group=="High SES") %>%
  select(dataset, outcome, status, metric, p_value, stars) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues















