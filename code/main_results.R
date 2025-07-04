
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


source("code/funs.R")


# RESULTS TABLE ######################################################


outcome <- "college"
predictor <- "pgi_education"
n_boot  <- 1000
metrics <- c("TPR", "TNR")

results <- lapply(c("WLS","ELSA"), function(ds) {
  results <- readRDS(paste0("results/",ds,"_",outcome,"_",n_boot,".rsd"))
  results %>% mutate(dataset = ds) %>% 
    filter(metric %in% metrics)
})

# Completed college
results_AH <- data.frame(metric    = c(rep("TPR",2), rep("TNR",2)),
                         value     = c(0.58, 0.69, 0.61, 0.57),
                         ci_lower  = c(0.55, 0.65, 0.58, 0.55),
                         ci_upper  = c(0.60, 0.74, 0.64, 0.59),
                         group     = c("High SES", "Low SES", "High SES", "Low SES"),
                         p_value   = c(rep(0.000, 2), rep(0.006,2)),
                         stars     = c(rep("***",2), rep("**",2)),
                         group_var = rep("SES", 4),
                         outcome   = rep(outcome, 4),
                         predictor = rep(predictor, 4),
                         dataset   = rep("AH", 4)
)

# Combine all datasets
results <- bind_rows(results, results_AH)

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


# ADJUST TO NEW FORMAT:

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

ds        <- "ELSA"
outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"

## Fake predictor as baseline
#set.seed(123)
#data$fake_predictor <- rnorm(nrow(data))
#predictor <- "fake_predictor"

# - Perc plot
results <- readRDS(paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd")) %>%
  mutate(dataset=ds)

# Correct stars
pvalues_ds <- filter(pvalues, dataset == ds) %>% select(dataset,p_value, stars.adj)
results    <- merge(results, pvalues_ds, by=c("dataset","p_value"))

# Plot
show_metrics <- c("TPR", "TNR")
adjust_pvalues <- ifelse("stars.adj" %in% colnames(results),T,F)
plot_perc(results, show_metrics, adjust_pvalues)

# Save
ggsave(paste0("plots/",ds,"_",outcome,".pdf"), width = 8, height =7)













