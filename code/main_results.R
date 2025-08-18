
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")




# RESULTS TABLE ######################################################
source("code/funs.R")


outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "median"
n_boot    <- 1000

# Read results
results <- lapply(c("WLS","ELSA"), function(ds) {
  results <- readRDS(paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd"))
  results %>% mutate(dataset = ds) %>% 
    filter(metric %in% c("TPR", "TNR"))
})

# Add Add Health manually (n_boot=10000, "no middle")
results_AH <- data.frame(metric    = c(rep("TPR",2), rep("TNR",2)),
                         value     = c(0.58, 0.69, 0.61, 0.57),
                         ci_lower  = c(0.55, 0.65, 0.58, 0.55),
                         ci_upper  = c(0.60, 0.74, 0.64, 0.58),
                         group     = c("High SES", "Low SES", "High SES", "Low SES"),
                         high_OUT  = c(1,1,0,0),
                         n         = c(799, 359, 589, 1035),
                         p_value   = c(rep(0.000, 2), rep(0.0084,2)),
                         stars     = c(rep("***",2), rep("**",2)),
                         group_var = rep("SES", 4),
                         outcome   = rep(outcome, 4),
                         predictor = rep(predictor, 4),
                         dataset   = rep("AH", 4)
)

# Add Add Health manually (n_boot=10000, "median")
results_AH <- data.frame(metric    = c(rep("TPR",2), rep("TNR",2)),
                         value     = c(0.57, 0.66, 0.59, 0.56),
                         ci_lower  = c(0.55, 0.62, 0.57, 0.54),
                         ci_upper  = c(0.59, 0.70, 0.62, 0.57),
                         group     = c("High SES", "Low SES", "High SES", "Low SES"),
                         high_OUT  = c(1,1,0,0),
                         n         = c(799, 359, 589, 1035),
                         p_value   = c(rep(0.000, 2), rep(0.0094,2)),
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
  mutate(status  = ifelse(metric=="TPR", "College", "No College"),
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

# Reorder rows
results <- results %>% arrange(dataset, status)

# Print in Latex
latex_table <- results %>%
  mutate(
    row_text = glue(
      "& {status} & ${round(`value_Low SES`, 2)}$  $[{`ci_lower_Low SES`},{`ci_upper_Low SES`}]$ & {`n_Low SES`} & {round(`p_Low SES`,2)} & ${round(`value_High SES`, 2)}$ $[{`ci_lower_High SES`}, {`ci_upper_High SES`}]$ & {`n_High SES`} & {round(`p_High SES`,2)} & ${round(diff, 2)}^{stars.adj}$ \\\\"
    )) %>% pull(row_text)
latex_table






# MAIN PLOT ######################################################

ds <- "WLS"

## Fake predictor as baseline
#set.seed(123)
#data$fake_predictor <- rnorm(nrow(data))
#predictor <- "fake_predictor"

# - Perc plot
results <- readRDS(paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd")) %>%
  mutate(dataset=ds, status=ifelse(high_OUT==1,"College","No College"))

# Correct stars
pvalues_ds <- filter(pvalues, dataset == ds) %>% select(dataset, status, stars.adj)
results    <- merge(results, pvalues_ds, by=c("dataset","status"))
adjust_pvalues <- ifelse("stars.adj" %in% colnames(results),T,F)

# Plot
plot_perc(results, c("TPR", "TNR"), adjust_pvalues)

# Save
ggsave(paste0("plots/",ds,"_",outcome,".pdf"), width = 6, height =7)






# SAMPLE SIZE PLOT ######################################################

outcome   <- "college"
fun_pred  <- "no middle"
n_boot <- 10000
which_metric <- "TNR"

# Read results for a metric
results <- lapply(c("WLS","ELSA"), function(ds) {
  readRDS(paste0("results/",ds,"_",outcome,"_",fun_pred,"_",n_boot,".rsd")) %>% 
    filter(metric %in% c("TPR","TNR")) %>%
    mutate(dataset = ds) %>% 
    select(dataset, group, high_OUT, n, metric, value, ci_lower, ci_upper) %>%
    arrange(dataset, group, metric, n)
}) %>% bind_rows()

# Add AH
results <- results_AH %>% 
  select(dataset, group, high_OUT, n, metric, value, ci_lower, ci_upper) %>%
  bind_rows(results)
results

# Compute p
results <- results %>% 
  group_by(dataset, group) %>%
  mutate(prevalence = n[high_OUT == 1] / sum(n)) %>%
  ungroup()

# Filter one metric
results <- results %>%
  filter(metric == which_metric)


# x label
#x_labs <- c("TPR"=)

# Plot with n
ggplot(results, aes(x=prevalence, y=value, color=group)) +
  geom_point(size=3) +
  geom_text(aes(label=dataset), color ="black", vjust=-3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.01, linewidth = 1) +
  labs(y=which_metric) +
  ylim(c(0, 1)) + 
  geom_hline(yintercept = mean(results$value))


