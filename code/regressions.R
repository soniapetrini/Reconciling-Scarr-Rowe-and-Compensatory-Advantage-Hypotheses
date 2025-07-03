
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


source("code/funs.R")


# MAIN TABLE ######################################################

outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"


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






# DESCRIPTIVES ######################################################

# Sample sizea
sapply(c("WLS","ELSA"), function(ds) nrow(get_data(ds)))






# CONFUSION MATRICES ######################################################

df <- get_data(ds)

# --- Check overall prevalence
sum(df[outcome]==1)/nrow(df) 


# ---  Confusion matrix
# Split by SES
df_low  <- df %>% filter(SES == "Low SES")
df_high <- df %>% filter(SES == "High SES")

# Dichotomize
df_low  <- dichotomize(df_low, outcome, predictor, fun_out, fun_pred, resid=T)
df_high <- dichotomize(df_high,outcome, predictor, fun_out, fun_pred, resid=T)

table(df_low$high_PRED, df_low$high_OUT)   %>% confusionMatrix(positive="1")
table(df_high$high_PRED, df_high$high_OUT) %>% confusionMatrix(positive="1")









# ALL REGRESSIONS ##################################################################

source("code/funs.R")

outcome   <- "college"
predictor <- "pgi_education"

datasets <- c("WLS","ELSA")


plots <- lapply(datasets, function(ds) {
  
  # - 1 - Prepare
  data     <- get_data(ds)
  data[predictor] = as.vector(scale(data[predictor]))
  data$SES <- factor(data$SES, levels = c("Low SES", "High SES"))
  
  
  # ---  Create all interactions
  controls          <- c(DEMO, PC_vars)
  pred_interactions <- paste0(predictor, "*", controls)
  ses_interactions  <- paste0("SES*", controls)
  
  # Combine in formula
  formula <- paste0(outcome, " ~ ", predictor, "*SES + ",
                    paste(pred_interactions, collapse = " + "), " + ",
                    paste(ses_interactions, collapse = " + "))
  formula = as.formula(formula)
  
  
  
  # - 2 - Run models
  # LOGIT
  model_logit <- glm(formula, data = data, family = binomial(link = "logit"))
  summary(model_logit)
  
  
  # LPM
  model_ols <- lm(formula, data = data)
  summary(model_ols)
  
  
  # - 3 - extract coefficients
  
  annots <- lapply(c("logistic","LPM"), function(model_name) {
    
    model <- switch(model_name, "LPM" = model_ols, "logistic"=model_logit)
    coefs <- coef(summary(model))[paste0(predictor,":SESHigh SES"), ]
    coef  <- coefs["Estimate"]
    
    if(model_name=="logistic") {
      coef <- round(exp(coef), 1)
      coef_label <- "OR:"
      pvalue     <- coefs["Pr(>|z|)"]
    } else {
      coef <- round(coef, 2)
      coef_label <- "beta:"
      pvalue     <- coefs["Pr(>|t|)"]
    }
    
    pvalue_label <- add_stars(pvalue)
    
    data.frame(
      model = model_name,
      label = paste(coef_label, coef, pvalue_label)
    )
  })
  
  annotations <- do.call(rbind, annots) %>% 
    mutate(model = factor(model, levels=c("logistic","LPM"), labels=c("Logistic", "LPM"))) 
  
  
  # - 4 - Plot
  # Logit
  preds <- ggpredict(model_logit, terms = c(paste(predictor,"[all]"), "SES"))
  
  plot_logit <- plot(preds) +
    geom_text(data = filter(annotations, model=="Logistic"), 
              aes(label = label),
              x = -1.5,
              y = 0.93,
              hjust = 0, 
              size=6,
              inherit.aes = FALSE) +
    labs(title="", x = "PGI", y = "Predicted probability") +
    theme_minimal() +
    theme(text=element_text(size=20)) +
    guides(color = "none") +
    scale_color_manual(values=groups.labs) +
    ylim(c(0,1)) +
    xlim(c(-2,2))
  
  # LPM
  preds <- ggpredict(model_ols, terms = c(paste(predictor,"[all]"), "SES"))
  plot_OLS <- plot(preds) +
    geom_text(data = filter(annotations, model=="LPM"), 
              aes(label = label),
              x = -1.5,
              y = 0.93,
              hjust = 0, 
              size=6,
              inherit.aes = FALSE) +
    labs(title="", x = "PGI", y = "Predicted probability") +
    theme_minimal() +
    theme(text=element_text(size=20)) +
    scale_color_manual(values=groups.labs, name="") +
    ylim(c(0,1)) +
    xlim(c(-2,2))
  
  plot <- ggarrange(plot_OLS, plot_logit, 
                    ncol=1, common.legend = T, 
                    legend = "bottom")
  plot
  
  ggsave(paste0("plots/",ds,"_regs.jpg"), width = 5, height = 10)
  
})


ggarrange(plotlist = plots, ncol=2,
          common.legend = T, 
          legend = "bottom")

ggsave(paste0("plots/regs.jpg"), width = 12, height = 10)







# LOGISTIC BY GROUP ######################################################
set.seed(123)
library(pROC)
library(olsrr)
library(lmtest)
library(ggfortify)

source("code/funs.R")

ds        <- "ELSA"
outcome   <- "college"
predictor <- "pgi_education"


# --- DATA
df <- get_data(ds)

# Split by SES
df_low  <- df %>% filter(SES == "low SES")
df_high <- df %>% filter(SES == "high SES")

# Normalize PGI
df_low[[predictor]]  = as.vector(scale(df_low[predictor], center = T))
df_high[[predictor]] = as.vector(scale(df_high[predictor], center = T))

# ---  Create formula
controls = c(DEMO, PC_vars)
#pred_interactions <- paste0(predictor, "*", controls)
formula = paste0(outcome, " ~ ", predictor, "+ ",paste(controls, collapse = " + "))
formula = as.formula(formula)
print(formula)

# --- MODELS

# -- Low SES --
logit_low <- glm(formula, data = df_low, family = binomial(link = "logit"))
summary(logit_low)

# Predict
df_low$pred_prob  <- predict(logit_low, type = "response") 

# Real outcome
df_low$real_class <- factor(as.character(df_low[[outcome]]), levels = c(0, 1))



# -- High SES --
logit_high <- glm(formula, data = df_high, family = binomial(link = "logit"))
summary(logit_high)

# Predict
df_high$pred_prob  <- predict(logit_high, type = "response") 

# Real outcome
df_high$real_class <- factor(as.character(df_high[[outcome]]), levels = c(0, 1))







# --- Plot predicted vs pgi

df_all <- bind_rows(df_high, df_low) %>% 
  group_by(SES) %>%
  mutate(index     = row_number())

ggplot(df_all, aes(x=pgi_education, y=pred_prob, color=SES)) + 
  geom_point() + 
  facet_wrap(~get(outcome), ncol=1) +
  ylim(0,1) +
  labs(y="Predicted Probability", 
       title=paste(ds, " - ", outcome.labs[outcome]))



# --- Plot TPR and TNR for different thresholds
thresholds <- seq(0.1, 0.9, by = 0.01)

tpr_data <- tibble(
  threshold = rep(thresholds, 2),
  SES = rep(c("low SES", "high SES"), each = length(thresholds)),
  TPR = c(
    map_dbl(thresholds, ~calc_tpr(df_low$pred_prob,  df_low$real_class,  .x)),
    map_dbl(thresholds, ~calc_tpr(df_high$pred_prob, df_high$real_class, .x))
  ),
  TNR = c(
    map_dbl(thresholds, ~calc_tnr(df_low$pred_prob,  df_low$real_class,  .x)),
    map_dbl(thresholds, ~calc_tnr(df_high$pred_prob, df_high$real_class, .x))
  )
) %>% mutate(SES = factor(SES, levels=c("low SES", "high SES")))



# Find closest TPR matches
find_equal_tpr <- function(target_tpr) {
  
  low_threshold <- tpr_data %>% 
    filter(SES == "low SES") %>%
    mutate(diff = abs(TPR - target_tpr)) %>%
    slice_min(diff) %>% 
    mutate(threshold = ifelse(diff>=0.02, NA, threshold)) %>%
    pull(threshold)
  
  high_threshold <- tpr_data %>% 
    filter(SES == "high SES") %>%
    mutate(diff = abs(TPR - target_tpr)) %>%
    slice_min(diff) %>% 
    mutate(threshold = ifelse(diff>=0.02, NA, threshold)) %>%
    pull(threshold)
  
  # Calculate resulting FPRs
  low_tnr  <- calc_tnr(df_low$pred_prob, df_low$real_class, low_threshold)
  high_tnr <- calc_tnr(df_high$pred_prob, df_high$real_class, high_threshold)
  
  tibble(
    target_TPR     = target_tpr,
    low_threshold  = low_threshold,
    high_threshold = high_threshold,
    low_TNR        = low_tnr,
    high_TNR       = high_tnr
  )
}


# --- AUC and ROC curve

# Compute ROC objects
roc_high <- roc(df_high$real_class, df_high$pred_prob)
roc_low  <- roc(df_low$real_class,  df_low$pred_prob)


# delta(spec) versus sensitivity

all_res <- lapply(seq(0.1,0.9,0.1), function(target_sens) {
  test_roc    <- roc.test(roc_high, roc_low, method="sensitivity", sensitivity=target_sens)
  results_roc <- find_equal_tpr(target_sens) %>% 
    mutate(delta_TNR = high_TNR - low_TNR,
           pval = test_roc$p.value)
  results_roc
})

bind_rows(all_res) %>%
  mutate(pval_bonferroni = p.adjust(pval, method = "bonferroni"),
         stars = add_stars(pval_bonferroni)) %>%
  ggplot(aes(x=target_TPR, y=delta_TNR)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="sensitivity", y="Delta (specificity)", title=paste(ds,"-",outcome)) +
  geom_text(aes(y = delta_TNR+0.01, label = stars),
            color = "black",
            size = 8,
            position = position_dodge(width = 0)) +
  ylim(-0.2,0.05) +
  xlim(0,1)

ggsave(paste0("plots/logistic/DeltaSpec_",ds,"_",outcome,".pdf"), width = 6,height = 5)







# Convert ROC data to data frames
df_roc_high <- data.frame(
  TPR = rev(roc_high$sensitivities),
  FPR = rev(1 - roc_high$specificities),
  SES = "High SES"
)

df_roc_low <- data.frame(
  TPR = rev(roc_low$sensitivities),
  FPR = rev(1 - roc_low$specificities),
  SES = "Low SES"
)

# Combine both into one data frame
df_roc_combined <- bind_rows(df_roc_high, df_roc_low) %>% 
  mutate(SES = factor(SES, levels=c("Low SES", "High SES")))

# Plot with ggplot2
ggplot(df_roc_combined, aes(x = FPR, y = TPR, color = SES)) +
  geom_line(linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = paste0("AUC High = ", round(auc(roc_high), 3),
                   ", AUC Low = ", round(auc(roc_low), 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")









# Reshape data
long_data <- tpr_data %>%
  pivot_longer(cols = c(TPR, TNR), names_to = "metric", values_to = "rate")

# Plot TPR/FPR
ggplot(long_data) +
  geom_line(aes(x=threshold, y=rate, color=SES, linetype=metric), linewidth=2) +
  scale_linetype_manual(values = c("TPR" = "solid", "TNR" = "dotted")) +
  theme(legend.position = "bottom") +
  labs(title=paste(ds, " - ", outcome.labs[outcome]))










# Find thresholds for different TPR levels
target_tprs <- c(0.2, 0.3, 0.4)
target_tprs <- c(0.6, 0.7, 0.8)
target_tprs <- 0.8
results     <- map_dfr(target_tprs, find_equal_tpr)

print(results)





















# HETEROSKEDASTICTY ##################################################################


ds <- "WLS"
df   <- get_data(ds)
data <- dichotomize(df, resid=T)

# Separate data by SES
high_ses_data <- filter(data, SES == "high SES")
low_ses_data <- filter(data, SES == "low SES")


# Linear models
# full sample
formula <- as.formula("education ~ pgi_education + sex + birth_year + SES*pgi_education")
model_ols <- lm(formula, data = data)
summary(model_ols)
model_ols$coefficients

# by SES
formula <- as.formula("education ~ pgi_education + sex + birth_year")
high_ses_model <- lm(formula, data = high_ses_data)
low_ses_model <- lm(formula, data = low_ses_data)


# diagnostic plots
autoplot(high_ses_model) + ggtitle(paste0("real data high SES"))
autoplot(low_ses_model) + ggtitle(paste0("real data low SES"))
autoplot(model_ols) + ggtitle(paste0("real data"))

# Breusch-Pagan test for heteroscedasticity
bptest(high_ses_model)
bptest(low_ses_model)
bptest(model_ols)



