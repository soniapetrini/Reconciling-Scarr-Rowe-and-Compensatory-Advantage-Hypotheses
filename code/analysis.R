setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")



# CONFUSION MATRIX #####################################

ds   <- "WLS"
outcome   <- "college"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"
dichot    <- "group"

# Read
df   <- get_data(ds)

# Split by SES
df_low <- df %>% filter(SES == "low SES")
df_high <- df %>% filter(SES == "high SES")


# Dichotomize
df_low   <- dichotomize(df_low,outcome, predictor, fun_out, fun_pred, resid=T)
df_high <- dichotomize(df_high,outcome, predictor, fun_out, fun_pred, resid=T)

table(df_low$high_PRED, df_low$high_OUT) %>% confusionMatrix(positive="1")
table(df_high$high_PRED, df_high$high_OUT) %>% confusionMatrix(positive="1")




# PGI DISTRIBUTION by SES #####################################

ds <- "WLS"
df <- get_data(ds)

df %>% 
  ggplot(aes(x = pgi_education, fill = SES)) +
  geom_density(alpha = 0.5) 



# OUTCOME DISTRIBUTION #####################################

group <- "SES"

ds <- "WLS"
df <- get_data(ds)

outcome   <- "education"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"
  
# education by group
df %>% group_by(SES) %>%
  dichotomize(outcome, predictor, fun_out, fun_pred) %>%
  ggplot(aes(x =  factor(high_PRED, levels=c(0,1), labels=c("low PGI","high PGI")), y = get(outcome))) +
  geom_jitter(aes(color = get(group)), width = 0.2, alpha = 0.5) +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = after_stat(y), ymin = after_stat(y)), 
               width = 0.75, linetype = "dashed", linewidth = 1) +
  labs(title = paste(ds),x = group,y = outcome,color = group) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~get(group))

ggsave(paste0("plots/edu_boxplots_",ds,".pdf"), width = 13, height = 10)






# BIVARIATE DENSITY
devtools::install_github("jtlandis/ggside")

library(ggside)

outcome   <- "education"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"

ggplot(data = df, 
       aes(x = get(predictor), y = get(outcome))) +
  geom_point(aes(col = SES)) +
  geom_xsidedensity(aes(fill = SES), alpha = 0.5) +
  geom_ysidedensity(aes(fill = SES), alpha = 0.5) +
  labs(x=predictor, y=outcome)












# ROC CURVE ##################################################################

library(plotROC)

outcome   <- "education"
predictor <- "pgi_education"
fun_out   <- "median"
fun_pred  <- "no middle"
dichot    <- "group"


plot_roc <- function(df, grouping_var="SES") {

  # Create plot data for each group
  groups <- unique(df$SES)
  
  # Compute precision and recall
  pr_data <- lapply(groups, function(g) {
    subset_data <- filter(df, SES == g)
    dichotomize(subset_data, outcome, predictor, fun_out, fun_pred)
  })
  
  df_roc <- bind_rows(pr_data)
  
  # plot
  ggplot(df_roc, aes(m = pgi_education, d = high_OUT, color=get(grouping_var))) + 
    geom_roc(size=2,pointsize = 1, labelround = 2,
             cutoffs.at=c(-0.01,-0.08, -0.13, -0.17, -0.22, -0.26, -0.31, -0.37,-0.6)) +
    theme_minimal() +
    labs(x="False Positive Rate",y="True Positive Rate",
         color=grouping_var) +
    theme(
      text=element_text(size=15),
      legend.position = "bottom",
      legend.title=element_blank())
}


lapply(DATASETS, function(ds) {
  df <- get_data(ds)
  plot_roc(df)
  ggsave(paste0("plots/ROC_",ds,".pdf"), width = 15, height = 12)
})




# PRECISION-RECALL ##################################################################

source("code/funs.R")

# precision recall with noramlized prevalence
plot_precision_recall <- function(df, normalize=T) {
  
  # Create plot data for each group
  groups <- unique(df$SES)
  
  
  
  # Compute precision and recall
  pr_data <- lapply(groups, function(g) {
    
    subset_data <- filter(df, SES == g)
    
    # Dichotomize
    subset_data <- dichotomize(subset_data)
    
    # Calculate precision-recall pairs at different thresholds
    pr_curve <- pr.curve(
      scores.class0  = subset_data$pgi_education, 
      weights.class0 = subset_data$high_OUT,
      curve = TRUE
    )
    
    # Get baseline precision for this group
    group_baseline <- mean(subset_data$high_OUT)
    
    # Convert to data frame for ggplot and normalize precision
    group_pr_df <- data.frame(
      recall             = pr_curve$curve[, 1],
      original_precision = pr_curve$curve[, 2],
      # Normalize precision to have baseline of 0.5
      norm_precision     = 0.5 + (pr_curve$curve[, 2] - group_baseline),
      group              = g,
      auc                = pr_curve$auc.integral,
      baseline           = group_baseline
    )
    
    group_pr_df
  })
  # Combine for both groups
  pr_data <- bind_rows(pr_data)
  
  
  
  
  # Normalization if required
  pr_data$precision <- if (normalize) pr_data$norm_precision else pr_data$original_precision
  y_lab <- if (normalize) "Normalized High PGI Predictive Power" else "High PGI Predictive Power"
  
  # Plot
  p <- ggplot(pr_data, aes(x = recall, y = precision, color = group)) +
    geom_line(linewidth = 2) +
    labs(x = "True Positive Rate", y = y_lab) +
    scale_color_manual(values = c("high SES" = "#2c7bb6", "low SES" = "#d7191c")) +
    theme_minimal() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    # Add standardized baseline
    {if (normalize) geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") } +
    theme(legend.position = "bottom",
      legend.title = element_blank()
    ) 
  
  # Add AUC annotation and original baselines for reference
  for (i in 1:length(groups)) {
    g <- groups[i]
    auc_value         <- unique(pr_data$auc[pr_data$group == g])
    original_baseline <- unique(pr_data$baseline[pr_data$group == g])
    
    p <- p + 
      annotate("text", 
               x = 0.75, 
               y = 0.75 + (i-1)*0.1, 
               label = sprintf("%s AUC = %.3f (baseline: %.2f)", g, auc_value, original_baseline),
               color = ifelse(g == "high SES", "#2c7bb6", "#d7191c"),
               size = 5) +
      
      {if (!normalize) geom_hline(yintercept = original_baseline, 
                                  linetype = "dashed",
                                  color = ifelse(g == "high SES", "#2c7bb6", "#d7191c")) }
  }
  
  p
}


normalize <- F
normalize_lab <- ifelse(normalize, "_normalized","")

lapply(DATASETS, function(ds) {
  
  df <- get_data(ds)
  
  plot_precision_recall(df, normalize) + ggtitle(ds)
  
  ggsave(paste0("plots/PROC",normalize_lab,"_",ds,".jpg"), width = 10, height = 7)
  
})






# TABLE OF RESULTS ##################################################################


# Usage:
latex_table <- create_metrics_table(df)
cat(latex_table)  



# Calculate metrics for both ds
results <- lapply(c("WLS","SOEP"), function(ds) {
  
  grouping_var = "SES"
  df <- readRDS(paste0("data/",ds,"/df.rds"))
  
  results <- lapply(metrics, function(met) {
    calculate_group_metrics(df, grouping_var, metric = met) %>%
      mutate(grouping_var = grouping_var)
  })
  results <- do.call(cbind, results) %>% mutate(data = ds)
  results <-  do.call(cbind, results) 
})

# Combine all results
all_results <- do.call(rbind, results)






# VARIANCE FUNCTION REGRESSION  ##############################################

df <- get_data(ds)
data <- dichotomize(df) %>% 
  mutate(SES = factor(SES),
         high_PRED=factor(high_PRED))

# gamlss  --------
library(gamlss)
model <- gamlss(education ~ high_PRED + SES + SES*high_PRED + sex + birth_year,  # Mean model
                sigma.formula = ~ high_PRED + SES + SES*high_PRED + sex + birth_year,  # Variance model
                family = NO(),  # Normal distribution
                data = data)

summary(model)

plot2way(model, c("high_PRED","SES"), what="sigma") 
term.plot(model, what = "sigma")


# dglm --------
install.packages("dglm")
library(dglm)

model <- dglm(education ~ high_PRED + SES + SES*high_PRED + sex + birth_year,  # Mean model
              dformula = ~ high_PRED + SES + SES*high_PRED + sex + birth_year,  # Variance model
              data = data)

summary(model)



