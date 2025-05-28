# Method 3: Residual Analysis by Group and Range for Different SES Groups

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(boot)
library(tidyr)


ds <- "WLS"
data <- get_data(ds)


# Step 1: Residualize PGI for control variables (birth_year and sex)
residual_model <- lm(pgi_education ~ birth_year + sex, data = data)
data$pgi_residualized <- residuals(residual_model)

# Step 2: Create bins for bottom and top PGI 
quantiles    <- quantile(data$pgi_residualized, probs = c(0.40, 0.60))
data$pgi_bin <- cut(data$pgi_residualized, 
                    breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                    labels = c("Bottom 40%", "Middle 20%", "Top 40%"))

# Step 4: Run regression model to predict education
pred_model <- lm(education ~ pgi_residualized * SES + birth_year + sex, 
                 data = data)

# Calculate predicted values and residuals
data$predicted <- predict(pred_model)
data$residuals <- residuals(pred_model)

# Step 5: Calculate error metrics by group and bin
results <- data %>%
  filter(pgi_bin %in% c("Bottom 40%", "Top 40%")) %>%
  group_by(SES, pgi_bin) %>%
  summarize(
    n = n(),
    mean_residual = mean(residuals),
    mse = mean(residuals^2),
    mae = mean(abs(residuals)),
    rmse = sqrt(mean(residuals^2)),
    sd_residual = sd(residuals),
    .groups = "drop"
  )

# Step 6: Bootstrap to get confidence intervals
# Function to bootstrap error metrics
boot_errors <- function(data, indices) {
  d <- data[indices, ]
  mean_resid <- mean(d$residuals)
  mse <- mean(d$residuals^2)
  mae <- mean(abs(d$residuals))
  rmse <- sqrt(mse)
  return(c(mean_resid, mse, mae, rmse))
}

# Run bootstrap for each group and bin
confidence_intervals <- data %>%
  filter(pgi_bin %in% c("Bottom 40%", "Top 40%")) %>%
  group_by(SES, pgi_bin) %>%
  group_modify(~ {
    boot_results <- boot(data = .x, statistic = boot_errors, R = 1000)
    
    # Get CIs for each metric
    ci_mean <- boot.ci(boot_results, type = "perc", index = 1)$percent[4:5]
    ci_mse <- boot.ci(boot_results, type = "perc", index = 2)$percent[4:5]
    ci_mae <- boot.ci(boot_results, type = "perc", index = 3)$percent[4:5]
    ci_rmse <- boot.ci(boot_results, type = "perc", index = 4)$percent[4:5]
    
    data.frame(
      metric = c("mean_residual", "mse", "mae", "rmse"),
      ci_lower = c(ci_mean[1], ci_mse[1], ci_mae[1], ci_rmse[1]),
      ci_upper = c(ci_mean[2], ci_mse[2], ci_mae[2], ci_rmse[2])
    )
  }) %>%
  ungroup()

# Merge results with confidence intervals
results_long <- results %>%
  pivot_longer(cols = c(mean_residual, mse, mae, rmse, sd_residual),
               names_to = "metric", values_to = "value") %>%
  filter(metric %in% c("mean_residual", "mse", "mae", "rmse"))

final_results <- left_join(
  results_long,
  confidence_intervals,
  by = c("SES", "pgi_bin", "metric")
)

# Return results, model, and the processed data
list(
  summary_table = final_results,
  model = pred_model,
  processed_data = data
)











# Function to visualize the results
plot_residual_analysis <- function(analysis_results) {
# Extract the data
data <- analysis_results$processed_data
summary <- analysis_results$summary_table

# 1. Plot residuals by PGI bin and SES group (boxplot)
p1 <- data %>%
  filter(pgi_bin %in% c("Bottom 40%", "Top 40%")) %>%
  ggplot(aes(x = pgi_bin, y = residuals, fill = ses_group)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Residuals by PGI Bin and SES Group",
    x = "PGI Bin",
    y = "Residuals (Years of Education)",
    fill = "SES Group"
  ) +
  scale_fill_brewer(palette = "Set1")

# 2. Bar plot of error metrics
p2 <- summary %>%
  filter(metric %in% c("rmse", "mae")) %>%
  ggplot(aes(x = interaction(pgi_bin, metric), y = value, fill = ses_group)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Error Metrics by PGI Bin and SES Group",
    x = "",
    y = "Error Value (Years of Education)",
    fill = "SES Group"
  ) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~pgi_bin, scales = "free_x")

# 3. Scatter plot of actual vs predicted values
p3 <- data %>%
  filter(pgi_bin %in% c("Bottom 40%", "Top 40%")) %>%
  ggplot(aes(x = predicted, y = education, color = ses_group)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(ses_group ~ pgi_bin) +
  theme_minimal() +
  labs(
    title = "Actual vs. Predicted Education by PGI Bin and SES Group",
    x = "Predicted Years of Education",
    y = "Actual Years of Education",
    color = "SES Group"
  ) +
  scale_color_brewer(palette = "Set1")

# Return the plots
return(list(
  residual_boxplot = p1,
  error_metrics = p2,
  prediction_scatter = p3
))
}

# Usage example (assuming your data is in a dataframe called 'your_data')
# results <- analyze_residuals_by_group_range(your_data)
# plots <- plot_residual_analysis(results)
# 
# # View the summary table
# print(results$summary_table)
# 
# # Display the plots
# plots$residual_boxplot
# plots$error_metrics
# plots$prediction_scatter
# 
# # Statistical tests for differences in error metrics
# # Test if error is different between SES groups at the bottom 40% of PGI
# bottom_test <- results$summary_table %>%
#   filter(pgi_bin == "Bottom 40%" & metric == "rmse")
# 
# # Test if error is different between SES groups at the top 40% of PGI
# top_test <- results$summary_table %>%
#   filter(pgi_bin == "Top 40%" & metric == "rmse")