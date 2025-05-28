rm(list=ls())

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")






# PLOT SINGLE DATASET ##########################################################

# For each dataset
lapply(c("WLS", "ELSA"), function(ds) {

  # Settings
  metrics <- c("TNR","TPR", "NPV", "PPV")
  outcome   <- "college"
  predictor <- "pgi_education"
  fun_out   <- "median"
  fun_pred  <- "no middle"
  dichot    <- "group"
  
  # Get data
  data <- get_data(ds)
  
  # Compute all metrics
  results <- lapply(metrics, 
                    compute_group_metrics, 
                    data=data,
                    dichot=dichot,
                    outcome=outcome, predictor=predictor, 
                    fun_out=fun_out, fun_pred=fun_pred)
  results <- bind_rows(results)
  
  # Plot
  plot_rates(results) + 
    ggtitle(paste0(ds, ": ",predictor, " predicts ", outcome.labs[outcome]))
    
  
  # Print results
  print(results)
  
  met_lab    <- ifelse("TNR" %in% metrics, "_rates", "")
  ggsave(paste0("plots/metrics",met_lab,"_",outcome,"_",predictor,"_",dichot,"_",ds,".jpeg"), width = 10, height = 10)
})






# PLOT ALL DATASETS  ##########################################################
source("code/funs.R")

metrics <- c("TNR","TPR")
label_metrics <- ifelse("PPV" %in% metrics, "", "_rates")

all_results <- lapply(DATASETS, function(ds) {
  # Select how to dichomotize PGI
  # Compute
  data    = get_data(ds)
  results = lapply(metrics, compute_group_metrics, data=data)
  bind_rows(results) %>% mutate(dataset=ds)
})

# Combine
results = bind_rows(all_results) %>% mutate(dataset = factor(dataset, levels=DATASETS))

# Plot
plot_rates(results) + 
  facet_wrap(~dataset) + 
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=18),
        legend.spacing.x = unit(20.0, 'cm'))
ggsave(paste0("plots/metrics",label_metrics,"_global.jpeg"), width = 11, height = 5)






# PREVALENCE ##########################################################


rows <- lapply(DATASETS, function(ds) {
  
  print(ds)
  df <- get_data(ds)
  
  rows_ds <- lapply(c("high SES","low SES"), function(g) {
    
    outcome   <- "cognitive"
    predictor <- "pgi_cognitive"
    fun_out   <- "median"
    fun_pred  <- "no middle"
    
    # Dichotomize
    group_df <- df #%>% filter(SES == g)
    group_df <- dichotomize(group_df, outcome, predictor, fun_out, fun_pred) 
    
    group_df <- filter(group_df, SES == g)
    
    # sample size per EA group
    n_low_OUT  <- nrow(filter(group_df, high_OUT==0))
    n_high_OUT <- nrow(filter(group_df, high_OUT==1))
    
    group_df %>%
      summarize(
        dataset = ds,
        group   = g,
        N_low_OUT  = n_low_OUT,
        N_high_OUT = n_high_OUT,
        prev    = round(mean(high_OUT),2)
      )
  })
  bind_rows(rows_ds) 
})
prev_df <- bind_rows(rows)

prev_df







# GENDER ANALYSIS ##########################################################

metrics <- METRICS
metrics <- c("TNR","TPR")
label_metrics <- ifelse("PPV" %in% metrics, "", "_rates")


all_results <- lapply(DATASETS, function(ds) {
  
  # Filter sex groups
  data_male   <- get_data(ds) %>% filter(sex=="male")
  data_female <- get_data(ds) %>% filter(sex=="female")
  
  # Compute all metrics
  results_male   <- lapply(metrics, compute_group_metrics, data=data_male)
  results_female <- lapply(metrics, compute_group_metrics, data=data_female)
  
  # Combine results for metrics
  results_male   <- do.call(rbind, results_male)   %>% mutate(sex="male")
  results_female <- do.call(rbind, results_female) %>% mutate(sex="female")
  
  # Combine results for gender
  rbind(results_male, results_female) %>% mutate(ds=ds)
})

# Combine
results = do.call(rbind, all_results) %>% mutate(ds = factor(ds, levels=DATASETS))

# Plot
plot_rates(results) + 
  facet_grid(sex~ds) + 
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=18),
        legend.spacing.x = unit(20.0, 'cm'))
ggsave(paste0("plots/metrics_gender",label_metrics,"_global.jpeg"), width = 9, height = 6)




# BIRTH YEAR ANALYSIS ##########################################################



# Sample size
res <- lapply(DATASETS,function(ds) {
  
  df <- get_data(ds)
  
  # Split by group
  low_data  <- filter(df, SES == "low SES")  %>% dichotomize()
  high_data <- filter(df, SES == "high SES") %>% dichotomize()
  
  tot_N <- nrow(low_data) + nrow(high_data)
  cat(ds, "N =",tot_N,"\n")
})




# - 1 -  Check yob distributions  #########################

WLS  <- get_data("WLS")  %>% dichotomize() %>% mutate(dataset = "WLS")
SOEP <- get_data("SOEP") %>% dichotomize() %>% mutate(dataset = "SOEP")
ELSA <- get_data("ELSA") %>% dichotomize() %>% mutate(dataset = "ELSA")
datas <- bind_rows(WLS, SOEP, ELSA)

datas <- datas %>%
  mutate(dataset = factor(dataset,levels = rev(DATASETS)))

# Boxplot
ggplot(datas) + 
  geom_violin(aes(x=birth_year, y=dataset, fill = dataset),alpha=0.2) +
  labs(x="birth year", y="") + 
  theme_bw() +
  theme(text = element_text(size=15))

ggsave(paste0("plots/birth_year_distributions.jpeg"), width = 5, height = 4)




# - 2 -  Cohorts #########################

metrics <- METRICS
metrics <- c("TNR", "TPR")
label_metrics <- ifelse("PPV" %in% metrics, "", "_rates")

plots <- lapply(DATASETS, function(ds) {
  
  data <- get_data(ds) 
  
  if (ds=="SOEP") {
    age_range <- max(data$birth_year)-min(data$birth_year)
    cutoff_1 <- (min(data$birth_year)+round(age_range)/3) %>% round()
    cutoff_2 <- (min(data$birth_year)+2*round(age_range)/3) %>% round()
    
    print(ds)
    print(paste("cutoff_age1:", cutoff_1))
    print(paste("cutoff_age2:", cutoff_2))
    
    data_1 <- data %>% filter(birth_year <= cutoff_1)
    data_2 <- data %>% filter(birth_year > cutoff_1 & birth_year <= cutoff_2)
    data_3 <- data %>% filter(birth_year > cutoff_2)
    datas  <- list(data_1, data_2, data_3)
    
  } else {
    cutoff_age <- median(data$birth_year) %>% round()
    
    print(ds)
    print(paste("cutoff_age:", cutoff_age))
    
    data_1 <- data %>% filter(birth_year < cutoff_age)
    data_2 <- data %>% filter(birth_year >= cutoff_age)
    datas  <- list(data_1, data_2)
  }
  
  # Summary
  lapply(datas, function(data) summary(data$birth_year))
  lapply(datas, nrow)
  
  # Compute all metrics
  fun_pred <- "median"
  results <- lapply(datas, function(data) {
    # Compute
    result <- compute_all_metrics(data, metrics)
    # Add lab
    result$cohort = paste0(ds," (",min(data$birth_year),"-",max(data$birth_year),")    N=",nrow(data))
    result
  })
  
  # Combine
  results <- bind_rows(results)
  
  # Plot
  plot_rates(results) + facet_wrap(~cohort) + theme(legend.position = "bottom")
  
  ggsave(paste0("plots/metrics_cohorts_",ds,label_metrics,"_global.jpeg"), width = ifelse(ds=="SOEP",9,7), height = 5)
  
})




# - 3 -  Sliding time window  #########################

metrics <- METRICS
metrics <- c("TNR", "TPR")
label_metrics <- ifelse("PPV" %in% metrics, "", "_rates")

ds   <- "ELSA"
data <- get_data(ds) 

# Create running window
min_yob = min(data$birth_year)
max_yob = max(data$birth_year)
window_size = 30
start_years = seq(min_yob, max_yob - window_size + 1)

# Compute for all windows
all_cohorts <- lapply(start_years, function(start_year) {
  
  # Find years window
  end_year    <- start_year + window_size - 1
  data_window <- data %>% filter(birth_year >= start_year & birth_year < end_year)
  
  # Compute
  result <- lapply(metrics, compute_group_metrics, data=data_window)
  result <- bind_rows(result)
  
  # Add indices
  result$label  = paste0(ds," (",min(data_window$birth_year),"-",max(data_window$birth_year),")    N=",nrow(data_window))
  result$cohort = start_year + (end_year-start_year)/2
  result
})

# Combine
results <- bind_rows(all_cohorts)

# Get the available metrics from your data
available_metrics <- unique(results$metric)

# Convert to factors for plotting
results <- results %>%
  mutate(metric = factor(metric, levels = available_metrics, labels = metrics.labs[available_metrics]),
         group  = factor(group, levels=c("low SES","high SES")))


# Create the plot
ggplot(results, aes(x = cohort, y = value, color = group, group=group)) +
  geom_line(linewidth = 1) +
  labs(y="Predictive Value") +
  facet_wrap(~metric, ncol=1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper, fill=group), alpha=0.2) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.spacing.x = unit(10.0, 'cm')) +
  ggtitle(paste0(ds, " - Running window size: ",window_size," years")) +
  scale_color_discrete(name="")+
  scale_fill_discrete(name="")

ggsave(paste0("plots/metrics_evolution_",ds,"_global.jpeg"), width = 6, height = 5)






# - 4 - central years #########################

all_results <- lapply(DATASETS, function(ds) {
  
  data <- get_data(ds) 
  
  # Find middle 20 years
  min_yob <- 1930 #median(data$birth_year) - 10
  max_yob <- 1950 #median(data$birth_year) + 10
  
  # Filter data
  data_window <- data %>% filter(birth_year>=min_yob & birth_year<=max_yob)
  
  # Compute metrics
  results <- lapply(METRICS, compute_group_metrics, data=data_window)
  N <- sum(results[[1]]$n) + sum(results[[2]]$n)
  
  bind_rows(results) %>% 
    mutate(dataset=ds) %>%
    mutate(label = paste0(ds, " (",min_yob,"-",max_yob,")    N=", N))
})

# Combine
results = bind_rows(all_results) %>% mutate(dataset = factor(dataset, levels=DATASETS))

# Create a new ordered factor for labels that preserves dataset order
label_order <- results %>%
  select(dataset, label) %>%
  distinct() %>%
  arrange(dataset) %>%
  pull(label)

results <- results %>%
  mutate(ordered_label = factor(label, levels = label_order))

# Plot
plot_rates(results) + 
  facet_wrap(~ordered_label) + 
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=18),
        legend.spacing.x = unit(20.0, 'cm'))
ggsave(paste0("plots/metrics_central_years.jpeg"), width = 11, height = 5)









# SUMMARY TABLES ###############################################################


# Sample size
lapply(DATASETS,function(ds) {
  
  df <- get_data(ds)
  
  # Split by group
  group1_data <- filter(df, SES == "high SES")
  group2_data <- filter(df, SES == "low SES")
  
  n1= group1_data %>% dichotomize(fun_pred="no middle", fun_out="mean") %>% nrow()
  n2= group2_data %>% dichotomize(fun_pred="no middle", fun_out="mean") %>% nrow()
  
  print(ds)
  print(n1+n2)
})



# Summary statistics by dataset
summaries <- lapply(DATASETS,summary_stats)

# Combine the results
combined_summary <- do.call(rbind, summaries) %>% 
  select(Dataset, SES, n, mean_education, sd_education, sex, mean_yob, sd_yob)

# Create the xtable
xtable_output <- xtable(combined_summary, 
                        caption = "Summary Statistics by SES, for each dataset. 
                        Standard deviations in parentheses. Sex is reported as the share of females.",
                        label = "tab:summary")

# Print with appropriate LaTeX formatting
print(xtable_output, 
      include.rownames = FALSE,
      include.colnames = TRUE,
      booktabs = TRUE,
      caption.placement = "top",
      hline.after = c(-1, 0, 2, 4),
      add.to.row = list(pos = list(-1),
                        command = "\\multicolumn{1}{c}{} & \\multicolumn{1}{c}{} & \\multicolumn{2}{c}{Education} & \\multicolumn{1}{c}{} & \\multicolumn{2}{c}{Birth Year} \\\\\n"),
      sanitize.text.function = function(x) x)






