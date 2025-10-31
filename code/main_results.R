rm(list=ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")



# =========================================================
#                     ✨ MAIN RESULTS  ✨
# =========================================================



fun_pred  <- "no middle"
n_boot    <- 10000
predictor <- "pgi_education"
outcomes  <- c("college")
outcomes  <- c("high_school","college")
datasets  <- c("ELSA","WLS")


results <- lapply(outcomes, function(Outcome) {
  
  # Read WLS and ELSA results
  results_other <- lapply(datasets, function(ds) {
    results <- readRDS(paste0("results/",ds,"_",Outcome,"_",fun_pred,"_",n_boot,".rsd"))
    results %>% mutate(dataset = ds) 
      
  })
  
  # Add Add Health manually 
  results_AH <- if (fun_pred == 0.5) {
                  results_AH_median %>% filter(outcome == Outcome)
                } else if (fun_pred == "no middle") {
                  results_AH_no_middle %>% filter(outcome == Outcome)
                } else {break}
  
  
  # Combine all datasets
  results <- bind_rows(results_other, results_AH)
  
  # Convert to 100%
  results <- results %>% 
    mutate(across(c(value, ci_lower, ci_upper), ~ .x*100))
  
  # Set labels
  results <- results %>% 
    mutate(status  = ifelse(high_OUT==1, outcome.labs[Outcome], neg.outcome.labs[Outcome]),
           group   = factor(group, levels=c("Low SES", "High SES")),
           dataset = factor(dataset, levels=c("WLS", "ELSA","AH")))
  
  # Return
  results
  
}) %>% bind_rows()



# Keep p-values and adjust
pvalues <- results %>% 
  filter(metric %in% c("FNR", "FPR")) %>%
  filter(group=="High SES") %>%
  select(dataset, outcome, status, metric, p_value, stars) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues




# =========================================================
#                     ✨ MAIN PLOT ✨  
# =========================================================


# Assumes that p-values have been adjusted in previous code:

lapply(outcomes, function(Outcome) {
  
  lapply(c("WLS","ELSA", "AH"), function(ds) {
    
    # Correct stars
    pvalues_ds <- pvalues %>%
      filter(dataset == ds, outcome == Outcome) %>% 
      select(dataset, outcome, status, stars.adj)
    res        <- merge(results, pvalues_ds, by=c("dataset","outcome","status"))
    
    # Plot
    plot_perc(res)
    
    # Save
    ggsave(paste0("plots/",ds,"_",Outcome,"_",fun_pred,".pdf"), width = 7, height =7)
  
  })
})




# =========================================================
#                     ✨ MAIN TABLE ✨  
# =========================================================


# Only Falses
results_table <- results %>% filter(metric %in% c("FNR","FPR"))

# Compute diff
diffs <- results_table %>% 
  select(outcome, dataset, group, metric, value) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(value = `Low SES` - `High SES`,
         group  = "diff",
         metric = metric) %>%
  select(outcome, dataset, group, metric, value) %>%
  pivot_wider(names_from = metric, values_from = c(value))

# Reshape main
results_table <- results_table %>% 
  select(outcome, dataset, metric, group, value, ci_lower, ci_upper, n) %>%
  pivot_wider(names_from = metric, values_from = c(value, ci_lower, ci_upper, n)) %>%
  rename(FPR=value_FPR, FNR=value_FNR)

# Combine
results_table <- bind_rows(results_table, diffs) 

# Reorder rows
results_table <- results_table %>% 
  mutate(group=factor(group, levels=c("Low SES","High SES","diff")),
         outcome = factor(outcome, levels=c("high_school","college"))) %>%
  arrange(outcome, dataset, group)

# All characters
results_table <- results_table %>%
  mutate(across(everything(), ~as.character(.x)))

results_table[is.na(results_table)] <- ""


# Print in Latex
latex_table <- results_table %>%
  mutate(
    row_text = glue(
      "& {group} & {`n_FNR`}& ${`FNR`}$  $[{`ci_lower_FNR`},{`ci_upper_FNR`}]$  & {n_FPR} & ${`FPR`}$ $[{ci_lower_FPR}, {ci_upper_FPR}]$ & \\\\"
    )) %>% pull(row_text)
latex_table












# =========================================================
#                        ✨ GENDER ✨
# =========================================================

source("code/funs.R")


n_boot    <- 10000
fun_pred  <- 0.5
predictor <- "pgi_education"
outcomes  <- c("high_school","college")
datasets  <- c("WLS","ELSA","AH")


results <- lapply(outcomes, function(Outcome) {
  
  res <- lapply(datasets, function(ds) {
    
    ######### Get results for dataset  ######### 
    
    if (ds == "AH") {
      results <- results_AH_gender %>% filter(outcome==Outcome) %>%
        mutate(status = ifelse(high_OUT==1, outcome.labs[Outcome], neg.outcome.labs[Outcome]))
    } else {
      
    # Read by gender
    results_sex <- lapply(c("female","male"), function(which_sex) {
      # Read and set labels
      readRDS(paste0("results/gender/",ds,"_",Outcome,"_",fun_pred,"_",n_boot,"_",which_sex,".rsd")) %>% 
        mutate(dataset = ds, sex = which_sex) %>% 
        mutate(status  = ifelse(high_OUT==1, outcome.labs[Outcome], neg.outcome.labs[Outcome]),
               group   = factor(group,   levels = c("Low SES", "High SES")))
    })
    # Both sexes
    results <- bind_rows(results_sex) 
    }
    
    # Return
    results
    
  }) 
  res %>% bind_rows %>% mutate(
    dataset = factor(dataset, levels = c("WLS",     "ELSA", "AH")),
    sex     = factor(sex,     levels = c("female",  "male"))
    )
  
}) %>% bind_rows
  
######### Correct p-values ######### 

pvalues <- results %>%
  filter(metric %in% c("FPR", "FNR")) %>% 
  filter(group=="High SES") %>%
  select(dataset, outcome, sex, status, metric, p_value, stars) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues





######### Plot ######### 

p <- lapply(outcomes, function(Outcome) {
  
  p <- lapply(datasets, function(ds) {
    
    # Convert to 100%
    results_ds <- results %>% mutate(across(c(value, ci_lower, ci_upper), ~ .x*100))
    
    # Correct stars
    pvalues_ds <- pvalues %>%
      filter(dataset == ds, outcome == Outcome) %>% 
      select(dataset, outcome, status, sex, stars.adj)
    results_ds <- merge(results_ds, pvalues_ds, by=c("dataset","outcome","status","sex"))
    
    # Plot
    plot_perc(results_ds) + facet_grid(sex~real, scales="free_x")
    
    # Save
    ggsave(paste0("plots/",ds,"_",Outcome,"_gender.pdf"), width = 8, height =10)

  })
  
})




























# =========================================================
#                 ✨ DIFFERENT THRESHOLDS ✨
# =========================================================

source("code/funs.R")

predictor <- "pgi_education"
n_boot    <- 10000


results <- lapply(OUTCOMES, function(Outcome) {
    
  lapply(c("WLS","ELSA","AH"), function(ds) {
    
    
    ######### Get results for dataset  ######### 
    
    if (ds == "AH") {
      results <- results_AH_thresholds %>% filter(outcome==Outcome)
    } else {
      results_thr <- lapply(c(0.2,0.4,0.5,0.6,0.8), function(fun_pred) {
        readRDS(paste0("results/",ds,"_",Outcome,"_",fun_pred,"_",n_boot,".rsd")) %>% 
          mutate(dataset = ds, threshold=fun_pred) %>%
          filter(metric %in% c("FNR", "FPR"))
      })
      # Combine
      results <- bind_rows(results_thr)
    }
  
    # Set labels
    results <- results %>% 
      mutate(status  = ifelse(high_OUT==1, "Completed", "Not completed"),
             group   = factor(group, levels=c("Low SES", "High SES")),
             dataset = factor(dataset, levels=c("WLS", "ELSA","AH")))
    
    # Return
    results
    
  }) %>% bind_rows


}) %>% bind_rows



######### Correct p-values ######### 

pvalues <- results %>%
  filter(group=="High SES") %>%
  select(dataset, outcome, status, metric, threshold, p_value, stars) %>% 
  mutate(p_value.adj = p.adjust(p_value, method = "holm"),
         stars.adj   = add_stars(p_value.adj))
pvalues
    
    
    
    ######### Plot ######### 

p <- lapply(OUTCOMES, function(Outcome) {
  
  p <- lapply(c("WLS","ELSA","AH"), function(ds) {
    
    # Correct stars
    pvalues_ds <- pvalues %>%
      filter(dataset == ds, outcome == Outcome) %>% 
      select(dataset, outcome, status, threshold, stars.adj)
    results_ds <- merge(results, pvalues_ds, by=c("dataset","outcome","status","threshold"))
    
    # Star label
    results_ds <- results_ds %>% 
      mutate(label=case_when(high_OUT==1 & group=="High SES" ~ stars.adj,
                             high_OUT==0 & group=="Low SES" ~ stars.adj,
                             TRUE ~ ""))
    
    # Plot
    ggplot(results_ds, aes(x=threshold, y=value, color=group)) +
      geom_line() + geom_point() +
      facet_wrap(~status, ncol=2) +
      geom_vline(aes(xintercept = 0.5), 
                 linetype="dashed", color="red") +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group),
                  alpha = 0.2, color = NA) +
      theme_minimal() +
      # Add stars
      geom_text(aes(label = label),
                color = "#635856", 
                size  = 10,
                vjust=-0.5) +
      # Set theme
      theme(legend.position = "bottom",
            legend.key.size = unit(2, "lines"),
            panel.grid.minor = element_blank(),
            legend.text  = element_text(size = 24),
            text         = element_text(size=24, color = "#635856"),
            axis.title.y = element_text(color = "#635856"),
            strip.text   = element_text(color = "#635856")) +
      # Fix axis display
      scale_y_continuous(limits = c(0, 1), 
                         name   = "p (overachieve)\n",
                         sec.axis = sec_axis(~ ., name = "p (underachieve) \n")
      ) +
      scale_color_discrete(name="") +
      scale_fill_manual(name="", values=SES.colors) +
      scale_x_continuous(breaks = unique(results_ds$threshold))
    
    # Save
    ggsave(paste0("plots/",ds,"_",Outcome,"_all_thresholds.pdf"), width = 12, height =6)
  
  })
  
  
})




