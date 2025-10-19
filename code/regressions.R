setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


# =========================================================
#                  ✨ REGRESSION ANALYSIS ✨
# =========================================================

source("code/funs.R")



# Settings
predictor <- "pgi_education"
outcomes  <- c("high_school","college")
datasets  <- c("WLS","ELSA")



# For each outcome
for (outcome in outcomes)  {

  #################  RUN REGRESSIONS  ################# 
  
  # === Datasets
  models_all <- lapply(datasets, function(ds) {
    
    # === Model specification
    models_type <- lapply(c("LPM","Logistic"), function(which_model) {
      
      # === Keller adjustment
      models_keller <- lapply(c(F,T), function(keller) {
      
        # === Run regression
        RunRegression(ds, which_model, outcome, predictor, keller)
    
      })
      c("NoKeller"=models_keller[1], "Keller"=models_keller[2])
      
    })
    c("LPM" = models_type[[1]], "Logistic" = models_type[[2]])
  
  })
  
  
  # Combine all models for an outcome
  models <- c("WLS"= models_all[[1]], "ELSA" = models_all[[2]])
  
  
  # Select only variables of interest
  coef_map <- c("pgi_education" = "PGI", "SESHigh SES" = "SES",
                "pgi_education:SESHigh SES" = "PGI×SES")
  
  
  # Export to LaTeX table
  modelsummary(
    models,
    stars = TRUE,
    coef_map = coef_map,
    exponentiate = c(F,F,T,T,F,F,T,T),
    #output = paste0("tables/regressions_",outcome,".txt"),
    output="latex",
    fmt = 2,
    gof_omit = ".*"
  ) %>% print()
  
  
  
  ############ PLOTS ############ 
  
  p <- lapply(datasets, function(ds) {
    
    # Select dataset
    models_ds <- models[grepl(ds, names(models))]
    
    # Keep NoKeller
    models_ds <- models_ds[grepl("NoKeller", names(models_ds))]
    
    
    # Plot and combine
    plot <- ggarrange(PlotRegression(models_ds, "LPM"), 
                      PlotRegression(models_ds, "Logistic"), 
                      ncol=1, common.legend = T, 
                      legend = "bottom")
    
    # Show
    plot
    
    # Save
    ggsave(paste0("plots/",ds,"_regs_",outcome,".pdf"), width = 5, height = 10)
    
  })

  
  
}








