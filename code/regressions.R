setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


# =========================================================
#                  ✨ RALL REGRESSIONS✨
# =========================================================

source("code/funs.R")



# Settings
outcome   <- "college"
predictor <- "pgi_education"
keller <- F
datasets <- c("WLS","ELSA")


# Loop for all datasets
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
                    paste(controls, collapse = " + "))
  if (keller) formula <- paste0(formula, " + ",
                                paste(pred_interactions, collapse = " + "), " + ",
                                paste(ses_interactions, collapse = " + ")
  )
  formula = as.formula(formula)
  
  
  
  # - 2 - Run models
  
  # LPM
  model_ols <- lm(formula, data = data)
  summary(model_ols)
  
  # LOGIT
  model_logit <- glm(formula, data = data, family = binomial(link = "logit"))
  summary(model_logit)
  

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





















