
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")




# DISTRIBUTIONS ##################################################################

ds <- "WLS"
df       <- get_data(ds)
data     <- dichotomize(df, resid=F)
data$SES <- factor(data$SES, levels = c("low SES", "high SES"))


# education distribution
tapply(df$education, df$SES, summary, na.rm = TRUE)
ggplot(df, aes(x=education,fill=SES)) + 
  geom_density(alpha=0.5)


# education by group
group <- "SES"
df %>% dichotomize() %>%
  ggplot(aes(x = get(group), y = education)) +
  geom_jitter(aes(color = get(group)), width = 0.2, alpha = 0.5) +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = after_stat(y), ymin = after_stat(y)), 
               width = 0.75, linetype = "dashed", linewidth = 1) +
  labs(title = paste("Distribution of Years of Education by SES and PGI"),
    x = group, y = "Years of Education", color = group) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~factor(high_PRED, levels=c(0,1), labels=c("low PGI","high PGI")),)


# principal components
plot(density(data$pc4))




# HETEROSKEDASTICTY ##################################################################
library(olsrr)
library(lmtest)
library(ggfortify)

ds <- "WLS"
df   <- get_data(ds)
data <- dichotomize(df, resid=T)
data$SES <- factor(data$SES, levels = c("low SES", "high SES"))

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



# SIMPLE OLS REGRESSIONS ##################################################################

ds <- "WLS"
df       <- readRDS(paste0("data/",ds,"/df.rds"))
data     <- dichotomize(df, resid=T)
data$SES <- factor(data$SES, levels = c("low SES", "high SES"))

# run regression
formula <- as.formula("education ~ pgi_education + SES*pgi_education")
formula <- as.formula("education ~ pgi_education + sex + birth_year + SES*pgi_education")
model_ols <- lm(formula, data = data)
summary(model_ols)
model_ols$coefficients





# ALL REGRESSIONS ##################################################################

source("code/funs.R")

outcome   <- "cognitive"
predictor <- "pgi_cognitive"
fun_out   <- "median"
fun_pred  <- "no middle"

datasets <- DATASETS
datasets <- c("WLS","ELSA")


plots <- lapply(datasets, function(ds) {
  
  # - 1 - Prepare
  df   <- get_data(ds)
  data <- dichotomize(df,outcome, predictor, fun_out, fun_pred, resid=T)
  data$SES <- factor(data$SES, levels = c("low SES", "high SES"))
  
  
  # - 2 - Run models
  
  # LOGIT: binary outcome
  formula = paste0("high_OUT ~ ",predictor,"*SES") %>% as.formula()
  model_logit <- glm(formula, data = data, family = binomial(link = "logit"))
  summary(model_logit)
  

  # OLS : continuous outcome
  formula = paste0(outcome," ~ ",predictor,"*SES") %>% as.formula()
  model_ols <- lm(formula, data = data)
  summary(model_ols)
  
  
  # - 3 - extract coefficients
  
  annots <- lapply(c("logistic","OLS"), function(model_name) {
    
    model <- switch(model_name, "OLS" = model_ols, "logistic"=model_logit)
    coefs <- coef(summary(model))[paste0(predictor,":SEShigh SES"), ]
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
      label = paste(coef_label, coef, pvalue_label),
      y_pos = ifelse(model_name=="logistic",0.85, 16)
    )
  })
  
  annotations <- do.call(rbind, annots) %>% 
    mutate(model = factor(model, levels=c("logistic","OLS"), labels=c("Logistic", "OLS"))) 
  
  
  # - 4 - Plot
  # Logit
  plot_logit <- 
    plot_model(model_logit, type = "pred", terms = c(paste(predictor,"[all]"), "SES")) +
    geom_text(data = filter(annotations, model=="Logistic"), 
              aes(label = label, y=y_pos),
              x = min(data[[predictor]]) + 0.2, 
              hjust = 0, 
              size=6,
              inherit.aes = FALSE) +
    labs(title="", x = "PGI", y = outcome.labs[outcome]) +
    theme_minimal() +
    theme(text=element_text(size=20)) +
    guides(color = "none") +
    scale_color_manual(values=groups.labs)  +
    ylim(c(0,1))
  
  # OLS
  plot_OLS <- 
    plot_model(model_ols, type = "pred", terms = c(paste(predictor,"[all]"), "SES")) +
    geom_text(data = filter(annotations, model=="OLS"), 
              aes(label = label, y=y_pos),
              x = min(data[[predictor]]) + 0.2, 
              hjust = 0, 
              size=6,
              inherit.aes = FALSE) +
    labs(title="", x = "PGI", y = outcome.labs[outcome]) +
    theme_minimal() +
    theme(text=element_text(size=20)) +
    scale_color_manual(values=groups.labs, name="") 
  
  plot <- ggarrange(plot_logit, plot_OLS, 
                    ncol=1, common.legend = T, 
                    legend = "bottom")
  ggsave(paste0("plots/regs_",ds,"_",outcome,".jpg"), width = 5, height = 10)
  
  plot
  
})


ggarrange(plotlist = plots, ncol=3,
          common.legend = T, 
          legend = "bottom")

ggsave(paste0("plots/regs.jpg"), width = 12, height = 10)





