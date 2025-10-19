setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")




# =========================================================
#                ✨ RGI DISTRIBUTION by SES✨
# =========================================================
source("code/funs.R")

library(scales)

predictor <- "pgi_education"


sapply(c("WLS","ELSA"), function(ds) {
  
  # Read data
  df <- get_data(ds)
  
  # Rescale PGI
  df[predictor] = as.vector(scale(df[predictor]))
  
  # Define custom colors
  fill_colors <- SES.colors
  line_colors <- c("Low SES"  = "#906803",
                   "High SES" = "#1E8081")  # darker by 40%
  
  # Compute medians
  medians <- df %>%
    group_by(SES) %>%
    summarise(med = median(pgi_education, na.rm = TRUE))
  
  # Plot
  df %>% 
    ggplot(aes(x = pgi_education, fill = SES)) +
    geom_density(alpha = 0.6) +
    geom_vline(data = medians, 
               aes(xintercept = med, color = SES),
               linetype = "dashed", linewidth = 1) +
    scale_fill_manual(name = "", values = fill_colors) +
    scale_color_manual(guide = "none", values = line_colors) +
    labs(x = "PGI") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text  = element_text(size = 20),
      legend.text = element_text(size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    ) +
    xlim(c(-4, 4)) +
    ylim(c(0, 0.42))
  
  # Save
  ggsave(paste0("plots/",ds,"_PGIdensity.pdf"), width = 6, height =6)

})



# =========================================================
#                    ✨ DESCRIPTIVES ✨   
# =========================================================

source("code/funs.R")

# Sample size
sapply(c("WLS","ELSA"), function(ds) nrow(get_data(ds)))


# Select variables
data <- get_data(ds) %>% 
  select(SES, any_of(DEMO), high_school, college)

head(data)

# Get summary statistics by SES group
tapply(data$birth_year, data$SES, summary) %>% bind_rows()

# Get summary statistics by dataset
all_df <- sapply(c("WLS","ELSA"), function(ds) {
  get_data(ds) %>% mutate(dataset=ds)
})
all_df <- bind_rows(all_df)
tapply(all_df$birth_year, all_df$dataset, summary) 


# Summary table
ds <- "WLS"
get_data(ds) %>%
  group_by(SES) %>%
  summarise(n = n(),
            high_school = mean(high_school),
            college = mean(college),
            share_female = mean(sex == "female", na.rm = TRUE),
            birth_year_m = as.integer(mean(birth_year)),
            birth_year_sd = sd(birth_year),
            ) %>% xtable()





# =========================================================
#                    ✨ BIRTH YEAR PLOT ✨ 
# =========================================================

ds <- "ELSA"
get_data(ds) %>%
  ggplot(aes(y = "", x = birth_year)) +
  geom_violin(fill = "lightblue") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x="birth year") +
  xlim(c(1905,1990)) 
ggsave(paste0("plots/",ds,"_birth_year.png"), width = 6, height =2)


ds <- "WLS"
get_data(ds) %>%
  ggplot(aes(y = "", x = birth_year)) +
  geom_violin(fill = "orange") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x="birth year") +
  xlim(c(1905,1990))
ggsave(paste0("plots/",ds,"_birth_year.png"), width = 6, height =2)







# =========================================================
#            ✨ COMPLETION BY SES AND OUTCOME ✨
# =========================================================


# Get rates for wls and elsa
rates_other <- lapply(c("WLS","ELSA"), function(ds) {
  
  # Get completion rates
  rates <- get_data(ds) %>%
    group_by(SES) %>%
    summarise(n = n(),
              high_school = mean(high_school),
              college = mean(college)) %>%
      mutate(dataset=factor(ds))
}) %>% bind_rows

# Add AH
rates_AH <- data.frame(dataset="Add Health",
           SES=c("Low SES", "High SES"),
           n=c(2009, 2032),
           high_school = c(0.943, 0.991),
           college = c(0.234, 0.605)
           )

# Combine
rates <- bind_rows(rates_other, rates_AH) %>% 
  mutate(dataset=factor(dataset, levels=DATASETS),
         SES=factor(SES, levels=c("Low SES", "High SES")))

# Melt
rates <- reshape2::melt(rates, id.vars=c("dataset","SES","n"), 
                        variable.name = "outcome")

ggplot(rates, aes(x=dataset, y=value, fill=SES)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(y="Completion rate",x="") +
  facet_wrap(~outcome, nrow=1, labeller=labeller(outcome=outcome.labs)) +
  theme_minimal() +
  scale_fill_manual(name="", values=SES.colors) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(2, "lines"),
    legend.text = element_text(size = 23, color = "#635856"),
    panel.grid.minor = element_blank(),
    text            = element_text(size=24, color = "#635856"),
    axis.title.y    = element_text(color = "#635856"),
    strip.text      = element_text(size=20,color = "#635856")
  )


ggsave(paste0("plots/completion_rates.pdf"), width = 10, height =7)







# Compute medians
df <- get_data(ds)

medians <- df %>%
  group_by(sex) %>%
  summarise(med = median(pgi_education, na.rm = TRUE))

# Plot
df %>% 
  ggplot(aes(x = pgi_education, fill = sex)) +
  geom_density(alpha = 0.6) +
  geom_vline(data = medians, 
             aes(xintercept = med, color = sex),
             linetype = "dashed", linewidth = 1) +
  #scale_fill_manual(name = "", values = fill_colors) +
  #scale_color_manual(guide = "none", values = line_colors) +
  labs(x = "PGI") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text  = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(c(-4, 4)) +
  ylim(c(0, 0.42))




