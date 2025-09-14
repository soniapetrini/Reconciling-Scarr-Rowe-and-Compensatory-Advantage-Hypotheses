setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")




# =========================================================
#                ✨ RGI DISTRIBUTION by SES✨
# =========================================================
source("code/funs.R")

library(scales)

predictor <- "pgi_education"

# Read data
ds <- "WLS"
df <- get_data(ds)

# Rescale PGI
df[predictor] = as.vector(scale(df[predictor]))

# Define custom colors
fill_colors <- c("Low SES" = "#fbaca7", "High SES" = "#52d8da")  # or choose your own
line_colors <- c("Low SES" = "#A83F38", "High SES" = "#0F7B7D")  # darker by 40%

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
  theme(
    legend.position = "bottom",
    text  = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  xlim(c(-4, 4)) +
  ylim(c(0, 0.42))

# Save
ggsave(paste0("plots/",ds,"_PGIdensity.png"), width = 6, height =6)




# =========================================================
#                    ✨ DESCRIPTIVES ✨   
# =========================================================

source("code/funs.R")

# Sample size
sapply(c("WLS","ELSA"), function(ds) nrow(get_data(ds)))

# Set
outcome <- "college"

# Select variables
data <- get_data(ds) %>% 
  select(SES, any_of(DEMO), college)

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
ds <- "ELSA"
get_data(ds) %>%
  group_by(SES) %>%
  summarise(n = n(),
            college = mean(college),
            share_female = mean(sex == "female", na.rm = TRUE),
            birth_year_m = mean(birth_year),
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








