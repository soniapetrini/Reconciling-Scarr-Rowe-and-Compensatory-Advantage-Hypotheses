setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")


# DESCRIPTIVES ######################################################
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
