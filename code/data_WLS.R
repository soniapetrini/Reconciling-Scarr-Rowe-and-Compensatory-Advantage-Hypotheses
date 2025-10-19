setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")



#####################################
# DATA

data <- readRDS(paste0(data_dir,"WLS/raw/data.rds"))

print("finished reading raw data.")


############################ IDENTIFIERS ########################

#--- first, an unique individual ID
data$ID<- paste(data$familypub, data$personid, sep = "_")
n_distinct(data$ID) #check how many unique obs

#--- second, a family ID (rename only)
data$familyID<-data$familypub
n_distinct(data$familypub) #check how many unique obs

#--- third, a within family identifier (rename only)
data$withinID<-data$personid

#---- fourth, an ID for PGIs merging
data$pgiID<-paste(data$idpub,data$rtype, sep = "_")


########################## DEMOGRAPHICS ##########################

#----- Basic ones
data <- data %>%
  mutate(
    race       = z_ie020re, # 1 white, 2 other (we don't need it, but leave it here just in case)
    sex        = z_sexrsp, # 1 male (0), 2 female (1)
    birth_year = z_brdxdy # note that birth month is protected characteristic in WLS, but we don't need it
    
  )

# remove non white
data <- data %>% filter(race == 1)

#--- Parental ages at the time of birth
data <- data %>%
  mutate(
    birth_year = ifelse(birth_year < 0, NA, birth_year),
    sex        = case_when(sex == 1 ~ 0, sex == 2 ~ 1, TRUE ~ NA_real_ )) 






########################## OUTCOMES ######################################

# Check missing cases in the educational variables
valid_summary <- data %>%
  summarise(
    valid_yoe_3 = sum(!is.na(z_edeqyr)), # R03 Equivalent years of regular education.
    valid_yoe_4 = sum(!is.na(z_rb003red)), # R04 Summary of equivalent yrs of regular education based on most recent degree.
    valid_yoe_5 = sum(!is.na(z_gb103red)), # R05 How many years of education does R have based on his or her Highest degree?
    valid_yoe_6 = sum(!is.na(z_hb103red)) #R06 Summary of equivalent years of regular education based on Highest degree.
  )
valid_summary


# Check values
attributes(data$z_hb103red)

EDU     <- c(education3  = "z_edeqyr",   education4  = "z_rb003red", education5 = "z_gb103red", education6 = "z_hb103red") # years of education
TALL    <- c(heigh   = "z_mx010rec")

# Rename
data <- data %>% rename(!!!EDU, !!!TALL)

# Clean (sending negative values to NA)
data <- data %>%
  mutate_at(vars(names(EDU), names(TALL)), ~ ifelse(. < 0, NA, .))

# Check
table(data$education4)

# Combine averaging to have more stable measures
data <- data %>%
  mutate(
    education       = rowMeans(select(., education5, education6), na.rm=TRUE),
    high_school     = if_else(education  >= 12, 1, 0),
    college         = if_else(education  >= 16, 1, 0),
    graduate_school = if_else(education  >= 17, 1, 0)
  )



########################## SES ######################################


# ------ Already available SES index

summary(data$ses57)
data <- data %>% mutate(SES = ses57)

# Impute siblings' SES
family_means <- data %>%
  group_by(familyID) %>%
  summarize(mean_SES = mean(SES, na.rm = TRUE))

# Join this back to the original dataframe
data <- data %>%
  left_join(family_means, by = "familyID") %>%
  mutate(SES = ifelse(is.na(SES), mean_SES, SES)) %>%
  select(-mean_SES)


summary(data$SES)







########################## OBSERVED ABILITY cognitive ##########################

# Rename
data <- data %>% rename(IQ = z_gwiiq_bm, centile_rank_IQ = z_ghncr_bm)

# Clean (sending negative values to NA)
data <- data %>%
  mutate_at(vars(IQ, centile_rank_IQ),
            ~ ifelse(. < 0, NA, .))  # Replace negative values with NA

# check distributions
summary(select(data, IQ, centile_rank_IQ))





########################## PGIs cognitive ######################################

# Read raw
#pgi_cog3    <- readRDS(paste0(data_dir,"WLS/raw/pgi_cog.rds"))

pgi_cog  <- read_dta(paste0(data_dir,"WLS/raw/PGIrepo_idpub_v1.1/PGIrepo_v1.1_idpub_shuffled.dta"))




# Relabel and select the variables of interest
pgi_cog <- pgi_cog %>%
  mutate(
    pgiID = paste(idpub,rtype, sep = "_")
  ) %>%
  select(
    pgiID,
    pgi_education = pgi_easingle,
    PC1  = pc1_PGI_shuffled,  PC11 = pc11_PGI_shuffled,
    PC2  = pc2_PGI_shuffled,  PC12 = pc12_PGI_shuffled,
    PC3  = pc3_PGI_shuffled,  PC13 = pc13_PGI_shuffled,
    PC4  = pc4_PGI_shuffled,  PC14 = pc14_PGI_shuffled,
    PC5  = pc5_PGI_shuffled,  PC15 = pc15_PGI_shuffled,
    PC6  = pc6_PGI_shuffled,  PC16 = pc16_PGI_shuffled,
    PC7  = pc7_PGI_shuffled,  PC17 = pc17_PGI_shuffled,
    PC8  = pc8_PGI_shuffled,  PC18 = pc18_PGI_shuffled,
    PC9  = pc9_PGI_shuffled,  PC19 = pc19_PGI_shuffled,
    PC10 = pc10_PGI_shuffled, PC20 = pc20_PGI_shuffled
  )




# Merge with main data

df <- merge(data, pgi_cog, by="pgiID", all.x=TRUE)



########################## SELECT FINAL VARIABLES  ##########################


# Select all the relevant variables to extract them from the sample
df <- df %>%
  select(ID, familyID, withinID, pgiID,   # IDs
         SES, 
         any_of(DEMO), 
         any_of(OUTCOMES),
         pgi_education, 
         any_of(PC_vars)
  )

# Label NAs rightly
df <- df %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))  # Applies to all columns

# There are a few cases that are not labelled properly and show 3 or 4 df, delete them
df <- df[!(df$withinID %in% c(3, 4)), ]

sapply(df, function(col) sum(is.na(col)))

# re-code IDs and sex to avoid scaling them
df <- df %>% 
  mutate(familyID = as.character(familyID),
         withinID = as.character(withinID),
         sex      = as.factor(as.character(sex)))

# remove NAs
df <- na.omit(df)

# check
n_distinct(df$ID)
n_distinct(df$pgiID)
n_distinct(df$familyID)

# create SES terciles 
full <- df %>%
  mutate(
    sex      = if_else(sex == 1, "male", "female"),
    SES_cont = SES
  )

full %<>% mutate(
  SES = ntile(SES_cont, 3)
)

full$SES <- factor(full$SES, levels = c(1,2,3), labels= SES.groups)


# save full data
saveRDS(full, file="data/WLS/df.rds")




## ----- SIBLINGS -----
#
## At least two kids (if siblings analysis)
#siblings <- df %>% 
#  group_by(familyID) %>%
#  filter(n() >= 2) %>%
#  ungroup()
#
#
## create SES binary 
#siblings %<>%
#  mutate(
#    sex      = if_else(sex == 1, "male", "female"),
#    SES_cont = SES,
#    SES      = if_else(SES >= median(SES, na.rm = TRUE), "High SES", "Low SES")
#  )
#
## --- save siblings data
#saveRDS(siblings, file="data/WLS/siblings.rds")


