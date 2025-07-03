setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")



#####################################
# DATA

data       <- readRDS(paste0(data_dir,"WLS/raw/data.rds"))
pgi_cog    <- readRDS(paste0(data_dir,"WLS/raw/pgi_cog.rds"))

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
missing_summary <- data %>%
  summarise(
    valid_yoe_1 = sum(!is.na(z_edeqyr)), # R03 Equivalent years of regular education.
    valid_yoe_2 = sum(!is.na(z_rb004red)), # R04 Summary of equivalent yrs of regular education based on most recent degree.
    valid_yoe_3 = sum(!is.na(z_gb103red)), # R05 How many years of education does R have based on his or her Highest degree?
    valid_yoe_4 = sum(!is.na(z_mx001rer)) #R06 Summary of equivalent years of regular education based on Highest degree.
  )


# High School
attributes(data$z_hb103red)




EDU     <- c(education_1  = "z_edeqyr",   education_2  = "z_rb004red", education_3  = "z_gb103red", education_4 = "z_hb103red") # years of education
INC_IND <- c(income_ind_5 = "z_gp250rec", income_ind_6 = "z_hpu50rec") # individual level income (total personal income)
TALL    <- c(heigh   = "z_mx010rec")
COLLEGE <- c(college = "z_hb001re")



# Rename
data <- data %>% rename(!!!EDU, !!!COLLEGE, !!!TALL, !!!INC_IND)


# Clean (sending negative values to NA)

# -- other (all negative)
data <- data %>%
  mutate_at(vars(names(EDU), names(COLLEGE), names(TALL), names(INC_IND)), ~ ifelse(. < 0, NA, .))


# Combine averaging to have more stable measures
data <- data %>%
  mutate(
    education       = rowMeans(select(., all_of(names(EDU))), na.rm=TRUE),
    High_school     = if_else(education >= 12, 1, 0),
    college         = if_else(education >= 15, 1, 0),
    graduate_school = if_else(education >= 17, 1, 0),
    income          = rowMeans(select(., all_of(names(INC_IND))), na.rm=TRUE)
  )



########################## SES ######################################


# ----- Build from paternal occupation and education
#data <- data %>%
#  mutate(
#    father_edu  = if_else(edfa57q  < 0, NA, edfa57q), 
#    father_occu = if_else(bmfoc1u  < 0, NA, bmfoc1u)
#  )
#
## Scale SES variables
#data <- data %>%
#  mutate(across(all_of(SES_vars), ~scale(.) %>% as.vector))
#
## Calculate mean of standardized variables
#data <- data %>% mutate(SES = rowMeans(select(data, any_of(SES_vars)), na.rm = T))
#
## Check
#summary(select(data,any_of(SES_vars), SES))



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

# Relabel and select the variables of interest
pgi_cog <- pgi_cog %>%
  mutate(
    pgiID = paste(idpub,rtype, sep = "_")
  )%>%
  select(
    pgiID,
    pgi_education = pgs_ea3_gwas,
    pc1  = pc1_shuffled,
    pc2  = pc2_shuffled,
    pc3  = pc3_shuffled,
    pc4  = pc4_shuffled,
    pc5  = pc5_shuffled,
    pc6  = pc6_shuffled,
    pc7  = pc7_shuffled,
    pc8  = pc8_shuffled,
    pc9  = pc9_shuffled,
    pc10 = pc10_shuffled
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

# create SES binary 
full <- df %>%
  mutate(
    sex      = if_else(sex == 1, "male", "female"),
    SES_cont = SES,
    SES      = if_else(SES >= median(SES, na.rm = TRUE), "High SES", "Low SES")
    #SES       = ntile(SES, 3)
  )

full$SES <- factor(full$SES, levels = c("Low SES", "High SES"))


# save full data
saveRDS(full, file="data/WLS/df.rds")




# ----- SIBLINGS -----

# At least two kids (if siblings analysis)
siblings <- df %>% 
  group_by(familyID) %>%
  filter(n() >= 2) %>%
  ungroup()


# create SES binary 
siblings %<>%
  mutate(
    sex      = if_else(sex == 1, "male", "female"),
    SES_cont = SES,
    SES      = if_else(SES >= median(SES, na.rm = TRUE), "High SES", "Low SES")
  )

# --- save siblings data
saveRDS(siblings, file="data/WLS/siblings.rds")


