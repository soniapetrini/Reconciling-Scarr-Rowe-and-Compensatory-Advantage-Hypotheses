setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")

#################################################
######## OPEN DATA ##############################
#################################################

######### MAIN DATASET #################

# harmonized dataset (provided by ELSA)
#elsa <- read_dta(paste0(data_dir,"ELSA/raw/h_elsa_g3.dta"))
elsa <- readRDS(paste0(data_dir,"ELSA/elsa.rds"))

#######  PGI data ##################

# polygenic index data (provided by ELSA)
pgi <- read_dta(paste0(data_dir,"ELSA/raw/list_pgs_scores_elsa_2022.dta"))
 
# Merge

merged<-merge(elsa, pgi, by="idauniq")

###### PRINCIPAL COMOPONENTS data ##################

# principal components data associated to PGIs (provided by ELSA)
pcs <- read_dta(paste0(data_dir, "ELSA/raw/principal_components_elsa_2022.dta"))

# Merge

merged<-merge(merged, pcs, by="idauniq")

####### CHILDHOOD RECALL from third wave ##################

# wave 3 information on life histories (provided by ELSA)
childhood <- read_dta(paste0(data_dir,"ELSA/raw/wave_3_life_history_data.dta"))

# Merge

merged<-merge(merged, childhood, by="idauniq", all.x=TRUE)

# Rename

data<-merged





#########################################################
######## CLEAN OUTCOME VARIABLES ########################
#########################################################

######## EDUCATION #######

# Years of education

table(data$raedyrs_e) # original variable years of education

# Information on the process of assignation of years of education
# can be found in Table S1 of the Supplementary Materials

data <- data %>%
  mutate(education = case_when(
    raedyrs_e <= 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
    raedyrs_e == 1 ~ 9,
    raedyrs_e == 2 ~ 10,
    raedyrs_e == 3 ~ 11,
    raedyrs_e == 4 ~ 12,
    raedyrs_e == 5 ~ 13,
    raedyrs_e == 6 ~ 14,
    TRUE ~ raedyrs_e  
  )) 

table(data$education)



# Tertiary education (binary outcome)

table(data$raeduc_e) # original variable 

data <- data %>%
  mutate(college = case_when(
    raeduc_e <= 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
    raeduc_e %in% c(1,3) ~ 0,
    raeduc_e %in% c(4,5) ~ 1,
    TRUE ~ raeduc_e  
  ))

table(data$college)




##################################################
######## PGI  ########################
##################################################

# PGI Educational Attainment

# Explore the variable
summary(data$EA_3)

# Standardize PGI for an easier interpretation
data <- data %>%
  mutate(pgi_education = scale(EA_3),
         pgi_cognitive = scale(GC_2015))



##################################################
######## COGNITIVE SKILLS  ########################
##################################################


# Explore the variable
summary(data$r1imrc) # immediate word recall
summary(data$r1verbf) # verbal fluency score

# Rename
data <- data %>%
  rename(cognitive = r1verbf)








##################################################
######## ASCRIBED CHARACTERISTICS ########################
##################################################

########## GENDER: male dummy #########

# Explore 
table(data$ragender)

# Recode
data <- data %>% mutate(
  sex = case_when(ragender == 1 ~ "male",
                  ragender == 2 ~ "female"))

# Check
table(data$sex)



########## MIGRATION / ETHNICITY (note that only 4 respondents are non-white) ######### 

table(data$raracem) # coded by ELSA as 1 white, 4 non-white

########## SOCIOECONOMIC BACKGROUND #########

# We use Principal Components Analysis to create an index 
# that summarizes respondent's SES during childhood 
# (info from Wave 3 life history surveys):

data <- data %>%
         # 1. House ownership
  mutate(owners = case_when(raown < 0  ~ NA_real_,
                            raown == 1 ~ 1,
                            TRUE       ~ 0),
         # 2. Number of rooms
         rooms = if_else(raroo < 0, NA_real_, raroo),
         # 3. Facilities
         across(starts_with("rafac"), ~if_else(. <= -1, NA_real_, .)),
         facilities = rowSums(select(., starts_with("rafac")), na.rm = TRUE),
         # 4. Unemployed parents
         unemp = case_when(rsunemp < 0  ~ NA_real_,
                           rsunemp == 1 ~ 1,
                           TRUE         ~ 0),
         # 5. Number of books
         books = if_else(rabks < 0, NA_real_, rabks)
         )
  




###############################################################
############### SELECT FINAL SAMPLE  ##########################
###############################################################

SES_PCA_vars <- c("books", "unemp", "facilities", "rooms", "owners")

OUTCOMES <- c("education","college","cognitive")

# Select variables
temp <- select(data, 
               id=idauniq, 
               birth_year = rabyear, sex, 
               pgi_education,
               pgi_cognitive,
               any_of(OUTCOMES),
               any_of(SES_PCA_vars),
               any_of(PC_vars))

# Complete information
temp <- na.omit(temp)  
# N = 3748






####  Construct Principal Component ###

#Select variables
myvars <- c("id", SES_PCA_vars)

#Extract the variables
pcdata<-temp[myvars]


# Perform PCA
pca_result <- prcomp(select(pcdata, -id), scale. = TRUE)
pca_result$sdev^2/sum(pca_result$sdev^2)

# Extract principal component scores
pc_scores <- pca_result$x

# Add principal components to the data
data_with_pcs <- cbind(pcdata, pc_scores)

# Extract first component
myvars<-c("id", "PC1")
pcs<-data_with_pcs[myvars]

#Rename first component to SES
pcs<-pcs %>% 
  rename(SES = PC1)

# Center SES around 0
pcs$SES<- pcs$SES - mean(pcs$SES)
summary(pcs$SES)

# Merge with data
merged <- merge(temp, pcs, by="id", all.x=TRUE)




# Binary SES
data <- merged %>%
  mutate(
    SES_cont = SES,
    SES      = if_else(SES >= median(SES, na.rm = TRUE), "high SES", "low SES")
  )
table(data$SES)

# select final sample
data <- data %>% select(-any_of(SES_PCA_vars))

saveRDS(data, file="data/ELSA/df.rds")


