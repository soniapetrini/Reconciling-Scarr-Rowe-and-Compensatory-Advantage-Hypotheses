setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")

#################################################
######## OPEN DATA ##############################
#################################################

# ------ MAIN DATASET

# harmonized dataset (provided by ELSA)
#elsa <- read_dta(paste0(data_dir,"ELSA/raw/h_elsa_g3.dta"))
elsa <- readRDS(paste0(data_dir,"ELSA/raw/elsa.rds")) %>%
  select(idauniq,
         years_edu  = raedyrs_e, 
         levels_edu = raeduc_e, 
         sex        = ragender, 
         birth_year = rabyear,
         raracem)

# ------ PGI data

pgi <- read_excel(paste0(data_dir,"ELSA/raw/genetic/elsa_pgirepo_v1.elsaid.xlsx")) %>%
  select(idauniq, 
         pgi_education = PGI_EA.single,
         all_of(PC_vars))


# ------ CHILDHOOD RECALL from third wave

# wave 3 information on life histories (provided by ELSA)
childhood <- read_dta(paste0(data_dir,"ELSA/raw/wave_3_life_history_data.dta")) %>%
  select(idauniq, 
         owners=raown, rooms=raroo, unemp=rsunemp, starts_with("rafac"), books=rabks)


# ------ MERGE ALL


# Merge
data <- merge(pgi, elsa, by="idauniq") %>%
  merge(childhood, by="idauniq")




#########################################################
######## CLEAN OUTCOME VARIABLES ########################
#########################################################

######## EDUCATION #######

# Years of education

table(data$years_edu) # original variable years of education

# Information on the process of assignation of years of education
# can be found in Table S1 of the Supplementary Materials

data <- data %>%
  mutate(education = case_when(
    years_edu <= 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
    years_edu == 1 ~ 9, #14, 
    years_edu == 2 ~ 10, #15, 
    years_edu == 3 ~ 11, #16, 
    years_edu == 4 ~ 12, #17, 
    years_edu == 5 ~ 13, #18, 
    years_edu == 6 ~ 14, #19, 
    TRUE ~ years_edu  
  )) 

table(data$education)



# Tertiary education (binary outcome)

table(data$levels_edu) # original variable 

data <- data %>%
  mutate(
    high_school = case_when(
      levels_edu <= 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
      levels_edu %in% c(1) ~ 0,
      levels_edu %in% c(3,4,5) ~ 1,
      TRUE ~ levels_edu  
    ),
    college = case_when(
      levels_edu <= 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
      levels_edu %in% c(1,3) ~ 0,
      levels_edu %in% c(4,5) ~ 1,
      TRUE ~ levels_edu  
    ),
    college = case_when(
      levels_edu <= 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
      levels_edu %in% c(1,3,4) ~ 0,
      levels_edu %in% c(5) ~ 1,
      TRUE ~ levels_edu  
    )
  )

table(data$college)

table(data$High_school)









##################################################
######## ASCRIBED CHARACTERISTICS ########################
##################################################

########## GENDER: male dummy #########

# Explore 
table(data$sex)

# Recode
data <- data %>% mutate(
  sex = case_when(sex == 1 ~ "male",
                  sex == 2 ~ "female"))

# Check
table(data$sex)



########## MIGRATION / ETHNICITY (note that only 1 respondent is non-white) ######### 

table(data$raracem) # coded by ELSA as 1 white, 4 non-white

# Filter only white
data <- data %>% filter(raracem==1)


########## SOCIOECONOMIC BACKGROUND #########

# We use Principal Components Analysis to create an index 
# that summarizes respondent's SES during childhood 
# (info from Wave 3 life history surveys):

data <- data %>%
         # 1. House ownership
         mutate(owners = case_when(owners < 0  ~ NA_real_,
                                   owners == 1 ~ 1,
                                   TRUE       ~ 0),
         # 2. Number of rooms
         rooms = if_else(rooms < 0, NA_real_, rooms),
         # 3. Facilities
         across(starts_with("rafac"), ~if_else(. <= -1, NA_real_, .)),
         facilities = rowSums(select(., starts_with("rafac")), na.rm = TRUE),
         # 4. Unemployed parents
         unemp = case_when(unemp < 0  ~ NA_real_,
                           unemp == 1 ~ 1,
                           TRUE         ~ 0),
         # 5. Number of books
         books = if_else(books < 0, NA_real_, books)
         )
  




###############################################################
############### SELECT FINAL SAMPLE  ##########################
###############################################################

SES_PCA_vars <- c("books", "unemp", "facilities", "rooms", "owners")


# Select variables
temp <- select(data, 
               id = idauniq, 
               birth_year, sex, 
               pgi_education,
               any_of(OUTCOMES),
               any_of(SES_PCA_vars),
               any_of(PC_vars))

# Check missings
sapply(temp, function(col) sum(is.na(col)))

# Complete information
temp <- na.omit(temp)  
# N = 3428






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



####  Constructed SES ###

# Binary SES
data <- merged %>%
  mutate(
    SES_cont = SES,
    SES      = if_else(SES > median(SES, na.rm = TRUE), "High SES", "Low SES")
  )

# Fix levels
table(data$SES)
data$SES <- factor(data$SES, levels = c("Low SES", "High SES"))

# Select final sample
data <- data %>% select(-any_of(SES_PCA_vars))




#####  Filter age ###
#
#data <- filter(data, birth_year >= 1933)



saveRDS(data, file="data/ELSA/df.rds")


