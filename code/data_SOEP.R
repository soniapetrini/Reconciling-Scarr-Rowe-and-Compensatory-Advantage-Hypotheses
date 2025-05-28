setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Reconciling-Scarr-Rowe-and-Compensatory-Advantage-Hypotheses")
source("code/funs.R")


############################
# ----- DATA SOURCES -----
############################

# -- BIOGRAPHIC

# mob, sex, birth_year
bio <- read_dta(paste0(data_dir,"SOEP/raw/is/ppfad.dta")) %>% 
  select(pid, mob = gebmonat, birth_year = gebjahr, sex = sex, origins = germborn) %>% 
  mutate(across(everything(), negative_to_na)) %>%
  mutate(male     = case_when(sex     == 1 ~ 1, sex     == 2 ~ 0, TRUE ~ NA),
         origins  = case_when(origins == 1 ~ 1, origins == 2 ~ 0, TRUE ~ NA))


# -- OUTCOMES
# all income, isei, yoe measurements
outcomes <- read_dta(paste0(data_dir,"SOEP/raw/is/pgen.dta")) %>%
  select(pid, education = pgbilzt, occupation = pgisei88, income = pglabgro, syear) %>% 
  mutate(across(c(education, occupation, income), negative_to_na))



# -- SES
# occupation and education
paren_info <- read_dta(paste0(data_dir,"SOEP/raw/is/bioparen.dta")) %>%
  select(pid, cid,
         father_edu=vsbil,      mother_edu = msbil, 
         father_occu = visei88, mother_occu = misei88, 
         f_birth_year =  vgebj, m_birth_year = mgebj) %>%
  mutate(across(c(father_edu, mother_edu, father_occu, mother_occu, f_birth_year, m_birth_year), negative_to_na)) %>%
  select(pid, cid, any_of(SES_vars))

## income
#parent_income <- read_dta(paste0(data_dir,"SOEP/raw/is/h.dta")) %>%
#  select(cid, hh_income = hlc0002) %>% 
#  mutate(hh_income = negative_to_na(hh_income))


# create SES variable
paren_info <- paren_info %>%
  mutate(across(any_of(SES_vars), 
                ~scale(.) %>% as.vector))

summary(select(paren_info,any_of(SES_vars)))

# Calculate mean of standardized variables
paren_info %<>% mutate(SES = rowMeans(select(paren_info, any_of(SES_vars)), na.rm = T))



# -- POLYGENIC INDICES

pgi <- read.csv(paste0(data_dir,'SOEP/raw/gene/GSOEP_PGI_v1_4.csv')) %>% 
  select(pid, 
         pgi_education=pgi_easingle, 
         pgi_cognitive=pgi_cpsingle,
         any_of(PC_vars))


# -- SIBLINGS
sibs <- read_dta(paste0(data_dir,"SOEP/raw/is/ppfad.dta")) 





# - Cognitive test
cog <- read_dta(paste0(data_dir,"SOEP/raw/is/cognit.dta")) %>%
  select(pid, f099r30) %>% group_by(pid) %>% reframe(cognitive = max(f099r30))  %>% 
  mutate(across(cognitive, negative_to_na))





####################
# ---- MERGE  ----
###################

# merge with bio info
temp <- merge(pgi, bio,   by='pid', all.y = T) %>%
  merge(outcomes,   by='pid', all.x = T) %>%
  merge(paren_info, by='pid', all.x = T) %>%
  merge(cog, by='pid', all.x = T)


# create binary variables
temp %<>%
  mutate(
    sex = if_else(sex == 1, "male", "female"),
    SES = if_else(SES >= median(SES, na.rm = TRUE), "high SES", "low SES")
  )



##################################
# --- BUILD AGGREGATE MEASURES ----
##################################

# age at first EA measurement
temp <- temp %>% 
  group_by(pid) %>% 
  mutate(age = min(syear) - birth_year)


# select sample
temp <- select(temp, 
               id=pid, 
               SES, 
               pgi_education, 
               pgi_cognitive,
               cognitive,
               #mob, 
               education, 
               any_of(DEMO),
               any_of(PC_vars)
               )

if ("education" %in% colnames(temp)) {
  # aggregate outcomes
  temp <- temp %>%
    group_by(id) %>% 
    mutate(education = max(education, na.rm = T)) %>%
    mutate(across(everything(), ~ ifelse(. == -Inf, NA, .)))
}

if ("cognitive" %in% colnames(temp)) {
  # aggregate outcomes
  temp <- temp %>%
    group_by(id) %>% 
    mutate(cognitive = max(cognitive, na.rm = T)) %>%
    mutate(across(everything(), ~ ifelse(. == -Inf, NA, .)))
}

# keep unique observations
temp <- temp %>% distinct(id, .keep_all = TRUE)


# remove na values
temp <- na.omit(temp)

# print n
print(paste0("complete info sample: ",nrow(temp)))


temp <- as.data.frame(unclass(temp))
saveRDS(temp, file="data/SOEP/df.rds")
