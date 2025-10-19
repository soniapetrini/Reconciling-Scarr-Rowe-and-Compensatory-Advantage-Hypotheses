# =========================================================
#                     ✨ MEDIAN✨
# =========================================================

# Median PGI (n_boot=10000, "median")
results_AH_median <- data.frame(
  group = c("High SES", "High SES", "High SES", "High SES",
            "Low SES", "Low SES", "Low SES", "Low SES",
            "High SES", "High SES", "High SES", "High SES",
            "Low SES", "Low SES", "Low SES", "Low SES"),
  high_OUT = c(0,0,1,1, 0,0,1,1, 0,0,1,1, 0,0,1,1),
  metric = c("FPR","TNR","FNR","TPR",
             "FPR","TNR","FNR","TPR",
             "FPR","TNR","FNR","TPR",
             "FPR","TNR","FNR","TPR"),
  value = c(0.08,0.92,0.50,0.50, 0.28,0.72,0.49,0.51,
            0.31,0.69,0.38,0.62, 0.44,0.56,0.30,0.70),
  ci_lower = c(0.00,0.75,0.49,0.50, 0.20,0.63,0.49,0.50,
               0.28,0.66,0.36,0.61, 0.42,0.55,0.26,0.66),
  ci_upper = c(0.25,1.00,0.51,0.50, 0.34,0.80,0.49,0.52,
               0.34,0.72,0.40,0.64, 0.48,0.58,0.34,0.74),
  p_value = c(0.0304,0.0304,0.0304,0.0304,
              0.0028,0.0028,0.0028,0.0028,
              0.0004,0.0004,0.0004,0.0004,
              0.0004,0.0004,0.0004,0.0004),
  stars = c("*","*","**","**",
            "*","*","**","***",
            "***","***","***","***",
            "***","***","***","***"),
  group_var = rep("SES", 16),
  outcome = c(rep("high_school", 8), rep("college", 8)),
  predictor = rep("pgi_education", 16),
  n = c(19,19,2013,2013,1894,1894,1894,1894,802,802,123,123,1539,1539,470,470)
)


results_AH_median <- results_AH_median %>% mutate(dataset="AH")





# =========================================================
#                     ✨ NO MIDDLE  ✨
# =========================================================

# No middle of PGI (n_boot=10000, "no middle")
results_AH_no_middle <- data.frame(
  group = c("High SES","High SES","High SES","High SES",
            "Low SES","Low SES","Low SES","Low SES",
            "High SES","High SES","High SES","High SES",
            "Low SES","Low SES","Low SES","Low SES"),
  high_OUT = c(0,0,1,1, 0,0,1,1, 0,0,1,1, 0,0,1,1),
  metric = c("FPR","TNR","FNR","TPR",
             "FPR","TNR","FNR","TPR",
             "FPR","TNR","FNR","TPR",
             "FPR","TNR","FNR","TPR"),
  value = c(0.06,0.94,0.50,0.50,
            0.25,0.75,0.48,0.52,
            0.27,0.73,0.35,0.65,
            0.42,0.58,0.24,0.76),
  ci_lower = c(0.00,0.80,0.49,0.50,
               0.16,0.66,0.48,0.51,
               0.24,0.70,0.33,0.63,
               0.40,0.57,0.20,0.72),
  ci_upper = c(0.20,1.00,0.50,0.51,
               0.34,0.84,0.49,0.54,
               0.30,0.76,0.37,0.67,
               0.43,0.59,0.28,0.80),
  p_value = c(0.0260,0.0260,0.0012,0.0012,
              0.0260,0.0260,0.0012,0.0012,
              0.0000,0.0000,0.0000,0.0000,
              0.0000,0.0000,0.0000,0.0000),
  stars = c("*","*","**","**",
            "*","*","**","**",
            "***","***","***","***",
            "***","***","***","***"),
  group_var = rep("SES", 16),
  outcome = c(rep("high_school", 8), rep("college", 8)),
  predictor = rep("pgi_education", 16),
  n = c(17,17,1609,1609,95,95,1513,1513,655,655,971,971,1229,1229,379,379)
)


results_AH_no_middle <- results_AH_no_middle %>% mutate(dataset="AH")



# =========================================================
#                     ✨ GENDER ✨
# =========================================================


# By gender (n_boot=10000, "median")
results_AH_gender_hs <- data.frame(
  group = c("High SES","High SES","High SES","High SES",
            "Low SES","Low SES","Low SES","Low SES",
            "High SES","High SES","High SES","High SES",
            "Low SES","Low SES","Low SES","Low SES"),
  high_OUT = c(0,0,1,1, 0,0,1,1, 0,0,1,1, 0,0,1,1),
  metric = c("TNR","FPR","TPR","FNR",
             "TNR","FPR","TPR","FNR",
             "TNR","FPR","TPR","FNR",
             "TNR","FPR","TPR","FNR"),
  value = c(0.98,0.02,0.50,0.50,
            0.66,0.34,0.51,0.49,
            0.90,0.10,0.50,0.50,
            0.76,0.24,0.52,0.48),
  ci_lower = c(0.78,0.00,0.50,0.49,
               0.52,0.22,0.51,0.49,
               0.67,0.00,0.50,0.49,
               0.64,0.13,0.51,0.47),
  ci_upper = c(1.00,0.22,0.51,0.50,
               0.80,0.48,0.52,0.50,
               1.00,0.33,0.51,0.50,
               0.87,0.33,0.53,0.49),
  p_value = c(0.0196,0.0196,0.2716,0.2716,
              0.0196,0.0196,0.2716,0.2716,
              0.0196,0.0196,0.0068,0.0068,
              0.0196,0.0196,0.0068,0.0068),
  stars = c("*","*","","",
            "*","*","","",
            "*","*","**","**",
            "*","*","**","**"),
  group_var = rep("SES", 16),
  outcome = rep("high_school", 16),
  predictor = rep("pgi_education", 16),
  n = c(8,8,1101,1101,52,52,1097,1097,11,11,912,912,63,63,797,797),
  dataset = rep("AH", 16),
  sex = c(rep("female", 8), rep("male", 8)),
  status = c(rep(c("No High School","High School"), each = 4),
             rep(c("No High School","High School"), each = 4))
)




results_AH_gender_college <- data.frame(
  group = c("High SES","High SES","High SES","High SES",
            "Low SES","Low SES","Low SES","Low SES",
            "High SES","High SES","High SES","High SES",
            "Low SES","Low SES","Low SES","Low SES"),
  high_OUT = c(0,0,1,1, 0,0,1,1, 0,0,1,1, 0,0,1,1),
  metric = c("TNR","FPR","TPR","FNR",
             "TNR","FPR","TPR","FNR",
             "TNR","FPR","TPR","FNR",
             "TNR","FPR","TPR","FNR"),
  value = c(0.69,0.31,0.60,0.40,
            0.58,0.42,0.70,0.30,
            0.69,0.31,0.65,0.35,
            0.55,0.45,0.72,0.28),
  ci_lower = c(0.65,0.27,0.58,0.37,
               0.56,0.40,0.65,0.25,
               0.65,0.27,0.62,0.31,
               0.53,0.43,0.65,0.22),
  ci_upper = c(0.73,0.35,0.63,0.42,
               0.60,0.44,0.74,0.35,
               0.73,0.35,0.69,0.38,
               0.57,0.47,0.78,0.35),
  p_value = c(0.0000,0.0000,0.0000,0.0000,
              0.0000,0.0000,0.0000,0.0000,
              0.0000,0.0000,0.1106,0.1106,
              0.0000,0.0000,0.1106,0.1106),
  stars = c("***","***","***","***",
            "***","***","***","***",
            "***","***","","",
            "***","***","",""),
  group_var = rep("SES", 16),
  outcome = rep("college", 16),
  predictor = rep("pgi_education", 16),
  n = c(378,378,731,731,834,834,315,315,
        424,424,499,499,705,705,155,155),
  dataset = rep("AH", 16),
  sex = c(rep("female", 8), rep("male", 8)),
  status = c(rep(c("No College","College"), each = 4),
             rep(c("No College","College"), each = 4))
)


results_AH_gender <- bind_rows(results_AH_gender_college, results_AH_gender_hs)%>%
  mutate(dataset="AH")


# =========================================================
#                     ✨ ALL THRESHOLDS ✨
# =========================================================


# For all thresholds (n_boot=10000, "median")
results_AH_thresholds_hs <- data.frame(
  group = c("High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES"),
  high_OUT = c(0,1,0,1,
               0,1,0,1,
               0,1,0,1,
               0,1,0,1,
               0,1,0,1),
  metric = rep(c("FPR","FNR","FPR","FNR"), 5),
  value = c(0.48,0.20,0.64,0.19,
            0.18,0.40,0.37,0.39,
            0.08,0.50,0.28,0.49,
            0.05,0.60,0.21,0.59,
            0.01,0.80,0.05,0.79),
  ci_lower = c(0.24,0.19,0.55,0.18,
               0.00,0.39,0.29,0.38,
               0.00,0.49,0.20,0.48,
               0.00,0.58,0.13,0.58,
               0.00,0.80,0.02,0.79),
  ci_upper = c(0.73,0.20,0.73,0.20,
               0.40,0.40,0.46,0.39,
               0.25,0.50,0.37,0.49,
               0.18,0.61,0.29,0.59,
               0.12,0.80,0.10,0.79),
  p_value = c(0.2304,0.0368,0.2304,0.0368,
              0.0938,0.0028,0.0938,0.0028,
              0.0304,0.0028,0.0304,0.0028,
              0.0356,0.0016,0.0356,0.0016,
              0.2130,0.0000,0.2130,0.0000),
  stars = c("","*","","*",
            ".","**",".","**",
            "*","**","*","**",
            "*","**","*","**",
            "","***","","***"),
  group_var = rep("SES", 20),
  outcome = rep("high_school", 20),
  predictor = rep("pgi_education", 20),
  n = c(19,2013,115,1894,
        19,2013,115,1894,
        19,2013,115,1894,
        19,2013,115,1894,
        19,2013,115,1894),
  dataset = rep("AH", 20),
  threshold = rep(c(0.2,0.4,0.5,0.6,0.8), each = 4)
)





results_AH_thresholds_college <- data.frame(
  group = c("High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES",
            "High SES","High SES","Low SES","Low SES"),
  high_OUT = c(0,1,0,1,
               0,1,0,1,
               0,1,0,1,
               0,1,0,1,
               0,1,0,1),
  metric = rep(c("FPR","FNR","FPR","FNR"), 5),
  value = c(0.66,0.11,0.76,0.07,
            0.41,0.27,0.54,0.20,
            0.31,0.38,0.44,0.30,
            0.22,0.48,0.33,0.38,
            0.08,0.72,0.15,0.64),
  ci_lower = c(0.64,0.10,0.75,0.05,
               0.38,0.26,0.52,0.16,
               0.28,0.36,0.42,0.26,
               0.20,0.47,0.32,0.34,
               0.06,0.71,0.14,0.60),
  ci_upper = c(0.69,0.13,0.77,0.09,
               0.43,0.29,0.55,0.23,
               0.34,0.39,0.45,0.34,
               0.25,0.50,0.35,0.42,
               0.10,0.73,0.16,0.67),
  p_value = c(0,5e-03,0,5e-03,
              0,0,0,0,
              0,4e-04,0,0,
              0,0,0,0,
              0,0,0,0),
  stars = c("***","**","***","**",
            "***","***","***","***",
            "***","***","***","***",
            "***","***","***","***",
            "***","***","***","***"),
  group_var = rep("SES", 20),
  outcome = rep("college", 20),
  predictor = rep("pgi_education", 20),
  n = c(802,1230,1539,470,
        802,1230,1539,470,
        802,1230,1539,470,
        802,1230,1539,470,
        802,1230,1539,470),
  dataset = rep("AH", 20),
  threshold = rep(c(0.2,0.4,0.5,0.6,0.8), each = 4)
)




results_AH_thresholds <- bind_rows(results_AH_thresholds_hs, results_AH_thresholds_college)





# =========================================================
#                     ✨ SES TERCILES ✨
# =========================================================



results_AH_terciles <- data.frame(
  metric = c(
    "FPR","FPR","FPR","FNR","FNR","FNR",
    "TPR","TPR","TPR","TNR","TNR","TNR",
    "FPR","FPR","FPR","FNR","FNR","FNR",
    "TPR","TPR","TPR","TNR","TNR","TNR"
  ),
  group = c(
    "SES Q1","SES Q2","SES Q3","SES Q1","SES Q2","SES Q3",
    "SES Q1","SES Q2","SES Q3","SES Q1","SES Q2","SES Q3",
    "SES Q1","SES Q2","SES Q3","SES Q1","SES Q2","SES Q3",
    "SES Q1","SES Q2","SES Q3","SES Q1","SES Q2","SES Q3"
  ),
  value = c(
    0.30,0.20,0.01,0.49,0.49,0.50,
    0.51,0.51,0.50,0.70,0.80,0.99,
    0.45,0.38,0.30,0.29,0.32,0.41,
    0.71,0.68,0.59,0.55,0.62,0.70
  ),
  ci_lower = c(
    0.22,0.04,0.01,0.48,0.49,0.49,
    0.51,0.50,0.50,0.60,0.61,0.80,
    0.44,0.36,0.26,0.23,0.29,0.39,
    0.66,0.64,0.57,0.54,0.60,0.67
  ),
  ci_upper = c(
    0.40,0.39,0.02,0.49,0.49,0.51,
    0.52,0.51,0.51,0.78,0.96,1.00,
    0.46,0.40,0.33,0.34,0.36,0.43,
    0.77,0.71,0.61,0.56,0.64,0.74
  ),
  p_value = c(
    0.274,NA,0.056,0.048,NA,0.250,
    0.048,NA,0.250,0.274,NA,0.056,
    0.000,NA,0.000,0.290,NA,0.000,
    0.290,NA,0.000,0.000,NA,0.000
  ),
  stars = c(
    NA,NA,".","* ",NA,NA,
    "* ",NA,NA,NA,NA,".",
    "***",NA,"***",NA,NA,"***",
    NA,NA,"***","***",NA,"***"
  ),
  group_var = rep("SES", 24),
  outcome = c(
    rep("high_school", 12),
    rep("college", 12)
  ),
  predictor = rep("pgi_education", 24),
  high_OUT = c(
    0,0,0,1,1,1,
    1,1,1,0,0,0,
    0,0,0,1,1,1,
    1,1,1,0,0,0
  ),
  n = c(
    100,25,9,1247,1322,1338,
    1247,1322,1338,100,25,9,
    1099,813,429,248,534,918,
    248,534,918,1099,813,429
  ),
  dataset = "AH"
)
















