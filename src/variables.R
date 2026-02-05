
selected_variables_egfr <- c(
  'age' = 'age',
  'sex' = 'sex',
  'creatinine' = 'creatinine',
  'egfr_ckdepi' = 'egfr'
)

bias_percent_egfr <- round(c(
  'age' = NA,
  'sex' = NA,
  'creatinine' = 0.9, 
  'egfr' = NA
), 2)

uncertain_variables_indicator_egfr <- c(
  FALSE,
  FALSE,
  TRUE,
  FALSE
)

df_bias_percent_egfr <- data.frame(
  lab = c("sittard", "heerlen"),
  age = c(NA, NA),
  sex = c(NA, NA),
  creatinine = c(0.9, 1.1),
  egfr = c(NA, NA)
)


df_bias_abs_egfr <- data.frame(
  lab = c("sittard", "heerlen"),
  age = c(NA, NA),
  sex = c(NA, NA),
  creatinine = c(2, 3),
  egfr = c(NA, NA)
)