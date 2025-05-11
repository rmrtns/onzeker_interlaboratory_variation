set.seed(seed = 200)

library("dplyr")

calculate_egfr <- function(data){
  egfr_men <- expression(141 * (pmin((data[["creatinine"]] / 88.4) / 0.9, 1) ** -0.411) * (pmax((data[["creatinine"]] / 88.4) / 0.9, 1) ** -1.209) * (0.993 ** data[["age"]]))
  egfr_women <- expression(141 * (pmin((data[["creatinine"]] / 88.4) / 0.7, 1) ** -0.329) * (pmax((data[["creatinine"]] / 88.4) / 0.7, 1) ** -1.209) * (0.993 ** data[["age"]]) * 1.018)
  if_else(data[['sex']] == "1",
          true = eval(egfr_men),
          false = eval(egfr_women),
          missing = NA)
}


test_data <- data.frame(
  id = seq(1, 100, 1),
  age = rnorm(100, 60, 10),
  sex = sample(c(0, 1), 100, replace = TRUE),
  creatinine = rnorm(100, 80, 15)
)

test_data <- test_data %>%
  mutate(
    egfr = calculate_egfr(.),
    egfr_60 = case_when(
      egfr < 60 ~ 1,
      TRUE ~ 0
    ),
    mgfr_60 = case_when(
      egfr_60 == 1 ~ sample(c(0, 1), 100, replace = TRUE, prob = c(0.20, 0.80)),
      egfr_60 == 0 ~ sample(c(0, 1), 100, replace = TRUE, prob = c(0.90, 0.10)) 
    )
  )

write.csv(test_data, "data/test_data.csv", row.names = FALSE)
