library(here)
library(tidyverse)
library(kableExtra)
library(geepack)

df <- read_rds(here("data", "processed_data.rds")) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::select(participant, dose, test_day, choice_latency,
                response_side, age_c, sex, age,  initiation_time_last,
                initiation_num_touch, response_correct, response_num_touch,
                choice_latency_prev, response_correct_prev, day, distance_to_center,
                trial)



m_accuracy <- geeglm(response_correct ~ dose + test_day, family = binomial, id = participant,
                     data = df)
summary(m_accuracy)