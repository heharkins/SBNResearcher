library(here)
library(tidyverse)
library(summarytools)

df <- read_rds(here("data", "combined_data.rds"))

df |>
  summarytools::descr() |>
  summarytools::view()

# Muffin_vehicle_5 has one row where choice_latency was 2000 seconds. She also did 30 touches before making the correct choice.

df |>
  dplyr::filter(choice_latency > 1999) |>
  View()

df |>
  dplyr::filter(response_num_touch > 20) |>
  View()

df |>
  dplyr::filter(is.na(participant)) |>
  View()