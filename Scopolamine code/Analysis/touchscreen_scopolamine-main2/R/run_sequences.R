library(here)
library(tidyverse)
library(ggstatsplot)
library(afex)
library(emmeans)

path <- here("data", "processed_data.rds")

df_sequence <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::group_by(participant, dose) |>
  dplyr::summarise(average_run = as.character(response_side) |>
                  rle() |>
                  list() |>
                  map("lengths") |>
                  map_dbl(mean))

# lengths of left runs
df_sequence_left <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::group_by(participant, dose) |>
  dplyr::summarise(average_run = as.character(response_side) |>
                     rle() |>
                     map(as.vector) |>
                     as.data.frame() |>
                     dplyr::filter(values == "left") |>
                     dplyr::select("lengths") |>
                     map_dbl(mean)) |>
  tibble::add_column(side = "left")

# lengths of right runs
df_sequence_right <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::group_by(participant, dose) |>
  dplyr::summarise(average_run = as.character(response_side) |>
                     rle() |>
                     map(as.vector) |>
                     as.data.frame() |>
                     dplyr::filter(values == "right") |>
                     dplyr::select("lengths") |>
                     map_dbl(mean))  |>
  tibble::add_column(side = "right")

df_sequence_both <- bind_rows(df_sequence_left, df_sequence_right)

# repeated measures ANOVA
# left
df_s_Muffin_l <- df_sequence_left |>
  dplyr::filter(participant != "Muffin")

aov_left <- aov_ez("participant", "average_run", df_s_Muffin_l, within = c("dose"))
aov_left

# right
df_s_Muffin_r <- df_sequence_right |>
  dplyr::filter(participant != "Muffin")

aov_right <- aov_ez("participant", "average_run", df_s_Muffin_r, within = c("dose"))
aov_right

# both
df_s_Muffin_b <- df_sequence_both |>
  dplyr::filter(participant != "Muffin")

aov_side <- aov_ez("participant", "average_run", df_s_Muffin_b, within = c("dose", "side"))
aov_side

