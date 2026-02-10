library(here)
library(tidyverse)
library(kableExtra)

df <- read_rds(here("data", "processed_data.rds")) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::select(participant, dose, test_day, choice_latency,
                response_side, age_c, sex, initiation_time_last,
                initiation_num_touch, response_correct, response_num_touch)

table(df$dose, df$response_side, df$participant) |>
  prop.table(margin = c(1,3)) |>
  round(2)


# tabulated choice latencies ----------------------------------------------

with(df |> dplyr::filter(response_correct == 1),
  tapply(choice_latency, list(dose = dose, participant = participant), median, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of median choice latencies for correct trials") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

with(df |> dplyr::filter(response_correct == 0),
     tapply(choice_latency, list(dose = dose, participant = participant), median, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of median choice latencies for incorrect trials") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

# tabulated initiation times ----------------------------------------------
with(df |> dplyr::filter(response_correct == 1),
     tapply(initiation_time_last, list(dose = dose, participant = participant), median, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of median trial initiation latencies for correct trials") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

with(df |> dplyr::filter(response_correct == 0),
     tapply(initiation_time_last, list(dose = dose, participant = participant), median, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of median trial initiation latencies for incorrect trials") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

# tabulated accuracy ------------------------------------------------------
with(df,
     tapply(response_correct, list(dose = dose, participant = participant), mean, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of performance accuracy") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

# tabulated number of touches un the choice task --------------------------
with(df |> dplyr::filter(response_correct == 1),
     tapply(response_num_touch, list(dose = dose, participant = participant), mean, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of the mean number of touches in the choice task for correct trials") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

with(df |> dplyr::filter(response_correct == 0),
     tapply(response_num_touch, list(dose = dose, participant = participant), mean, na.rm = TRUE)) |>
  round(2) |>
  as.data.frame() |> kbl(caption = "Table of the mean number of touches in the choice task for incorrect trials") |>
  kable_styling(full_width = FALSE, html_font = "Cambria")

# accuracy by day  --------------------------------------------------------
df_accuracy_summary <- df |>
  group_by(participant, dose, test_day) |>
  summarise(accuracy = mean(response_correct, na.rm = TRUE))

df_accuracy_max <- df_accuracy_summary |>
  group_by(participant, dose) |>
  summarise(max_accuracy_day = test_day[which.max(accuracy)],
            max_accuracy = max(accuracy))


