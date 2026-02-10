library(here)
library(tidyverse)
library(readxl)  # for reading excel files
library(writexl)


# regex patterns

ptn_dose <- "(?<=_)([^_]+)(?=_)"
ptn_last_number <- "([-+]?[0-9]*\\.[0-9]+)"

# Define the folder path
folder_path <- here("raw_data", "combined_uncleaned_data")  # Replace with your actual folder path

# Get all excel files in the folder
excel_files <- list.files(path = folder_path, pattern = "^[^~]*\\.xlsx$",
                          full.names = TRUE)

named_excel_files <- purrr::set_names(excel_files, nm = basename(excel_files))

df_ <- named_excel_files |>
  map_dfr(read_excel, .id = "file") |>
  dplyr::filter(!is.na(status))

# columns to drop
drop_cols <- c("trials.thisTrialN", "trials.thisN", "trials.thisIndex", "trialsReward.thisRepN",
               "trialsReward.thisTrialN", "trialsReward.thisN", "trialsReward.thisIndex",
               "trialsNoReward.thisRepN", "trialsNoReward.thisTrialN", "trialsNoReward.thisN",
               "text.started", "mouse_2.leftButton", "mouse_2.midButton", "mouse_2.rightButton",
               "background.started", "initiate.started", "mouse_2.clicked_name", "polygon.started",
               "mouse.started",  "pos.started", "neg.started", "psychopyVersion", "frameRate",
               "reward_screen.started", "sound_2.started", "sound_2.stopped", "black.started",
               "sound_1.started", "sound_1.stopped", "serial_cmd", "Experimenter", "expName",
               "trialsNoReward.thisIndex", "mouse_2.started", "pos+ pos", "neg- pos", "mouse.leftButton",
               "mouse.midButton", "mouse.rightButton", "status", "mouse.clicked_name", "serial_cmd_t",
               "black.stopped", "trials.thisRepN", "mouse_2.x", "mouse_2.y", "mouse_2.time", "mouse.x",
               "mouse.y", "mouse.time")

df <- df_ |>
  dplyr::mutate(dose = str_extract(file, ptn_dose) |> factor(), .after = file) |>
  dplyr::mutate(test_day = parse_number(file), .after = dose) |>
  dplyr::mutate(response_x_last = stringi::stri_match_last_regex(mouse.x, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse.x) |>
  dplyr::mutate(response_y_last = stringi::stri_match_last_regex(mouse.y, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse.y) |>
  dplyr::mutate(initiation_x_last = stringi::stri_match_last_regex(mouse_2.x, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse_2.x) |>
  dplyr::mutate(initiation_y_last = stringi::stri_match_last_regex(mouse_2.y, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse_2.y) |>
  dplyr::mutate(response_time_last = stringi::stri_match_last_regex(mouse.time, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse.time) |>
  dplyr::mutate(response_time_first = stringi::stri_match_first_regex(mouse.time, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse.time) |>
  dplyr::mutate(initiation_time_last = stringi::stri_match_last_regex(mouse_2.time, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse_2.time) |>
  dplyr::mutate(initiation_time_first = stringi::stri_match_first_regex(mouse_2.time, ptn_last_number)[, 1] |>
                  as.numeric(),
                .after = mouse_2.time) |>
  dplyr::mutate(response_num_touch = str_count(mouse.x, ptn_last_number), .after = response_x_last) |>
  dplyr::mutate(initiation_num_touch = str_count(mouse_2.x, ptn_last_number), .after = initiation_x_last) |>
  dplyr::mutate(correct_pos_x = stringi::stri_match_first_regex(`pos+ pos`, ptn_last_number)[, 1] |>
                  as.numeric()) |>
  dplyr::mutate(incorrect_pos_x = stringi::stri_match_first_regex(`neg- pos`, ptn_last_number)[, 1] |>
                  as.numeric()) |>
  dplyr::mutate(response_correct = -1*status+2) |>
  dplyr::mutate(date = lubridate::ymd_hms(date) |> lubridate::floor_date("day")) |>
  dplyr::mutate(trial = (trials.thisRepN + 1)) |>
  dplyr::mutate(response_side = ifelse(response_x_last < 0, "left", "right") |> factor()) |>
  # extract touch locations in X and Y direction outside of the response window
  dplyr::mutate(response_x_nontarget = str_extract_all(mouse.x, ptn_last_number) |> map(as.numeric) |>
                  map(head, n = -1)) |>
  dplyr::mutate(response_y_nontarget = str_extract_all(mouse.y, ptn_last_number) |> map(as.numeric) |>
                  map(head, n = -1)) |>
  # extract touch latencies to decision task
  dplyr::mutate(response_times = str_extract_all(mouse.time, ptn_last_number) |> map(as.numeric)) |>
  # distance between last touch and stimulus center
  dplyr::mutate(distance_to_center = if_else(response_correct == 1,
                                             sqrt((response_x_last - correct_pos_x)^2 + response_y_last^2),
                                             sqrt((response_x_last - incorrect_pos_x)^2) + response_y_last^2)) |>
  dplyr::mutate(participant = str_to_title(participant) |> factor()) |>
  dplyr::select(-one_of(drop_cols))


write_rds(df, here("data", "combined_data.rds"))
# write_xlsx(df, here("data", "combined_data.xlsx"))
