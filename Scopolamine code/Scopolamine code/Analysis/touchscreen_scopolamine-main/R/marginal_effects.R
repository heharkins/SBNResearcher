library(here)
library(tidyverse)
library(gamlss)
library(ggsci)
library(nlme)
library(patchwork)

theme_set(theme_classic(base_size = 14))
dodge <- position_dodge(width = 0.2)
jd <- position_jitterdodge(dodge.width = 0.7, jitter.width = 0.45, seed = 42)
days <- as_labeller(c("1" = "Day 1", "2" = "Day 2", "3" = "Day 3",
                      "4" = "Day 4", "5" = "Day 5"))
th <- theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

# load data and models ----------------------------------------------------
path <- here("data", "processed_data.rds")

df <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::select(participant, dose, test_day, choice_latency,
                response_side, age_c, sex, age, initiation_time_last,
                initiation_num_touch, response_correct, response_num_touch,
                choice_latency_prev, response_correct_prev, day, distance_to_center,
                trial) |>
  dplyr::mutate(choice_latency_c = scale(choice_latency) |> as.numeric(),
                initiation_time_last_c = scale(initiation_time_last) |> as.numeric(),
                choice_latency_prev_c = scale(choice_latency_prev) |> as.numeric(),
                distance_to_center_c = scale(distance_to_center) |> as.numeric(),
                alias = case_when(
                  participant == "Crumble" ~ "M7",
                  participant == "Harmonia" ~ "M5",
                  participant == "Icarus" ~ "M2",
                  participant == "Kore" ~ "M3",
                  participant == "Muffin" ~ "M1",
                  participant == "Nereus" ~ "M6",
                  participant == "Nyx" ~ "M8",
                  participant == "Quintas" ~ "M4",
                  TRUE ~ NA_character_)) |>
  na.omit()


m_success <- read_rds(here("data", "gamlss_success.rds"))

df_success <- df |> bind_cols(pred = predict(m_success, output = "matrix", what = "mu", type = "link",
                        se.fit = FALSE)) |>
  bind_cols(pred_se = predict(m_success, output = "matrix", what = "mu", type = "link",
                              se.fit = TRUE)$se.fit) |>
  dplyr::mutate(predic = plogis(pred),
              upper_ci = plogis(pred + 1.96 * pred_se),
              lower_ci = plogis(pred - 1.96 * pred_se))


df_sum_success <- df_success |>
  dplyr::group_by(dose, sex) |>
  dplyr::summarise(mean_pred = mean(predic),
                   mean_resp = mean(response_correct),
                   mean_upper_ci = mean(upper_ci),
                   mean_lower_ci = mean(lower_ci))

gg_accuracy <- ggplot(df_sum_success, aes(dose, mean_pred, colour = sex,
                                     ymin = mean_lower_ci,
                                     ymax = mean_upper_ci)) +
              geom_linerange(position = dodge, linewidth = 1) +
              geom_point(aes(y = mean_resp, group = sex), position = dodge,
                         size = 5, shape = 21, show.legend = FALSE) +
              geom_point(position = dodge, size = 3) +
  scale_color_jco() +
  labs(y = NULL, x = NULL) +
  guides(colour = "none") +
  theme(plot.background = element_rect(colour = "firebrick3", fill = "white", linewidth = 3))

gg_accuracy

# sex differences in accuracy by the training day --------------------------

df_day_success <- df_success |>
  dplyr::group_by(dose, sex, test_day) |>
  dplyr::summarise(mean_pred = mean(predic),
                   mean_resp = mean(response_correct),
                   mean_upper_ci = mean(upper_ci),
                   mean_lower_ci = mean(lower_ci))

gg_day_accuracy <- ggplot(df_day_success, aes(dose, mean_pred, colour = sex,
                                         ymin = mean_lower_ci,
                                         ymax = mean_upper_ci)) +
  geom_linerange(position = dodge, linewidth = 1) +
  geom_point(aes(y = mean_resp, group = sex), position = dodge,
             size = 5, shape = 21, show.legend = FALSE) +
  geom_point(position = dodge, size = 3) +
  facet_wrap(~test_day, labeller = days) +
  scale_color_jco() +
  labs(y = "Accuracy", x = "Dose")

gg_day_accuracy

gg_inset <- gg_day_accuracy + inset_element(gg_accuracy, left = 0.73, bottom = 0, right = 1.10,
                                top = 0.41, clip = FALSE, ignore_tag = FALSE, on_top = FALSE,
                                align_to = "plot")

ggsave(filename = here("graphs", "accuracy_model_preds.pdf"),
       plot = gg_inset,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "accuracy_model_preds.svg"),
       plot = gg_inset,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)

# sex differences in accuracy by the training day and subject ----------------

df_day_subject_success <- df_success |>
  dplyr::group_by(dose, sex, test_day, age_c, alias) |>
  dplyr::summarise(mean_pred = mean(predic),
                   mean_resp = mean(response_correct),
                   mean_upper_ci = mean(upper_ci),
                   mean_lower_ci = mean(lower_ci))


gg_day_sex_subject_accuracy <- ggplot(df_day_subject_success,
                                 aes(x = dose, y = mean_resp, group = age_c,
                                     colour = sex, shape = alias)) +
  geom_point(position = jd,
             size = 3) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "firebrick3") +
  scale_shape_manual(values = c(1:4, 15:18), name = "marmoset") +
  facet_wrap(~test_day, labeller = days) +
  scale_color_jco() +
  labs(y = "Accuracy", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_day_age_subject_accuracy <- ggplot(df_day_subject_success,
                                      aes(x = dose, y = mean_resp, group = age_c,
                                          colour = age_c, shape = alias)) +
  geom_point(position = jd,
             size = 3) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "firebrick3") +
  scale_shape_manual(values = c(1:4, 15:18), name = "marmoset") +
  facet_wrap(~test_day, labeller = days) +
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(4)[3:4], name = "age") +
  labs(y = NULL, x = "Dose") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_day_age_subject_accuracy

gg_combined <- gg_day_sex_subject_accuracy / gg_day_age_subject_accuracy +
  plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(filename = here("graphs", "accuracy_by_sex_age_subject.pdf"),
       plot = gg_combined,
       units = "in",
       height = 10,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "accuracy_by_sex_age_subject.svg"),
       plot = gg_combined,
       units = "in",
       height = 10,
       width = 8,
       dpi = 600)