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

panel_bg <- "#EBF1F7"
axis_col <- "#3B3B3B"
base_size <- 11L

th <- theme_classic(base_size = 14) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # facet strip (Day labels)
            strip.background  = element_blank(),
            strip.text.x      = element_text(size = base_size + 2, colour = "black"),

            # axes
            axis.title.x      = element_blank(),
            axis.title.y      = element_text(colour = "black"),
            axis.text.x       = element_text(colour = axis_col),
            axis.text.y       = element_text(colour = axis_col),
            axis.line         = element_line(colour = axis_col, linewidth = 0.6),
            axis.ticks        = element_line(colour = axis_col, linewidth = 0.6),
            axis.ticks.length = grid::unit(2.5, "pt"),

            # legend
            legend.box.background = element_rect(colour = "black"),
            legend.background = element_rect(fill = "transparent"),
            legend.key = element_rect(fill = "transparent")
            )

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
  na.omit() |>
  dplyr::mutate(dose = fct_recode(dose, !!!c(Veh = "vehicle", Low = "")))


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

df_for_plot <- df |>
  dplyr::group_by(dose, sex, test_day, age_c, alias) |>
  summarise(mean_resp = mean(response_correct))


gg_day_sex_subject_accuracy <- ggplot(df_day_subject_success,
                                 aes(x = dose, y = mean_resp, group = sex,
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


# Graphs with added daily mean values -------------------------------------

# Daily means by SEX (within each test_day x dose x sex)
df_day_sex_mean <- df_for_plot %>%
  group_by(test_day, dose, sex) %>%
  summarise(mean_resp = mean(mean_resp, na.rm = TRUE), .groups = "drop")

# Daily means by AGE (within each test_day x dose x age_c)
df_day_age_mean <- df_for_plot %>%
  group_by(test_day, dose, age_c) %>%
  summarise(mean_resp = mean(mean_resp, na.rm = TRUE), .groups = "drop")

gg_day_sex_subject_accuracy <- ggplot(
  df_for_plot,
  aes(x = dose, y = mean_resp, group = sex, colour = sex, shape = alias)
) +
  geom_point(position = jd, size = 2) +
  geom_point(
    data = df_day_sex_mean,
    aes(x = dose, y = mean_resp, group = sex, colour = sex),
    inherit.aes = FALSE,
    shape = 95,
    size = 10,
    alpha = 0.5,
    show.legend = FALSE
  ) +
#  geom_hline(yintercept = 0.75, linetype = "dashed", color = "firebrick3") +
  scale_shape_manual(values = c(1:4, 15:18), name = "ID") +
  facet_wrap(~test_day, labeller = days, nrow = 1) +
  scale_color_jco(name = "Sex") +
  labs(y = "Accuracy", x = "Dose") +
  th +
  theme(panel.background = element_rect(fill = panel_bg, colour = NA),
        panel.spacing.x = grid::unit(0.6, "lines"),
        aspect.ratio = 1,
        plot.margin = unit(c(0.02, 0.1, 0.02, 0.1), "in"),
        legend.position="none")

gg_day_sex_subject_accuracy

# --- AGE plot + horizontal bars for daily age-group means ---
gg_day_age_subject_accuracy <- ggplot(
  df_for_plot,
  aes(x = dose, y = mean_resp, group = age_c, colour = age_c, shape = alias)
) +
  geom_point(position = jd, size = 2) +
  geom_point(
    data = df_day_age_mean,
    aes(x = dose, y = mean_resp, group = age_c, colour = age_c),
    inherit.aes = FALSE,
    shape = 95,
    size = 10,
    alpha = 0.5,
    show.legend = FALSE
  ) +
#  geom_hline(yintercept = 0.75, linetype = "dashed", color = "firebrick3") +
  scale_shape_manual(values = c(1:4, 15:18), name = "ID") +
  facet_wrap(~test_day, labeller = days, nrow = 1) +
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(4)[3:4], name = "Age") +
  labs(y = "Accuracy", x = "Dose") +
  th +
  theme(panel.background = element_rect(fill = panel_bg, colour = NA),
        panel.spacing.x = grid::unit(0.6, "lines"),
        aspect.ratio = 1,
        plot.margin = unit(c(0.02, 0.1, 0.02, 0.1), "in"),
        legend.position="none")

gg_day_age_subject_accuracy

gg_combined_mean <- gg_day_sex_subject_accuracy / gg_day_age_subject_accuracy +
  plot_annotation()

gg_combined_mean

ggsave(filename = here("graphs", "accuracy_by_sex_age_subject_with_means.pdf"),
       plot = gg_combined_mean,
       units = "in",
       height = 6,
       width = 11,
       dpi = 600)

ggsave(filename = here("graphs", "accuracy_by_sex_age_subject_with_means.svg"),
       plot = gg_combined_mean,
       units = "in",
       height = 6,
       width = 11,
       dpi = 600)

# Means only --------------------------------------------------------------
gg_day_sex_subject_accuracy_means <- ggplot() +
  geom_point(
    data = df_day_sex_mean,
    aes(x = dose, y = mean_resp, group = sex, colour = sex),
    inherit.aes = FALSE,
    shape = 95,
    size = 8,
    alpha = 1
  ) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "firebrick3") +
  scale_color_jco(name = "Sex") +
  coord_cartesian(y = c(0.2, 1)) +
  facet_wrap(~test_day, labeller = days, nrow = 1) +
  labs(y = "Accuracy", x = "Dose") +
  th +
  theme(panel.background = element_rect(fill = panel_bg, colour = NA),
        panel.spacing.x = grid::unit(0.6, "lines"),
        aspect.ratio = 1,
        plot.margin = unit(c(0.1, 0.2, 0.1, 0.2), "in"))

gg_day_sex_subject_accuracy_means

gg_day_age_subject_accuracy_means <- ggplot() +
  geom_point(
    data = df_day_age_mean,
    aes(x = dose, y = mean_resp, group = age_c, colour = age_c),
    inherit.aes = FALSE,
    shape = 95,
    size = 8,
    alpha = 1
  ) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "firebrick3") +
  coord_cartesian(y = c(0.2, 1)) +
  facet_wrap(~test_day, labeller = days, nrow = 1) +
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(4)[3:4], name = "Age") +
  labs(y = "Accuracy", x = "Dose") +
  th +
  theme(panel.background = element_rect(fill = panel_bg, colour = NA),
        panel.spacing.x = grid::unit(0.6, "lines"),
        aspect.ratio = 1,
        plot.margin = unit(c(0.1, 0.2, 0.1, 0.2), "in"))

gg_day_age_subject_accuracy_means

gg_combined_means_only <- gg_day_sex_subject_accuracy_means / gg_day_age_subject_accuracy_means

gg_combined_mean_only

ggsave(filename = here("graphs", "accuracy_by_sex_age_subject_means_only.pdf"),
       plot = gg_combined_means_only,
       units = "in",
       height = 6,
       width = 11,
       dpi = 600)

ggsave(filename = here("graphs", "accuracy_by_sex_age_subject_means_only.svg"),
       plot = gg_combined_means_only,
       units = "in",
       height = 6,
       width = 11,
       dpi = 600)

# Overall means -----------------------------------------------------------

df_sex_mean <- df_for_plot %>%
  group_by(sex, dose, alias) %>%
  summarise(mean_resp = mean(mean_resp))

df_age_mean <- df_for_plot %>%
  group_by(age_c, dose, alias) %>%
  summarise(mean_resp = mean(mean_resp))

df_sex_overall_mean <-  df_for_plot %>%
  group_by(sex, dose) %>%
  summarise(mean_resp = mean(mean_resp))

df_age_overall_mean <-  df_for_plot %>%
  group_by(age_c, dose) %>%
  summarise(mean_resp = mean(mean_resp))


gg_sex_subject_accuracy <- ggplot(
df_sex_mean,
  aes(x = dose, y = mean_resp, colour = sex, shape = alias)
) +
  geom_point(position = jd, size = 2) +
  geom_point(
    data = df_sex_overall_mean,
    aes(x = dose, y = mean_resp, group = sex, colour = sex),
    inherit.aes = FALSE,
    shape = 95,
    size = 10,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_shape_manual(values = c(1:4, 15:18), name = "ID") +
  scale_color_jco(name = "Sex") +
  labs(y = NULL, x = "Dose") +
  th +
  theme(panel.background = element_rect(fill = panel_bg, colour = NA),
        panel.spacing.x = grid::unit(0.6, "lines"),
        aspect.ratio = 1,
        plot.margin = unit(c(0.02, 0.1, 0.02, 0.1), "in"),
        legend.position="none")

gg_sex_subject_accuracy

gg_age_subject_accuracy <- ggplot(
  df_age_mean,
  aes(x = dose, y = mean_resp, colour = age_c, shape = alias)
) +
  geom_point(position = jd, size = 2) +
  geom_point(
    data = df_age_overall_mean,
    aes(x = dose, y = mean_resp, group = age_c, colour = age_c),
    inherit.aes = FALSE,
    shape = 95,
    size = 10,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_shape_manual(values = c(1:4, 15:18), name = "ID") +
  scale_color_manual(values = pal_npg("nrc", alpha = 1)(4)[3:4], name = "Age") +
  labs(y = NULL, x = "Dose") +
  th +
  theme(panel.background = element_rect(fill = panel_bg, colour = NA),
        panel.spacing.x = grid::unit(0.6, "lines"),
        aspect.ratio = 1,
        plot.margin = unit(c(0.02, 0.1, 0.02, 0.1), "in"),
        legend.position="none")

gg_age_subject_accuracy

gg_combined_age_sex <- (gg_sex_subject_accuracy / gg_age_subject_accuracy)


gg_combined_age_sex

ggsave(filename = here("graphs", "accuracy_by_sex_age_averaged_over_days.pdf"),
       plot = gg_combined_age_sex,
       units = "in",
       height = 8,
       width = 10,
       dpi = 600)

ggsave(filename = here("graphs", "accuracy_by_sex_age_averaged_over_days.svg"),
       plot = gg_combined_age_sex,
       units = "in",
       height = 8,
       width = 10,
       dpi = 600)


# by day and overall plots together ---------------------------------------

gg_combined <- ((gg_day_sex_subject_accuracy + gg_sex_subject_accuracy) +
                  plot_layout(widths = c(5, 1))) /
  ((gg_day_age_subject_accuracy + gg_age_subject_accuracy) +
     plot_layout(widths = c(5, 1)))

gg_combined

ggsave(filename = here("graphs", "accuracy_by_sex_age_over_days_and_overall.pdf"),
       plot = gg_combined,
       units = "in",
       height = 6,
       width = 11,
       dpi = 600)

ggsave(filename = here("graphs", "accuracy_by_sex_age_over_days_and_overall.svg"),
       plot = gg_combined,
       units = "in",
       height = 6,
       width = 11,
       dpi = 600)
