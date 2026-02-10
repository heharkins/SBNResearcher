library(here)
library(tidyverse)
library(ggstatsplot)
library(afex)
library(emmeans)
library(kableExtra)
library(flextable)
library(ggsci)
library(rstatix)

path <- here("data", "processed_data.rds")

df_accuracy <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::filter(participant != "Muffin") |>
  dplyr::group_by(participant, sex, dose, test_day) |>
  dplyr::summarise(accuracy = mean(response_correct))

df_accuracy_day5_high_control <- df_accuracy |>
  dplyr::filter(dose != "low", test_day == 5) |>
  dplyr::mutate(dose = as.character(dose), test_day = NULL)

vehicle <- df_accuracy_day5_high_control |>
  dplyr::filter(dose == "vehicle") |> pull(accuracy)

high <- df_accuracy_day5_high_control |>
  dplyr::filter(dose == "high") |> pull(accuracy)

mean(vehicle)
mean(high)

stats::t.test(vehicle, high, data = df_accuracy_day5_high_control, paired = TRUE)


aov_accuracy <- aov_ez("participant", "accuracy", df_accuracy, within = c("dose", "test_day"))
aov_accuracy |> nice(aov = TRUE, p.adjust ="holm") |>
  as_flextable() |>
  flextable::set_table_properties(width = .5) |>
  save_as_docx(path = here("tables", "accuracy_anova_table.docx"))

summary(aov_accuracy)

df_accuracy_age <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::filter(participant != "Muffin") |>
  dplyr::group_by(participant, dose, age_c) |>
  dplyr::summarise(accuracy = mean(response_correct))

aov_accuracy_age <- aov_ez("participant", "accuracy", df_accuracy_age, within = c("dose"),
                           between = "age_c")
aov_accuracy |> nice(aov = TRUE, p.adjust ="holm") |>
  as_flextable() |>
  flextable::set_table_properties(width = .5) |>
  save_as_docx(path = here("tables", "accuracy_anova_table.docx"))

summary(aov_accuracy)

gg_afex_accuracy <- afex_plot(aov_accuracy, x = "test_day", trace = "dose", error = "within",
                     factor_levels = list(test_day = c("1", "2", "3", "4", "5")),
                     mapping = c("shape", "linetype", "color"),
                     point_arg = list(size = 3), data_arg = list(cex = 2)) +
  theme_classic(base_size = 14) +
  labs(title = "Accuracy by test day and scopolamine dose",
       x = "Test day",
       y = "Accuracy") +
  scale_color_jco()

gg_afex_accuracy

ggsave(filename = here("graphs", "anova_accuracy.pdf"),
       plot = gg_afex_accuracy,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "anova_accuracy.svg"),
       plot = gg_afex_accuracy,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)