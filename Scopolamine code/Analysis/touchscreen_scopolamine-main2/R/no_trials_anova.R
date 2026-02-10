library(here)
library(tidyverse)
library(ggstatsplot)
library(afex)
library(emmeans)
library(kableExtra)
library(flextable)
library(ggsci)
library(papaja)


path <- here("data", "processed_data.rds")

df_no_trials <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::group_by(participant, dose, test_day) |>
  dplyr::summarise(no_trials = n())

# mean number of trials
mean(df_no_trials$no_trials)

aov_no_trials <- aov_ez("participant", "no_trials", df_no_trials, within = c("dose", "test_day"))
aov_no_trials |> nice(aov = TRUE, p.adjust ="holm") |>
  as_flextable() |>
  flextable::set_table_properties(width = .5) |>
  save_as_docx(path = here("tables", "no_trials_anova_table.docx"))

summary(aov_no_trials)

papaja::apa_print(aov_no_trials)


gg_afex_no_trials <- afex_plot(aov_no_trials, x = "test_day", trace = "dose", error = "within",
                               mapping = c("shape", "linetype", "color"),
                               factor_levels = list(test_day = c("1", "2", "3", "4", "5")),
                               point_arg = list(size = 3), data_arg = list(cex = 2)) +
  theme_classic(base_size = 14) +
  labs(title = "Number of trials by test day and scopolamine dose",
       x = "Test day",
       y = "Number of trials") +
  scale_color_jco()

gg_afex_no_trials

ggsave(filename = here("graphs", "anova_no_trials.pdf"),
       plot = gg_afex_no_trials,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "anova_no_trials.svg"),
       plot = gg_afex_no_trials,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)
