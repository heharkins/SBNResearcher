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

df_choice_latency <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::group_by(participant, dose, test_day) |>
  dplyr::summarise(choice_latency = mean(choice_latency))

aov_choice_latency <- aov_ez("participant", "choice_latency", df_choice_latency, within = c("dose", "test_day"))
aov_choice_latency |> nice(aov = TRUE, p.adjust ="holm") |>
  as_flextable() |>
  flextable::set_table_properties(width = .5) |>
  save_as_docx(path = here("tables", "choice_latency_anova_table.docx"))

summary(aov_choice_latency)

papaja::apa_print(aov_choice_latency)


gg_afex_choice_latency <- afex_plot(aov_choice_latency, x = "test_day", trace = "dose", error = "within",
                               mapping = c("shape", "linetype", "color"),
                               factor_levels = list(test_day = c("1", "2", "3", "4", "5")),
                               point_arg = list(size = 3), data_arg = list(cex = 2)) +
  theme_classic(base_size = 14) +
  labs(title = "Choice latency by test day and scopolamine dose",
       x = "Test day",
       y = "Latency(s)") +
  scale_color_jco()

gg_afex_choice_latency

ggsave(filename = here("graphs", "anova_choice_latency.pdf"),
       plot = gg_afex_choice_latency,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "anova_choice_latency.svg"),
       plot = gg_afex_choice_latency,
       units = "in",
       height = 5,
       width = 8,
       dpi = 600)