suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
})


# load data ---------------------------------------------------------------

df <- read_rds(here("data", "processed_data.rds")) |>
  dplyr::mutate(alias = case_when(
    participant == "Crumble" ~ "M7",
    participant == "Harmonia" ~ "M5",
    participant == "Icarus" ~ "M2",
    participant == "Kore" ~ "M3",
    participant == "Muffin" ~ "M1",
    participant == "Nereus" ~ "M6",
    participant == "Nyx" ~ "M8",
    participant == "Quintas" ~ "M4",
    TRUE ~ NA_character_))


# calculate transition probabilities --------------------------------------
alias_transitions <- df %>%
  arrange(alias, dose, test_day, trial) %>%
  group_by(alias, dose, test_day) %>%
  mutate(prev_correct = lag(response_correct)) %>%
  summarise(
    p_after_correct = mean(response_correct[prev_correct == 1], na.rm = TRUE),
    p_after_error   = mean(response_correct[prev_correct == 0], na.rm = TRUE),
  ) %>%
  summarise(
    p_after_correct = mean(p_after_correct, na.rm = TRUE),
    p_after_error   = mean(p_after_error,   na.rm = TRUE),
  ) %>%
  pivot_longer(
    cols = c(p_after_correct, p_after_error),
    names_to = "transition",
    values_to = "prob_correct"
  ) %>%
  mutate(
    transition = recode(
      transition,
      p_after_correct = "After Correct",
      p_after_error   = "After Error"
    ),
    transition = factor(transition, levels = c("After Error", "After Correct"))
  ) |>
  group_by(dose, transition) %>%
  summarise(
    mean_prob = mean(prob_correct, na.rm = TRUE),
    sd_prob   = sd(prob_correct, na.rm = TRUE),
    n_sess    = sum(!is.na(prob_correct)),
    sem_prob  = sd_prob / sqrt(n_sess),
    .groups = "drop")


# Conditional transition plots --------------------------------------------

# Paired (side-by-side) bar plot per alias
cond_cor_plot <- ggplot(
  alias_transitions,
  aes(x = dose, y = mean_prob, fill = transition)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean_prob - sem_prob,
      ymax = mean_prob + sem_prob
    ),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    x = "Dose",
    y = "Conditional percent correct",
    fill = NULL
  ) +
  theme_classic(base_size = 14) +
  scale_fill_jco()

cond_cor_plot

ggsave(filename = here("graphs", "cond_correct.pdf"),
       plot = cond_cor_plot,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

ggsave(filename = here("graphs", "cond_correct.svg"),
       plot = cond_cor_plot,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

# t-test ------------------------------------------------------------------
paired_transition_data <- df %>%
  arrange(alias, dose, test_day, trial) %>%
  group_by(alias, dose, test_day) %>%
  mutate(prev_correct = lag(response_correct)) %>%
  summarise(
    p_after_correct = mean(response_correct[prev_correct == 1], na.rm = TRUE),
    p_after_error   = mean(response_correct[prev_correct == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(alias, dose) %>%
  summarise(
    p_after_correct = mean(p_after_correct, na.rm = TRUE),
    p_after_error   = mean(p_after_error,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    !is.na(p_after_correct),
    !is.na(p_after_error),
  dose == "high")

t_test_result <- t.test(
  paired_transition_data$p_after_correct,
  paired_transition_data$p_after_error,
  paired = TRUE
)

t_test_result