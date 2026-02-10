suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(afex)
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
# compute ECn+1 scores -----------------------------------------------------

# helper functions
compute_ECn <- function(correct_vec, n_values = 2:8) {
  x <- as.integer(correct_vec)
  stopifnot(all(x %in% c(0L, 1L)))

  map_dfr(n_values, function(n) {
    t_idx <- (n + 2):length(x)
    if (length(t_idx) == 0) {
      return(tibble(n = n, n_events = 0L, pct_correct = NA_real_))
    }

    is_event <- vapply(t_idx, function(t) {
      (x[t - (n + 1)] == 0L) && all(x[(t - n):(t - 1)] == 1L)
    }, logical(1))

    score_trials <- t_idx[is_event]
    scores <- x[score_trials]

    tibble(
      ECn = n - 1,
      n_events = length(scores),
      pct_correct = if (length(scores) == 0) NA_real_ else mean(scores) * 100
    )
  })
}

df_sorted <- df %>%
  arrange(alias, dose, test_day, trial)


# 2) ECn+1 per session (n = 2..7)
ecn_by_session <- df_sorted %>%
  group_by(alias, dose, test_day) %>%
  group_modify(~ compute_ECn(.x$response_correct, n_values = 2:8)) %>%
  ungroup() %>%
  rename(ecn_events = n_events, ecn_pct_correct = pct_correct)

# 3) Aggregate across sessions (simple mean of session percents)
ecn1_group_summary <- ecn_by_session %>%
  group_by(dose, ECn) %>%
  summarise(
    sessions_with_events = sum(!is.na(ecn_pct_correct)),
    mean_pct_correct = mean(ecn_pct_correct, na.rm = TRUE),
    .groups = "drop"
  )


# graph -------------------------------------------------------------------

dodge_width <- 0.15

ggECn <- ggplot(
  ecn1_by_session,
  aes(x = ECn, y = ecn1_pct_correct, color = dose, group = dose)
) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1.5,
    position = position_dodge(width = dodge_width)
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    position = position_dodge(width = dodge_width)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.3,
    linewidth = 1.5,
    position = position_dodge(width = dodge_width)
  ) +
  scale_x_continuous(breaks = 1:7) +
  labs(
    x = "Consecutive correct responses",
    y = "Percent correct",
    color = "Dose"
  ) +
  scale_color_jco() +
  theme_classic(base_size = 14)

ggECn

ggsave(filename = here("graphs", "ECn.pdf"),
       plot = ggECn,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

ggsave(filename = here("graphs", "ECn.svg"),
       plot = ggECn,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

# ANOVA -------------------------------------------------------------------

# For some marmosets ECn > 3 are missing and I filter them out before ANOVA
# Also compare only vehicle with high dose. The comparison shows no statistically significant effect of dose.

ecn_by_session_agg_day <- ecn_by_session |>
  group_by(alias, dose, ECn) |>
  summarise(ecn_pct_correct = mean(ecn_pct_correct, na.rm = TRUE)) |>
  dplyr::filter(ECn <= 3L, dose != "low")


aov_ECn <- aov_ez("alias", "ecn_pct_correct", ecn_by_session_agg_day, within = c("dose", "ECn"))

summary(aov_ECn)
