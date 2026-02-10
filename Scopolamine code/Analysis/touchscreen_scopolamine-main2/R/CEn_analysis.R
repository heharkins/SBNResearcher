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
# compute CEn+1 scores -----------------------------------------------------

# helper functions
compute_CEn <- function(correct_vec, n_values = 2:4) {
  x <- as.integer(correct_vec)
  stopifnot(all(x %in% c(0L, 1L)))

  map_dfr(n_values, function(n) {
    t_idx <- (n + 2):length(x)
    if (length(t_idx) == 0) {
      return(tibble(n = n, n_events = 0L, pct_correct = NA_real_))
    }

    is_event <- vapply(t_idx, function(t) {
      (x[t - (n + 1)] == 1L) && all(x[(t - n):(t - 1)] == 0L)
    }, logical(1))

    score_trials <- t_idx[is_event]
    scores <- x[score_trials]

    tibble(
      CEn = n - 1,
      n_events = length(scores),
      pct_error = if (length(scores) == 0) NA_real_ else mean(1 - scores) * 100
    )
  })
}

df_sorted <- df %>%
  arrange(alias, dose, test_day, trial)


# 2) CEn+1 per session (n = 2..7)
cen_by_session <- df_sorted %>%
  group_by(alias, dose, test_day) %>%
  group_modify(~ compute_CEn(.x$response_correct, n_values = 2:4)) %>%
  ungroup()

# 3) Aggregate across sessions (simple mean of session percents)
cen_group_summary <- cen_by_session %>%
  group_by(dose, CEn, alias) %>%
  summarise(
    sessions_with_events = sum(!is.na(pct_error)),
    mean_pct_error = mean(pct_error, na.rm = TRUE),
    .groups = "drop"
  ) |> dplyr::filter(CEn == 3)


# graph -------------------------------------------------------------------

dodge_width <- 0.15

ggCEn <- ggplot(
  cen_by_session,
  aes(x = CEn, y = pct_error, color = dose, group = dose)
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
  scale_x_continuous(breaks = 1:3) +
  labs(
    x = "Consecutive erroneous responses",
    y = "Percent incorrect",
    color = "Dose"
  ) +
  scale_color_jco() +
  theme_classic(base_size = 14)

ggCEn

ggsave(filename = here("graphs", "CEn.pdf"),
       plot = ggCEn,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

ggsave(filename = here("graphs", "CEn.svg"),
       plot = ggCEn,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

# ANOVA -------------------------------------------------------------------

# For some marmosets CEn > 3 are missing and I filter them out before ANOVA
# Also compare only vehicle with high dose. The comparison shows no statistically significant effect of dose.

cen_by_session_agg_day <- cen_by_session |>
  group_by(alias, dose, CEn) |>
  summarise(cen_pct_correct = mean(cen_pct_correct, na.rm = TRUE)) |>
  dplyr::filter(CEn <= 3L, dose != "low")


aov_CEn <- aov_ez("alias", "cen_pct_correct", ecn_by_session_agg_day, within = c("dose", "CEn"))

summary(aov_CEn)
