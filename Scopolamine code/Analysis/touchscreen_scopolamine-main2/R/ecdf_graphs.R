library(here)
library(tidyverse)
library(summarytools)
library(ggsci)

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


# density plots -----------------------------------------------------------
# Day 1
gg_init_latency_ecdf_d1 <- ggplot(df |> dplyr::filter(test_day == 1),
                                      aes(x = initiation_time_last, colour = dose)) +
  stat_ecdf(size = 1.5) +
  facet_wrap(~alias, nrow = 2) +
  labs(x = "Trial initiation latencies on day 1 (s)", y = "Cumulative proportion") +
  coord_cartesian(xlim = c(0, 50)) +
  scale_color_jco() +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_init_latency_ecdf_d1

ggsave(filename = here("graphs", "ecdf_init_lat_d1.pdf"),
       plot = gg_init_latency_ecdf_d1,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

ggsave(filename = here("graphs", "ecdf_init_lat_d1.svg"),
       plot = gg_init_latency_ecdf_d1,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

# Day 4
gg_init_latency_ecdf_d4 <- ggplot(df |> dplyr::filter(test_day == 4),
                                  aes(x = initiation_time_last, colour = dose)) +
  stat_ecdf(size = 1.5) +
  facet_wrap(~alias, nrow = 2) +
  labs(x = "Trial initiation latencies on day 4 (s)", y = "Cumulative proportion") +
  coord_cartesian(xlim = c(0, 50)) +
  scale_color_jco() +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_init_latency_ecdf_d4

ggsave(filename = here("graphs", "ecdf_init_lat_d4.pdf"),
       plot = gg_init_latency_ecdf_d4,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)

ggsave(filename = here("graphs", "ecdf_init_lat_d4.svg"),
       plot = gg_init_latency_ecdf_d4,
       units = "in",
       height = 7,
       width = 10,
       dpi = 600)
