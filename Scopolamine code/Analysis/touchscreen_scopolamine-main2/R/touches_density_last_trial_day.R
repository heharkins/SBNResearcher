library(here)
library(tidyverse)
library(ggsci)

# load data ---------------------------------------------------------------
df <- read_rds(here("data", "processed_data.rds")) |>
  dplyr::select(participant, dose, test_day, response_x_last, response_y_last) |>
  dplyr::mutate(alias = case_when(
                  participant == "Crumble" ~ "M7",
                  participant == "Harmonia" ~ "M5",
                  participant == "Icarus" ~ "M2",
                  participant == "Kore" ~ "M3",
                  participant == "Muffin" ~ "M1",
                  participant == "Nereus" ~ "M6",
                  participant == "Nyx" ~ "M8",
                  participant == "Quintas" ~ "M4",
                  TRUE ~ NA_character_)) |>
  dplyr::filter(test_day == max(test_day), .by = c("participant", "dose"))



# Vehicle condition -------------------------------------------------------
df_vehicle <- df |>
  dplyr::filter(dose == "vehicle") |>
  dplyr::select(-dose)

gg_touch_dens_vehicle <- ggplot(df_vehicle, aes(x = response_x_last, y = response_y_last)) +
  stat_density_2d(geom = "raster", contour = FALSE, aes(fill = after_stat(ndensity)),
                  show.legend = FALSE) +
  geom_rect(aes(xmin = -0.50, xmax = -0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  geom_rect(aes(xmax = 0.50, xmin = 0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  coord_fixed(ratio = 1) +
  labs(x = "X axis coordinates", y = "Y axis coordinates") +
  expand_limits(x = c(-0.55, 0.55), y = c(-0.2, 0.2)) +
  scale_x_continuous(breaks = c(-0.45, -0.15, 0.15, 0.45)) +
  facet_wrap(~alias) +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_touch_dens_vehicle

ggsave(filename = here("graphs", "touches_vehicle.pdf"),
       plot = gg_touch_dens_vehicle,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "touches_vehicle.svg"),
       plot = gg_touch_dens_vehicle,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)


# Low dose condition ------------------------------------------------------
df_low <- df |>
  dplyr::filter(dose == "low") |>
  dplyr::select(-dose)

gg_touch_dens_low <- ggplot(df_low, aes(x = response_x_last, y = response_y_last)) +
  stat_density_2d(geom = "raster", contour = FALSE, aes(fill = after_stat(ndensity)),
                  show.legend = FALSE) +
  geom_rect(aes(xmin = -0.50, xmax = -0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  geom_rect(aes(xmax = 0.50, xmin = 0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  coord_fixed(ratio = 1) +
  labs(x = "X axis coordinates", y = "Y axis coordinates") +
  expand_limits(x = c(-0.55, 0.55), y = c(-0.2, 0.2)) +
  scale_x_continuous(breaks = c(-0.45, -0.15, 0.15, 0.45)) +
  facet_wrap(~alias) +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_touch_dens_low

ggsave(filename = here("graphs", "touches_low_dose.pdf"),
       plot = gg_touch_dens_low,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "touches_low_dose.svg"),
       plot = gg_touch_dens_low,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)


# High dose condition -----------------------------------------------------
df_high <- df |>
  dplyr::filter(dose == "high") |>
  dplyr::select(-dose)

gg_touch_dens_high <- ggplot(df_high, aes(x = response_x_last, y = response_y_last)) +
  stat_density_2d(geom = "raster", contour = FALSE, aes(fill = after_stat(ndensity)),
                  show.legend = FALSE) +
  geom_rect(aes(xmin = -0.50, xmax = -0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  geom_rect(aes(xmax = 0.50, xmin = 0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  coord_fixed(ratio = 1) +
  labs(x = "X axis coordinates", y = "Y axis coordinates") +
  expand_limits(x = c(-0.55, 0.55), y = c(-0.2, 0.2)) +
  scale_x_continuous(breaks = c(-0.45, -0.15, 0.15, 0.45)) +
  facet_wrap(~alias) +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_touch_dens_high

ggsave(filename = here("graphs", "touches_high_dose.pdf"),
       plot = gg_touch_dens_high,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "touches_high_dose.svg"),
       plot = gg_touch_dens_high,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)

