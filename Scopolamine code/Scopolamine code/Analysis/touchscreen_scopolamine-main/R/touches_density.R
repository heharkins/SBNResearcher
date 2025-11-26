library(here)
library(tidyverse)
library(ggsci)

# regex patterns
ptn_last_number <- "([-+]?[0-9]*\\.[0-9]+)"

# Define the folder path
folder_path <- here("touches_data")

# Get all excel files in the folder
excel_files <- list.files(path = folder_path, pattern = "^[^~]*\\.csv$",
                          full.names = TRUE)

named_excel_files <- purrr::set_names(excel_files, nm = basename(excel_files))

df_ <- named_excel_files |>
  map_dfr(read_csv, .id = "file", col_select = c("participant", "mouse.x", "mouse.y", "status")) |>
  dplyr::filter(!is.na(status)) |>
  dplyr::mutate(participant = str_to_title(participant) |> factor(),
                mouse_x_last = stringi::stri_match_last_regex(mouse.x, ptn_last_number)[, 1] |>
                  as.numeric(),
                mouse_y_last = stringi::stri_match_last_regex(mouse.y, ptn_last_number)[, 1] |>
                  as.numeric(),
                alias = case_when(
                  participant == "Crumble" ~ "M7",
                  participant == "Harmonia" ~ "M5",
                  participant == "Icarus" ~ "M2",
                  participant == "Kore" ~ "M3",
                  participant == "Muffin" ~ "M1",
                  participant == "Nereus" ~ "M6",
                  participant == "Nyx" ~ "M8",
                  participant == "Quintas" ~ "M4",
                  TRUE ~ NA_character_))

# faceted by marmoset -----------------------------------------------------
gg_touch_dens <- ggplot(df_, aes(x = mouse_x_last, y = mouse_y_last)) +
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

gg_touch_dens

ggsave(filename = here("graphs", "touches.pdf"),
       plot = gg_touch_dens,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)

ggsave(filename = here("graphs", "touches.svg"),
       plot = gg_touch_dens,
       units = "in",
       height = 4,
       width = 8,
       dpi = 600)

# combined data -----------------------------------------------------------
gg_touch_dens_comb <- ggplot(df_, aes(x = mouse_x_last, y = mouse_y_last)) +
  stat_density_2d(geom = "raster", contour = FALSE, aes(fill = after_stat(density)),
                  show.legend = FALSE) +
  geom_rect(aes(xmin = -0.50, xmax = -0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  geom_rect(aes(xmax = 0.50, xmin = 0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  coord_fixed(ratio = 1) +
  labs(x = "X axis coordinates", y = "Y axis coordinates") +
  expand_limits(x = c(-0.55, 0.55), y = c(-0.2, 0.2)) +
  scale_x_continuous(breaks = c(-0.45, -0.15, 0.15, 0.45)) +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gg_touch_dens_comb

ggsave(filename = here("graphs", "touches_combined.pdf"),
       plot = gg_touch_dens_comb,
       units = "in",
       height = 3,
       width = 7.5,
       dpi = 600)

ggsave(filename = here("graphs", "touches_combined.svg"),
       plot = gg_touch_dens_comb,
       units = "in",
       height = 3,
       width = 7.5,
       dpi = 600)

gg_touch_point <- ggplot(df_, aes(x = mouse_x_last, y = mouse_y_last, colour = alias)) +
  geom_point(size = 1.3, alpha = 0.6) +
  geom_rect(aes(xmin = -0.50, xmax = -0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  geom_rect(aes(xmax = 0.50, xmin = 0.20, ymin = -0.15, ymax = 0.15), fill = NA,
            color = "firebrick1", linetype = 2, linewidth = 0.5) +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  expand_limits(x = c(-0.51, 0.51), y = c(-0.16, 0.16)) +
  scale_x_continuous(breaks = c(-0.45, -0.15, 0.15, 0.45)) +
  scale_color_nejm() +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 8)))

gg_touch_point

ggsave(filename = here("graphs", "touches_points.pdf"),
       plot = gg_touch_point,
       units = "in",
       height = 3.4,
       width = 7.5,
       dpi = 600)

ggsave(filename = here("graphs", "touches_points.svg"),
       plot = gg_touch_point,
       units = "in",
       height = 3.4,
       width = 7.5,
       dpi = 600)