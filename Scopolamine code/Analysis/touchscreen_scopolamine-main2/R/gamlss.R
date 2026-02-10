library(here)
library(tidyverse)
library(kableExtra)
library(lme4)
library(modelsummary)
library(splines)
library(easystats)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
library(insight)
library(broom.mixed)
library(geepack)

options(scipen = 999)


# load data ---------------------------------------------------------------
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
                distance_to_center_c = scale(distance_to_center) |> as.numeric()) |>
  na.omit() # to have the same numbe rof observations for trials with lookback to a previous trial

df_s_Muffin <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::filter(participant != "Muffin") |>
  dplyr::mutate(participant = droplevels(participant)) |>
  dplyr::select(participant, dose, test_day, choice_latency,
                response_side, age_c, sex, age, initiation_time_last,
                initiation_num_touch, response_correct, response_num_touch,
                choice_latency_prev, response_correct_prev, day, distance_to_center,
                trial) |>
  dplyr::mutate(choice_latency_c = scale(choice_latency) |> as.numeric(),
                initiation_time_last_c = scale(initiation_time_last) |> as.numeric(),
                choice_latency_prev_c = scale(choice_latency_prev) |> as.numeric(),
                distance_to_center_c = scale(distance_to_center) |> as.numeric()) |>
  na.omit() # to have the same numbe rof observations for trials with lookback to a previous trial

# gamlss -------------------------------------------------------------------
# probability of correct decision

m_success <- gamlss(response_correct ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                     choice_latency_c + response_correct_prev + response_num_touch +
                     distance_to_center_c, re(random = ~pb(day)|participant), family = BI(),
                   data = df)
AIC(m_success)
summary(m_success)

pdf(here("graphs", "gamlss_diagnostic.pdf"),
    width = 8, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")
plot(m_success)
dev.off()

svg(here("graphs", "gamlss_diagnostic.svg"),
    width = 8, height = 6, # Width and height in inches
    bg = "white")
plot(m_success)
dev.off()

termplot(m_success)

# adding the second degree polynomial of the distance to stimulus center
m_success_dist <- gamlss(response_correct ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                      choice_latency_c + response_correct_prev + response_num_touch +
                      distance_to_center_c + poly(distance_to_center_c, 2),
                      re(random = ~pb(day)|participant), family = BI(),
                    data = df)
summary(m_success_dist)

m_success_s_Muffin <- gamlss(response_correct ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                      choice_latency_c + response_correct_prev + response_num_touch +
                      distance_to_center_c, re(random = ~pb(day)|participant), family = BI(),
                    data = df_s_Muffin)

summary(m_success_s_Muffin)
AIC(m_success_s_Muffin)


tab_model(m_success, m_success_s_Muffin, show.intercept = FALSE, show.r2 = TRUE,
          string.ci = "95% CI", file = here("tables", "gamlss_success_table.doc"))
# write_rds(m_success, "C:/Users/jazzh/OneDrive/Documents/R/Code/touchscreen_scopolamine/data/gamlss_success.rds")


# Geese model
m_geese_success <- geese(response_correct ~ sex*dose + bs(test_day) + age + bs(trial) +
        choice_latency_c + response_correct_prev + response_num_touch +
        distance_to_center_c, family = binomial, id = participant, data = df,
        control = geese.control(trace = TRUE),
        corstr = "ar1")

summary(m_geese_success)
# write_rds(m_geese_success, "C:/Users/jazzh/OneDrive/Documents/R/Code/touchscreen_scopolamine/data/m_geese_success.rds")


m_success_glmer <- glmer(response_correct ~ sex*dose + age + ns(trial, 3) + ns(test_day, 3) +
                      choice_latency_c + response_correct_prev + response_num_touch +
                      distance_to_center_c + (day|participant), family = binomial,
                    data = df)
AIC(m_success)
summary(m_success)

# latency -------------------------------------------------------------------

df_10 <- df |> dplyr::filter(choice_latency <= 10)
df_10m <- df_s_Muffin |> dplyr::filter(choice_latency <= 10)

m_latency <- gamlss(choice_latency ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                      response_correct + response_correct_prev + response_num_touch +
                      distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                      response_side + re(random = ~pb(day)|participant), family = GA(),
                    i.control = glim.control(cyc = 500, bf.cyc = 100),
                    data = df_10)
summary(m_latency)
plot(m_latency)


m_latency_s_Muffin <- gamlss(choice_latency ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                      response_correct + response_correct_prev + response_num_touch +
                      distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                      response_side + re(random = ~pb(day)|participant), family = GA(),
                    i.control = glim.control(cyc = 500, bf.cyc = 100),
                    data = df_10m)

tab_model(m_latency, m_latency_s_Muffin, show.intercept = FALSE, show.r2 = TRUE,
          string.ci = "95% CI", file = here("tables", "gamlss_latency_table.doc"))


# adding the second degree polynomial of the distance to stimulus center
m_latency_dist <- gamlss(choice_latency ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                      response_correct + response_correct_prev + response_num_touch +
                      distance_to_center_c + poly(distance_to_center_c, 2) +
                      choice_latency_prev + initiation_time_last_c +
                      response_side + re(random = ~pb(day)|participant) + (1|test_day), family = GB2(),
                    nu.formula = ~dose, i.control = glim.control(cyc = 500, bf.cyc = 100),
                    method = mixed(5, 40),
                    data = df_10)
summary(m_latency_dist)

# Geese model
m_geese_latency <- geese(choice_latency ~ sex*dose + bs(test_day) + age + bs(trial) +
                           response_correct + response_correct_prev + response_num_touch +
                           distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                           response_side,
                         data = df_10, id = participant, family = Gamma(link = "log"),
                         control = geese.control(trace = TRUE),
                         corstr = "ar1")
summary(m_geese_latency)

# write_rds(m_latency, "C:/Users/jazzh/OneDrive/Documents/R/Code/touchscreen_scopolamine/data/gamlss_latency.rds")
# write_rds(m_geese_latency, "C:/Users/jazzh/OneDrive/Documents/R/Code/touchscreen_scopolamine/data/m_geese_latency.rds")

dist <- fitDist(df$response_correct, k = 2, type = "binom", trace = FALSE, try.gamlss = TRUE)
dist
