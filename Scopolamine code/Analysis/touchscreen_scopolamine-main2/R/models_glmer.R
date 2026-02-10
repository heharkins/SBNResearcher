library(here)
library(tidyverse)
library(kableExtra)
library(lme4)
library(rms)
library(brms)
library(splines)
library(easystats)
library(lmerTest)
library(glmmTMB)


# load data ---------------------------------------------------------------
df <- read_rds(here("data", "processed_data.rds")) |>
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
  na.omit() # to have the same number of observations for trials with lookback to a previous trial


# glmer -------------------------------------------------------------------
# probability of correct decision
m_success <- glmer(response_correct ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                      choice_latency_c + response_correct_prev + response_num_touch +
                      distance_to_center_c +
                      (day|participant), family = binomial,
                     data = df)

summary(m_success)

check_model(m_success)

# mb_success <- brm(response_correct ~ sex*dose + test_day + s(test_day, k = 3) + age + trial + s(trial, k = 3) +
#                      choice_latency + response_correct_prev + response_num_touch +
#                      distance_to_center +
#                      (day|participant), family = bernoulli(link="logit"),
#                    data = df, control = list(adapt_delta = .95),
#                   iter = 2000, warmup = 1000, chains = 4, cores = 4,
#                   seed = 1215)

m_latency <- glmer(choice_latency ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                     response_correct + response_correct_prev + response_num_touch +
                     distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                     response_side +
                     (ns(test_day, 3)|participant), family = Gamma(link = "log"),
                   data = df |> dplyr::filter(choice_latency <= 10), control = glmerControl(optimizer = "bobyqa"))

summary(m_latency)

m_latency_s <- glmer(choice_latency ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                       response_correct + response_correct_prev + response_num_touch +
                       distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                       response_side + dose:day +
                       (1|participant), family = Gamma(link = "log"),
                     data = df|> dplyr::filter(choice_latency <= 10), control = glmerControl(optimizer = "bobyqa"))

summary(m_latency_s)

m_latency_s1 <- glm(choice_latency ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                        response_correct + response_correct_prev + response_num_touch +
                        distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                        response_side + dose:day, family = Gamma(link = "log"),
                      data = df|> dplyr::filter(choice_latency <= 10))

summary(m_latency_s1)
compare_performance(m_latency, m_latency_s)
model_performance(m_latency_s1)
check_model(m_latency_s1)

AICcmodavg::aictab(list(m1 = m_latency, m2 = m_latency_s))

