library(tidyverse)
library(ggplot2)
library(dplyr)
library(bayesrules)
library(bayesplot)
library(rstanarm)
library(tidybayes)

#loading in initial data csv
drug_data <- read.csv("data/drugs.csv")

#filtering for age ranges and 5 biggest states by population
alc_dis_rate_1217 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.12.17`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan"))

alc_dis_rate_1825 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.18.25`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan"))

alc_dis_rate_26 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.26.`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan"))

"
AGE GROUP 1: 12-17
"
# initial boxplot for each state with overall mean
mean_rate_1217 <- mean(alc_dis_rate_1217$alc_dis_rate)

ggplot(alc_dis_rate_1217, aes(x = state, y = alc_dis_rate)) +
  geom_boxplot() +
  labs(title = "Alcohol Disorder Rate for Age Range 12-17",
       x = "State",
       y = "Alcohol Disorder Rate") +
  geom_hline(yintercept = mean_rate_1217, color = "red", linetype = "solid")

# hierarchical model
adr_1217_hierarchical <- stan_glmer(
  alc_dis_rate ~ (1 | state),
  data = alc_dis_rate_1217, family = gaussian,
  prior_intercept = normal(30, 15),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter=5000*2, seed=84375, refresh=FALSE)

# diagnostic plots
mcmc_trace(adr_1217_hierarchical)
mcmc_dens_overlay(adr_1217_hierarchical)
mcmc_acf(adr_1217_hierarchical)

pp_check(adr_1217_hierarchical) + 
  xlab("Alcohol Disorder Rate (12-17)")

# credible intervals
state_summary_1217 <- adr_1217_hierarchical %>%
  spread_draws(b[term, state], `(Intercept)`) %>%
  mutate(state_mean = `(Intercept)` + b) %>%
  group_by(state) %>%
  mean_qi(state_mean, .width = 0.80) %>%
  mutate(state = fct_reorder(state, state_mean))

ggplot(state_summary_1217, 
       aes(x = state, y = state_mean, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  labs(title = "Posterior State Means: Alcohol Disorder Rate (12-17)",
       subtitle = "80% Credible Intervals",
       x = "State", y = "Alcohol Disorder Rate (per 1000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# predictive intervals
new_states <- data.frame(
  state = c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")
)

set.seed(84735)
predictions_1217 <- posterior_predict(adr_1217_hierarchical, newdata = new_states)

ppc_intervals(
  y    = alc_dis_rate_1217 %>% group_by(state) %>% summarise(m = mean(alc_dis_rate)) %>% pull(m),
  yrep = predictions_1217,
  prob_outer = 0.80
) + ggplot2::scale_x_continuous(
  labels = new_states$state,
  breaks = 1:nrow(new_states)
) + labs(title = "Posterior Predictive Intervals by State (12-17)",
         y = "Alcohol Disorder Rate (per 1000)")


"
AGE GROUP 2: 18-25
"
# initial boxplot for each state with overall mean
mean_rate_1825 <- mean(alc_dis_rate_1825$alc_dis_rate)

ggplot(alc_dis_rate_1825, aes(x = state, y = alc_dis_rate)) +
  geom_boxplot() +
  labs(title = "Alcohol Disorder Rate for Age Range 18-25",
       x = "State",
       y = "Alcohol Disorder Rate") +
  geom_hline(yintercept = mean_rate_1825, color = "red", linetype = "solid")

# hierarchical model
adr_1825_hierarchical <- stan_glmer(
  alc_dis_rate ~ (1 | state),
  data = alc_dis_rate_1825, family = gaussian,
  prior_intercept = normal(140, 30),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter=5000*2, seed=84375, refresh=FALSE)

# diagnostic plots
mcmc_trace(adr_1825_hierarchical)
mcmc_dens_overlay(adr_1825_hierarchical)
mcmc_acf(adr_1825_hierarchical)

pp_check(adr_1825_hierarchical) + 
  xlab("Alcohol Disorder Rate (18-25)")

# credible intervals
state_summary_1825 <- adr_1825_hierarchical %>%
  spread_draws(b[term, state], `(Intercept)`) %>%
  mutate(state_mean = `(Intercept)` + b) %>%
  group_by(state) %>%
  mean_qi(state_mean, .width = 0.80) %>%
  mutate(state = fct_reorder(state, state_mean))

ggplot(state_summary_1825, 
       aes(x = state, y = state_mean, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  labs(title = "Posterior State Means: Alcohol Disorder Rate (18-25)",
       subtitle = "80% Credible Intervals",
       x = "State", y = "Alcohol Disorder Rate (per 1000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# predictive intervals
new_states <- data.frame(
  state = c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")
)

set.seed(84735)
predictions_1825 <- posterior_predict(adr_1825_hierarchical, newdata = new_states)

ppc_intervals(
  y    = alc_dis_rate_1825 %>% group_by(state) %>% summarise(m = mean(alc_dis_rate)) %>% pull(m),
  yrep = predictions_1825,
  prob_outer = 0.80
  ) + ggplot2::scale_x_continuous(
    labels = new_states$state,
    breaks = 1:nrow(new_states)
  ) + labs(title = "Posterior Predictive Intervals by State (18-25)",
       y = "Alcohol Disorder Rate (per 1000)")



"
AGE GROUP 3: 26+
"

# initial boxplot for each state with overall mean
mean_rate_26 <- mean(alc_dis_rate_26$alc_dis_rate)

ggplot(alc_dis_rate_26, aes(x = state, y = alc_dis_rate)) +
  geom_boxplot() +
  labs(title = "Alcohol Disorder Rate for Age Range 26+",
       x = "State",
       y = "Alcohol Disorder Rate") +
  geom_hline(yintercept = mean_rate_26, color = "red", linetype = "solid")

# hierarchical model
adr_26_hierarchical <- stan_glmer(
  alc_dis_rate ~ (1 | state),
  data = alc_dis_rate_26, family = gaussian,
  prior_intercept = normal(60, 20),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter=5000*2, seed=84375, refresh=FALSE)

# diagnostic plots
mcmc_trace(adr_26_hierarchical)
mcmc_dens_overlay(adr_26_hierarchical)
mcmc_acf(adr_26_hierarchical)

pp_check(adr_26_hierarchical) + 
  xlab("Alcohol Disorder Rate (26+)")

# credible intervals
state_summary_26 <- adr_26_hierarchical %>%
  spread_draws(b[term, state], `(Intercept)`) %>%
  mutate(state_mean = `(Intercept)` + b) %>%
  group_by(state) %>%
  mean_qi(state_mean, .width = 0.80) %>%
  mutate(state = fct_reorder(state, state_mean))

ggplot(state_summary_26, 
       aes(x = state, y = state_mean, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  labs(title = "Posterior State Means: Alcohol Disorder Rate (26+)",
       subtitle = "80% Credible Intervals",
       x = "State", y = "Alcohol Disorder Rate (per 1000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# predictive intervals
new_states <- data.frame(
  state = c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")
)

set.seed(84735)
predictions_26 <- posterior_predict(adr_26_hierarchical, newdata = new_states)

ppc_intervals(
  y    = alc_dis_rate_26 %>% group_by(state) %>% summarise(m = mean(alc_dis_rate)) %>% pull(m),
  yrep = predictions_26,
  prob_outer = 0.80
) + ggplot2::scale_x_continuous(
  labels = new_states$state,
  breaks = 1:nrow(new_states)
) + labs(title = "Posterior Predictive Intervals by State (26+)",
         y = "Alcohol Disorder Rate (per 1000)")

"
AGE GROUP COMPARISONS
"

# combine 3 sets of posterior credible intervals with new "age_group" column
state_summary_all <- bind_rows(
  state_summary_1217 %>% mutate(age_group = "12-17"),
  state_summary_1825 %>% mutate(age_group = "18-25"),
  state_summary_26 %>% mutate(age_group = "26+")
)

# plot clustered point/interval plot
ggplot(state_summary_all,
       aes(x = state, y = state_mean, ymin = .lower, ymax = .upper, color = age_group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Posterior State Means: Alcohol Disorder Rate (across age groups)",
       subtitle = "80% Credible Intervals",
       x = "State", 
       y = "Alcohol Disorder Rate (per 1000)",
       color = "Age Group") +
  theme(axis.text.x = element_text(hjust = 1))

prob_highest_1825 <- adr_1825_hierarchical %>%
  spread_draws(b[term, state], `(Intercept)`) %>%
  mutate(state_mean = `(Intercept)` + b) %>%
  group_by(.draw) %>%
  mutate(is_highest = state_mean == max(state_mean)) %>%
  group_by(state) %>%
  summarise(P_highest = round(mean(is_highest), 3)) %>%
  arrange(desc(P_highest)) %>%
  mutate(state = str_remove(state, "state:")) %>%
  slice_head(n = 5)

kable(prob_highest_1825,
      col.names = c("State", "P(highest)"),
      caption = "P(state has highest rate) — 18–25")

raw_means <- alc_dis_rate_1825 %>%
  group_by(state) %>%
  summarise(raw_mean = mean(alc_dis_rate))

shrinkage_df <- state_summary_1825 %>%
  mutate(state = str_remove(as.character(state), "state:")) %>%
  left_join(raw_means, by = "state")

ggplot(shrinkage_df, aes(y = reorder(state, state_mean))) +
  geom_point(aes(x = raw_mean, color = "Raw"), size = 3) +
  geom_point(aes(x = state_mean, color = "Posterior"), size = 3) +
  geom_segment(aes(x = raw_mean, xend = state_mean, yend = state),
               arrow = arrow(length = unit(0.15, "cm"))) +
  labs(title = "Shrinkage (18–25)",
       x = "Rate per 1,000", y = NULL, color = "") +
  theme(legend.position = "bottom")