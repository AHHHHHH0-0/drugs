library(tidyverse)
library(ggplot2)
library(dplyr)
library(bayesrules)
library(bayesplot)
library(rstanarm)

#loading in initial data csv
drug_data <- read.csv("data/drugs.csv")

#filtering for age ranges and 5 biggest states by population
alc_dis_rate_1217 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.12.17`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania"))

alc_dis_rate_1825 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.18.25`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania"))

alc_dis_rate_26 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.26.`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania"))

"
AGE GROUP 1: 12-17
"



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
  prior_intercept = (),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter=5000*2, seed=84375, refresh=FALSE)

# diagnostic plots
mcmc_trace(adr_1825_hierarchical)
mcmc_dens_overlay(adr_1825_hierarchical)
mcmc_acf(adr_1825_hierarchical)

pp_check(adr_1825_hierarchical) + 
  xlab("Alcohol Disorder Rate")

# credible intervals
state_summary <- adr_1825_hierarchical %>%
  spread_draws(b[term, state], `(Intercept)`) %>%
  filter(grepl("state", term) | term == "(Intercept)") %>%
  mutate(state_mean = `(Intercept)` + b) %>%
  group_by(state) %>%
  mean_qi(state_mean, .width = 0.80) %>%
  mutate(state = fct_reorder(state, state_mean))

ggplot(state_summary, 
       aes(x = state, y = state_mean, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  labs(title = "Posterior State Means: Alcohol Disorder Rate (18-25)",
       subtitle = "80% Credible Intervals",
       x = "State", y = "Alcohol Disorder Rate (%)")

# predictive intervals
new_states <- data.frame(
  state = c("California", "Texas", "Florida", "New York", "Pennsylvania")
)

set.seed(84735)
predictions_1825 <- posterior_predict(adr_1825_hierarchical, newdata = new_states)

ppc_intervals(
  y    = alc_dis_rate_1825 %>% group_by(state) %>% summarise(m = mean(alc_dis_rate)) %>% pull(m),
  yrep = predictions_1825,
  prob_outer = 0.80
) +
  ggplot2::scale_x_continuous(
    labels = new_states$state,
    breaks = 1:nrow(new_states)
  ) +
  labs(title = "Posterior Predictive Intervals by State (18-25)",
       y = "Alcohol Disorder Rate (%)")


"
AGE GROUP 3: 26+
"




"
AGE GROUP COMPARISONS
"