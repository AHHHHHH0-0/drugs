library(tidyverse)
library(ggplot2)
library(dplyr)
library(bayesrules)
library(bayesplot)
library(rstanarm)

#loading in initial data csv
drug_data <- read.csv("data/drugs.csv")

#filtering for age range 18-25, 5 biggest states by population
alc_dis_rate_1825 <- drug_data %>% 
  select(
    state = State, 
    alc_dis_rate   = `Rates.Alcohol.Use.Disorder.Past.Year.18.25`,
  ) %>% 
  filter(state %in% c("California", "Texas", "Florida", "New York", "Pennsylvania"))

#plotting a boxplot for each state with overall mean
mean_rate <- mean(alc_dis_rate_1825$alc_dis_rate)
ggplot(alc_dis_rate_1825, aes(x = state, y = alc_dis_rate)) +
  geom_boxplot() +
  labs(title = "Alcohol Disorder Rate for Age Range 18-25",
       x = "State",
       y = "Alcohol Disorder Rate") +
  geom_hline(yintercept = mean_rate, color = "red", linetype = "solid")

#hierarchical model
adr_1825_hierarchical <- stan_glmer(
  alc_dis_rate ~ (1 | state),
  data = alc_dis_rate_1825, family = gaussian,
  prior_intercept = (),
  prior_aux = (),
  prior_covariance = (),
  chains = 4, iter=5000*2, seed=84375, refresh=FALSE)

#diagnostic plots
mcmc_trace(adr_1825_hierarchical)
mcmc_dens_overlay(adr_1825_hierarchical)
mcmc_acf(adr_1825_hierarchical)

pp_check(adr_1825_hierarchical) + 
  xlab("Alcohol Disorder Rate")

artist_summary_scaled <- artist_chains %>% 
  select(-`(Intercept)`, -b) %>% 
  mean_qi(.width = 0.80) %>% 
  mutate(artist = fct_reorder(artist, mu_j))

#credible intervals
ggplot(artist_summary_scaled, 
       aes(x = artist, y = mu_j, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  xaxis_text(angle = 90, hjust = 1)

#predictive intervals
set.seed(84735)
predictions_complete <- posterior_predict(spotify_complete_pooled,
                                          newdata = artist_means)

ppc_intervals(artist_means$popularity, yrep = predictions_complete,
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist,
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1)