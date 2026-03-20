# Stats 115 Final Project: Drug Use in Young Adults

**Group 14:** Melody Fang, Andrew Liu, Hasnain Sizar, Tyler Tran

## Overview
This project analyzes Alcohol Usage Disorder (AUD) rates in the US across different states using Bayesian hierarchical models. For the sake of simplicity, we examine only the top 10 states by population, and we compare hierarchical models across three age groups:

- 12–17
- 18–25
- 26+

## Questions
- Across different age groups, which state has the highest rate of alcohol usage disorder?
- Which age group has the overall highest rate?

## Data
- **Source:** NSDUH / SAMHSA
- **Time range:** 2002–2018
- **Units:** Rates per 1,000 people
- **States used in analysis:** 10 most populous U.S. states

## Method
We fit three separate hierarchical models, one for each age group (12-17, 18-25, and 26+). For each model, we use data from the 10 most populous states, with **state** as the grouping variable and **partial pooling** across states. We assume each age group has a separate normal distribution.

### Priors
- 12–17: `normal(30, 15)`
- 18–25: `normal(140, 30)`
- 26+: `normal(60, 20)`
- Residual SD: `exponential(1, autoscale = TRUE)`
- Between-state variability: `decov(1, 1, 1, 1)`

## Results
The 18–25 age group has the highest AUD rates overall. In this age group, Illinois is the state with the highest rate. 

## Files
- `data/` – dataset and data README
- `presentation_files/` – Quarto presentation files
- `projectcode.R` – modeling and analysis code
