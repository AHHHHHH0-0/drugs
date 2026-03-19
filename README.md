# Stats 115 Final Project: Drug Use in Young Adults

**Group 14:** Melody Fang, Andrew Liu, Hasnain Sizar, Tyler Tran

## Overview
This project analyzes Alcohol Use Disorder (AUD) rates using Bayesian hierarchical models. We compare three age groups:

- 12–17
- 18–25
- 26+

Our goal is to identify which states have the highest AUD rates and which age group has the highest overall rate.

## Data
- **Source:** NSDUH / SAMHSA
- **Time range:** 2002–2018
- **Units:** Rates per 1,000 people
- **States used in analysis:** 10 most populous U.S. states

## Method
We fit separate Bayesian hierarchical Gaussian models for each age group, using **state** as the grouping variable and **partial pooling** across states.

### Priors
- 12–17: `normal(30, 15)`
- 18–25: `normal(140, 30)`
- 26+: `normal(60, 20)`
- Residual SD: `exponential(1, autoscale = TRUE)`
- Between-state variability: `decov(1, 1, 1, 1)`

## Main Result
The 18–25 age group has the highest AUD rates overall.

## Files
- `data/` – dataset and data README
- `presentation_files/` – Quarto presentation files
- `projectcode.R` – modeling and analysis code
