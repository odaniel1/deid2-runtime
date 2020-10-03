# R implement of the baselinse solution set out in:
# https://github.com/drivendataorg/deid2-runtime/tree/master/references

# NOTE: This is currently a standalone .R file which creates a file
# submission.csv for manual submission during the Open Arena (Stage 1); i.e.
# this does not provide a template for using the full submission pipeline set
# out in the README:
# https://github.com/drivendataorg/deid2-runtime#making-a-submission

library(tidyverse)
library(jsonlite)

params <- fromJSON("./data/parameters.json")

## ---- (Pre-processing) Initialise empty histogram data structure ------------

hist_bins <- crossing(
  params$schema$periods %>% as_data_frame(),
  params$schema$neighborhood %>% as_data_frame() %>% select(neighborhood = code),
  params$schema$incident_type %>% as_data_frame() %>% select(incident_type = code)
)

## ---- (Privatization) Generate histogram from provided data -----------------

dat <- read_csv("./data/incidents.csv")

hist <- dat %>%
  count(year, month, neighborhood, incident_type)

# bind histogram to empty histogram to ensure all possible strata are accounted
# for; fill NA values with 0
hist <- left_join(hist_bins, hist) %>% replace_na(list(n = 0))


## ---- (Privatization) Add Laplace noise -------------------------------------

set.seed(1414214)

# submissions will be required for a number of different runs, which we extract
# from the parameters data provided
runs <- params$runs %>% as_data_frame() %>%
  mutate(
    # see section Privitization and Privacy Proof for a derivation of the
    # appropriate sensitivty and Laplace scale parameter
    sensitivity = max_records_per_individual,
    lambda = sensitivity / epsilon
  )

# for each run, make a copy of the histogram data, and add laplace noise to the
# observed frequencies
runs <- crossing(runs, hist) %>%
  mutate(
    # Laplace distribution can be derived as a difference of two exponential
    # distributions: en.wikipedia.org/wiki/Laplace_distribution#Related_distributions
    laplace_noise = rexp(n(), rate = 1/lambda) - rexp(n(), rate = 1/lambda),
    n_noise = n + laplace_noise
  )

## ---- (Post-Processing) Ensure frequencies are integer valued ---------------

runs <- runs %>%
  mutate(n_noise = pmax(0, round(n_noise, 0)))

## ---- Prepare submission ----------------------------------------------------

# reformat as described in the problem description
sub <- runs %>%
  pivot_wider(
    id_cols = c("epsilon", "neighborhood", "year", "month"),
    names_from = incident_type,
    values_from = n_noise
  ) %>%
  # ensure the rows are ordered correctly for submission
  arrange(epsilon, neighborhood, year, month)

write_csv(sub, "./data/submission.csv")