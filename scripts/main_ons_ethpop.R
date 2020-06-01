
#
# create synthetic cohort
# using joined ONS census and ETHPOP in/out flow data
# including UK born/Non-UK born
#
# N Green


library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(tibble)
library(ggplot2)
library(demoSynthPop)


# # load grouped ONS starting population
# dat_ons <- read_ONS_census2011()


# original data from where?
dat_pop <-
  read_csv("/Users/laurasnoad/Documents/R_Projects/demoSynthPop/output_data/clean_census2011.csv",
           col_types = list(sex = col_character(),
                            year = col_double())) %>%
  rename(ETH.group = ethgrp,                                ##TODO: move to cleaning function...
         pop = population) %>%
  mutate(CoB = ifelse(CoB == "UK-born", "UK born", "Non-UK born"),
         age = ifelse(age %in% c("85-89", "90-94", "95-99", "100"), "85", age),
         age = as.numeric(age)) %>%
  group_by(CoB, ETH.group, age, sex, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()


# explicitly define sex column so not coerced to logical
dat_inflow <- read_csv("/Users/laurasnoad/Documents/R_Projects/cleanETHPOP/output_data/clean_inmigrants_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_outflow <- read_csv("/Users/laurasnoad/Documents/R_Projects/cleanETHPOP/output_data/clean_outmigrants_Leeds2.csv",
                        col_types = list(sex = col_character()))
dat_births <- read_csv("/Users/laurasnoad/Documents/R_Projects/cleanETHPOP/output_data/clean_births_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_deaths <- read_csv("/Users/laurasnoad/Documents/R_Projects/cleanETHPOP/output_data/clean_deaths_Leeds2.csv",
                       col_types = list(sex = col_character()))


# harmonise ETHPOP with initial population

dat_inflow <- harmonise_ons_inflow(dat_inflow)
dat_outflow <- harmonise_ons_outflow(dat_outflow)
dat_births <- harmonise_ons_births(dat_births)
dat_deaths <- harmonise_ons_deaths(dat_deaths)


res <-
  run_model(dat_pop,
            dat_births,
            dat_deaths,
            dat_inflow,
            dat_outflow,
            n_years = 20,
            max_age = 85)

sim_pop <- bind_rows(res)



########
# plot #
########


sim_plot <-
  sim_pop %>%
  filter(sex == "M",
         ETH.group == "Bangladeshi",
         year %in% c(2011, 2020, 2030)
  ) %>%
  mutate(year = as.factor(year))


ggplot(sim_plot, aes(x=age, y=pop, colour = interaction(CoB, year))) +
  geom_line() +
  ylim(0, 11000) + xlim(0,90)

