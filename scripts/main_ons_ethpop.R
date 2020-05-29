
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


# load joined LFS and ETHPOP formatted data
# original data from where?
dat_pop <-
  read_csv("~/R/demoSynthPop/output_data/clean_census2011.csv",
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



# explicitly define sex so not coerced to logical
dat_inflow <- read_csv("~/R/cleanETHPOP/output_data/clean_inmigrants_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_outflow <- read_csv("~/R/cleanETHPOP/output_data/clean_outmigrants_Leeds2.csv",
                        col_types = list(sex = col_character()))
dat_births <- read_csv("~/R/cleanETHPOP/output_data/clean_births_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_deaths <- read_csv("~/R/cleanETHPOP/output_data/clean_deaths_Leeds2.csv",
                       col_types = list(sex = col_character()))


# harmonise ETHPOP with initial population -------------------------------

dat_inflow <-
  dat_inflow %>%
  mutate(ETH.group =
           case_when(
             ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
             ETH.group %in% c("WBI","WHO") ~ "White",
             ETH.group == "BAN" ~ "Bangladeshi",
             ETH.group == "BLA" ~ "Black-African",
             ETH.group == "BLC" ~ "Black-Caribbean",
             ETH.group == "OBL" ~ "Black-Other",
             ETH.group == "CHI" ~ "Chinese",
             ETH.group == "IND" ~ "Indian",
             ETH.group == "PAK" ~ "Pakistan"),
         age = ifelse(age %in% 85:100, 85, age)) %>%             # make 90 max single age
  group_by(sex, age, ETH.group, year) %>%
  summarise(inmigrants = sum(inmigrants)) %>%
  mutate(CoB = "Non-UK born")

dat_outflow <-
  dat_outflow %>%
  mutate(ETH.group =
           case_when(
             ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
             ETH.group %in% c("WBI","WHO") ~ "White",
             ETH.group == "BAN" ~ "Bangladeshi",
             ETH.group == "BLA" ~ "Black-African",
             ETH.group == "BLC" ~ "Black-Caribbean",
             ETH.group == "OBL" ~ "Black-Other",
             ETH.group == "CHI" ~ "Chinese",
             ETH.group == "IND" ~ "Indian",
             ETH.group == "PAK" ~ "Pakistan"),
         age = ifelse(age %in% 85:100, 85, age)) %>%
  group_by(sex, age, ETH.group, year) %>%
  summarise(outmigrants = sum(outmigrants)) %>%
  ungroup() %>%
  mutate(`UK born` = outmigrants/2,                   # assume 50/50 between UK born/Non-UK born
         `Non-UK born` = outmigrants/2) %>%
  reshape2::melt(measure.vars = c("UK born", "Non-UK born"),
                 id.vars = c("sex", "age", "ETH.group", "year"),
                 variable.name = "CoB",
                 value.name = "outmigrants") %>%
  as_tibble()

dat_births <-
  dat_births %>%
  mutate(ETH.group =
           case_when(
             ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
             ETH.group %in% c("WBI","WHO") ~ "White",
             ETH.group == "BAN" ~ "Bangladeshi",
             ETH.group == "BLA" ~ "Black-African",
             ETH.group == "BLC" ~ "Black-Caribbean",
             ETH.group == "OBL" ~ "Black-Other",
             ETH.group == "CHI" ~ "Chinese",
             ETH.group == "IND" ~ "Indian",
             ETH.group == "PAK" ~ "Pakistan")) %>%
  group_by(sex, ETH.group, year) %>%
  summarise(births = sum(births)) %>%
  mutate(CoB = "UK born")

dat_deaths <-
  dat_deaths %>%
  mutate(ETH.group =
           case_when(
             ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
             ETH.group %in% c("WBI","WHO") ~ "White",
             ETH.group == "BAN" ~ "Bangladeshi",
             ETH.group == "BLA" ~ "Black-African",
             ETH.group == "BLC" ~ "Black-Caribbean",
             ETH.group == "OBL" ~ "Black-Other",
             ETH.group == "CHI" ~ "Chinese",
             ETH.group == "IND" ~ "Indian",
             ETH.group == "PAK" ~ "Pakistan"),
         age = ifelse(age %in% 85:100, 85, age)) %>%
  group_by(sex, age, ETH.group, year) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(`UK born` = deaths/2,                   # assume 50/50 between UK born/Non-UK born
         `Non-UK born` = deaths/2) %>%
  reshape2::melt(measure.vars = c("UK born", "Non-UK born"),
                 id.vars = c("sex", "age", "ETH.group", "year"),
                 variable.name = "CoB",
                 value.name = "deaths") %>%
  as_tibble()


# -------------------------------------------------------------------------

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

