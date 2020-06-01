
# create synthetic cohort
# using joined LFS and ETHPOP in/out flow data
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


# load joined LFS and ETHPOP formatted data
dat_pop <- read_csv("~/R/demoSynthPop/output_data/joined_ETHPOP_LFS_2011.csv",
                    col_types = list(sex = col_character(),
                                     age = col_double(),
                                     year = col_double()))

# explicitly define sex so not coerced to logical
dat_inflow <- read_csv("~/R/cleanETHPOP/output_data/clean_inmigrants_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_outflow <- read_csv("~/R/cleanETHPOP/output_data/clean_outmigrants_Leeds2.csv",
                        col_types = list(sex = col_character()))
dat_births <- read_csv("~/R/cleanETHPOP/output_data/clean_births_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_deaths <- read_csv("~/R/cleanETHPOP/output_data/clean_deaths_Leeds2.csv",
                       col_types = list(sex = col_character()))


# harmonise ETHPOP with initial population

dat_inflow <- harmonise_lfs_inflow(dat_inflow)
dat_outflow <- harmonise_lfs_outflow(dat_outflow,
                                     p_UKborn_outflow = 0.5)
dat_births <- harmonise_lfs_births(dat_births)
dat_deaths <- harmonise_lfs_deaths(dat_deaths)


res <-
  run_model(dat_pop,
            dat_births,
            dat_deaths,
            dat_inflow,
            dat_outflow,
            n_years = 20,
            max_age = 90)

sim_pop <- bind_rows(res)



########
# plot #
########

sim_plot <-
  sim_pop %>%
  filter(sex == "M",
         ETH.group == "BAN",
         year %in% c(2011, 2020, 2030)
         ) %>%
  mutate(year = as.factor(year))


ggplot(sim_plot, aes(x=age, y=pop, colour = interaction(CoB, year))) +
  geom_line() +
  ylim(0,11000)
