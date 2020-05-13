#
# Recover ETHPOP population data
# using ETHPOP in/out flow data
#
# N Green
#


library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(tibble)


# model UK-born/non UK-born
##TODO:

# load ONS starting population
dat_ons <- read_ONS_census2011()

# load ETHPOP cleaned data
dat_pop <- read_csv("~/R/cleanETHPOP/output_data/clean_pop.csv")
dat_inflow <- read_csv("~/R/cleanETHPOP/output_data/clean_inmigrants.csv")
dat_outflow <- read_csv("~/R/cleanETHPOP/output_data/clean_outmigrants.csv")
dat_births <- read_csv("~/R/cleanETHPOP/output_data/clean_births.csv")
dat_deaths <- read_csv("~/R/cleanETHPOP/output_data/clean_deaths.csv")


res <-
  run_model(dat_pop,
            dat_births,
            dat_deaths,
            dat_inflow,
            dat_outflow)


# functions ---------------------------------------------------------------

# if data = NA then skipped in calculation (identity function)
run_model <- function(dat_pop,
                      dat_births = NA,
                      dat_deaths = NA,
                      dat_inflow = NA,
                      dat_outflow = NA,
                      year0 = min(dat_pop$year)) {

  # sequence of years to estimate for
  years <- sort(unique(dat_pop$year))

  # results list
  res <- vector("list", length(years))
  names(res) <- years

  # starting year population
  pop <- filter(dat_pop, year == year0) %>% select(-X1)
  res[[as.character(year0)]] <- pop

  # loop over years
  for (i in as.character(years[-1])) {

    pop <-
      pop %>%
      age_population() %>%
      add_births(dat_births) %>%
      add_deaths(dat_deaths) %>%
      add_inflow(dat_inflow) %>%
      add_outflow(dat_outflow)

    res[[i]] <- pop

    message("year ", i)
  }

  res
}


# newborn population increase
#
add_births <- function(pop, dat_births) {

  if (any(pop$age == 0)) stop("Shouldn't be any 0 aged in population data")
  if (all(is.na(dat_births))) return(pop)

  dat_births %>%
    select(-X1) %>%
    mutate(age = 0) %>%                # everyone is age 0
    filter(year == pop$year[1],
           ETH.group %in% unique(pop$ETH.group),
           sex %in% unique(pop$sex)) %>%
    rename(pop = births) %>%
    rbind.data.frame(pop) %>%
    arrange(year, ETH.group, sex, age) # sort ages in ascending order
}

# decrease population
#
add_deaths <- function(pop, dat_deaths) {

  if (all(is.na(dat_deaths))) return(pop)

  dat_deaths %>%
    select(-agegrp, -X1) %>%
    merge(pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - deaths) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-deaths) %>%
    as_tibble()
}

# inmigration population increase
#
add_inflow <- function(pop, dat_inflow) {

  if (all(is.na(dat_inflow))) return(pop)

  dat_inflow %>%
    select(-X1) %>%
    merge(pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop + inmigrants) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-inmigrants) %>%
    as_tibble()
}

# outmigration population decrease
#
add_outflow <- function(pop, dat_outflow) {

  if (all(is.na(dat_outflow))) return(pop)

  dat_outflow %>%
    select(-X1) %>%
    merge(pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - outmigrants) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-outmigrants) %>%
    as_tibble()
}

# increment everyone by one year
#
age_population <- function(pop) {

  pop %>%
    mutate(age = ifelse(age < 100, age + 1, age), # age 100 means >=100
           year = year + 1) %>%
    group_by(ETH.group, sex, year, age) %>%
    summarise(pop = sum(pop))             # sum all 100 ages
}

