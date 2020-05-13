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


run_model <- function(dat_pop,
                      dat_births,
                      dat_deaths,
                      dat_inflow,
                      dat_outflow,
                      year0 = min(dat_pop$year)) {

  years <- sort(unique(dat_pop$year))

  res <- vector("list", length(years))
  names(res) <- years

  pop <- filter(dat_pop, year == year0) %>% select(-X1)
  res[[as.character(year0)]] <- pop

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


# newborn population at age 0
add_births <- function(pop, dat_births) {

  if (any(pop$age == 0)) stop("Shouldn't be any 0 aged in population data")

  xx <-
    dat_births %>%
    select(-X1) %>%
    mutate(age = 0) %>%
    filter(year == pop$year[1],
           ETH.group %in% unique(pop$ETH.group),
           sex %in% unique(pop$sex)) %>%
    rename(pop = births) %>%
    rbind.data.frame(pop) %>%
    arrange(year, ETH.group, sex, age)
}

# decrease population
add_deaths <- function(dat_pop, dat_deaths) {

  dat_deaths %>%
    select(-agegrp, -X1) %>%
    merge(dat_pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - deaths) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-deaths)
}

# inmigration population
add_inflow <- function(dat_pop, dat_inflow) {

  dat_inflow %>%
    select(-X1) %>%
    merge(dat_pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop + inmigrants) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-inmigrants)
}

# outmigration population
add_outflow <- function(dat_pop, dat_outflow) {

  dat_outflow %>%
    select(-X1) %>%
    merge(dat_pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - outmigrants) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-outmigrants)
}

# increment everyone by one year
age_population <- function(pop) {

  pop %>%
    mutate(age = age + 1,
           year = year + 1)
}
