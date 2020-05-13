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


# model without UK-born/non UK-born
##TODO:

# load ONS starting population
dat_ons <- read_ONS_census2011()

# load ETHPOP cleaned data
dat_pop <- read_csv("~/R/cleanETHPOP/output_data/clean_pop.csv")
dat_inflow <- read_csv("~/R/cleanETHPOP/output_data/clean_inmigrants.csv")
dat_outflow <- read_csv("~/R/cleanETHPOP/output_data/clean_outmigrants.csv")
dat_births <- read_csv("~/R/cleanETHPOP/output_data/clean_births.csv")
dat_deaths <- read_csv("~/R/cleanETHPOP/output_data/clean_deaths.csv")


years <- sort(unique(dat_pop$year))
year0 <- 2011

# subset population data
dat_pop <-
  dat_pop %>%
  filter(year == year0,
         sex == "M",
         ETH.group == "BAN")


res <- vector("list", length(years))
names(res) <- years

pop <- filter(dat_pop, year == year0)
res[[year0]] <- pop

for (i in years[-1]) {

  pop <-
    pop %>%
    age_population() %>%
    add_births(dat_births) %>%
    add_deaths(dat_deaths)

  res[[i]] <- pop
}


# functions ---------------------------------------------------------------

# newborn population at age 0
add_births <- function(dat_pop, dat_births) {

  if (any(dat_pop$age == 0)) stop("Shouldn't be any 0 aged in population data")

  dat_births %>%
    mutate(age = 0) %>%
    rename(pop = births) %>%
    rbind.data.frame(dat_pop) %>%
    arrange("year", "ETH.group",  "sex", "age")
}

# decrease population
add_deaths <- function(dat_pop, dat_deaths) {

  merge(dat_pop,
        dat_deaths,
        by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - deaths) %>%
    select(-deaths)
}

# increment everyone by one year
age_population <- function(pop) {

  pop %>%
    mutate(age = age + 1,
           year = year + 1)
}
