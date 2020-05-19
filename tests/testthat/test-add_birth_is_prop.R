
library(testthat)
library(purrr)
library(readr)
library(dplyr)
library(demoSynthPop)

# load data
dat_pop <- read_csv("~/R/cleanETHPOP/output_data/clean_pop_Leeds2.csv")
dat_births <- read_csv("~/R/cleanETHPOP/output_data/births_per_capita_Leeds1.csv", col_types = list(sex = col_character()))

pop <- filter(dat_pop, year == 2011) %>% select(-X1)

# recreate the population numbers that were originally use to
# get the proportions in the first place
#
test_that("counts from per capita", {

  xx <-
    pop %>%
    filter(sex == "F",          # childbearing aged women only
           age >= 15,           # could choose other denominator
           age <= 45) %>%
    group_by(year, ETH.group) %>%
    summarise(pop = sum(pop)) %>%
    ungroup() %>%
    merge(dat_births,                       # duplicate poo for each sex
          by = c("year", "ETH.group"),
          all.x = TRUE) %>%
    mutate(births2 = births_per_capita_15_45*pop)

  expect_equivalent(xx$births, xx$births2)
})
