
library(testthat)
library(purrr)
library(readr)
library(dplyr)
library(demoSynthPop)

dat_pop0 <- read_csv("C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/clean_pop_Leeds2.csv")

##TODO: not per capita rates
# dat_inflow <- read_csv("C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/clean_inmigrants_Leeds2.csv",
#                        col_types = list(sex = col_character()))

dat_outflow <-
  read_csv("C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/outmigrants_per_capita_Leeds2.csv",
           col_types = list(sex = col_character())) %>%
  rename(outmigrants = outmigrants_per_capita)

dat_births <- read_csv("C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/births_per_capita_Leeds1.csv",
                       col_types = list(sex = col_character()))

dat_deaths <-
  read_csv("C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/deaths_per_capita_Leeds1.csv",
           col_types = list(sex = col_character())) %>%
  rename(deaths = deaths_per_capita)


# subset population data
dat_pop <-
  dat_pop0 %>%
  filter(year %in% 2011:2012,
         sex == "M",
         ETH.group == "BAN") %>%
  arrange(year, age)


is_prop <- TRUE

test_that("age population", {

  res <-
    run_model(dat_pop, is_prop = is_prop)

  expect_type(res, type = "list")

  expect_true(all(names(res) == 2011:2012))

  expect_true(all(names(res[[1]]) == c("age", "ETH.group", "sex", "pop", "year")))

  # everyone shift up one year with no new births
  expect_true(
    nrow(res$`2011`) == nrow(res$`2012`) + 1)

  # same size pop one year older
  expect_true(
    all(res$`2011`$pop[1:99] == res$`2012`$pop[1:99]))

  expect_true(
    all(res$`2011`$age[1:99] + 1 == res$`2012`$age[1:99]))

  # age 100 is 100 + 101
  # i.e. >= 100

  expect_true(
    sum(res$`2011`$pop[100:101]) == res$`2012`$pop[100])

})


test_that("births", {

  # subset population data
  dat_popF <-
    dat_pop0 %>%
    filter(year %in% 2011:2012,
           sex == "F",
           ETH.group == "BAN") %>%
    arrange(year, age)

  res <-
    run_model(dat_pop = dat_popF,
              dat_births = dat_births,
              is_prop = is_prop)

  expect_type(res, type = "list")

  expect_true(all(names(res) == 2011:2012))

  expect_true(all(names(res[[1]]) == c("age", "ETH.group", "sex", "pop", "year")))

  births2012 <-
    dat_births %>%
    filter(year == 2012,
           sex == "F",
           ETH.group == "BAN") %>%
    select(births)

  # direct value
  expect_true(
    res$`2012`$pop[res$`2012`$age == 0] == births2012)

})

test_that("deaths", {

  res <-
    run_model(dat_pop,
              dat_deaths = dat_deaths,
              is_prop = is_prop)

  expect_type(res, type = "list")

  expect_true(all(names(res) == 2011:2012))

  expect_true(all(names(res[[1]]) == c("age", "ETH.group", "sex", "pop", "year")))

  deaths2012 <-
    dat_deaths %>%
    filter(year == 2012,
           sex == "M",
           age == 20,
           ETH.group == "BAN") %>%
    select(deaths)


  # add deaths only
  # define function
  add_deaths <- rm_pop(deaths, is_prop = FALSE)
  xx <- dat_pop[dat_pop$year == 2011, ] %>% add_deaths(dat_deaths)

  expect_equal(xx$pop[xx$age == 0], 4842, tolerance = 1)
  expect_equal(xx$pop[xx$age == 9], 4804, tolerance = 1 )


  # one year younger minus direct death value
  expect_true(
    res$`2012`$pop[res$`2012`$age == 20] ==
      (res$`2011`$pop[res$`2011`$age == 19]*(1 - deaths2012)))

  deaths100 <-
    dat_deaths %>%
    filter(year == 2012,
           sex == "M",
           age == 100,
           ETH.group == "BAN") %>%
    select(deaths)

  # one year younger minus direct death value
  # and previous >= 100 year old
  ((res$`2011`$pop[res$`2011`$age == 99] + res$`2011`$pop[res$`2011`$age == 100]) * (1 - deaths100)) %>%
    unlist() %>%
    expect_equivalent(res$`2012`$pop[res$`2012`$age == 100])

})

test_that("inflow", {

  ##TODO:
})

test_that("outflow", {

  res <-
    run_model(dat_pop,
              dat_outflow = dat_outflow,
              is_prop = is_prop)

  expect_type(res, type = "list")

  expect_true(all(names(res) == 2011:2012))

  expect_true(all(names(res[[1]]) == c("age", "ETH.group", "sex", "pop", "year")))

  outflow2012 <-
    dat_outflow %>%
    filter(year == 2012,
           sex == "M",
           age == 20,
           ETH.group == "BAN") %>%
    select(outmigrants)

  # one year younger minus direct outflow value
  expect_true(
    res$`2012`$pop[res$`2012`$age == 20] ==
      (res$`2011`$pop[res$`2011`$age == 19]*(1 - outflow2012)))

  outflow100 <-
    dat_outflow %>%
    filter(year == 2012,
           sex == "M",
           age == 100,
           ETH.group == "BAN") %>%
    select(outmigrants)

  expect_true(
    res$`2012`$pop[res$`2012`$age == 100] ==
      ((res$`2011`$pop[res$`2011`$age == 99] + res$`2011`$pop[res$`2011`$age == 100])*(1 - outflow100))
  )
})

