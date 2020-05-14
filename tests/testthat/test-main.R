
library(testthat)
library(purrr)
library(readr)
library(dplyr)
library(ETHPOPreprod)

dat_pop0 <- read_csv("~/R/cleanETHPOP/output_data/clean_pop.csv")
dat_inflow <- read_csv("~/R/cleanETHPOP/output_data/clean_inmigrants.csv")
dat_outflow <- read_csv("~/R/cleanETHPOP/output_data/clean_outmigrants.csv")
dat_births <- read_csv("~/R/cleanETHPOP/output_data/clean_births.csv")
dat_deaths <- read_csv("~/R/cleanETHPOP/output_data/clean_deaths.csv")

# subset population data
dat_pop <-
  dat_pop0 %>%
  filter(year %in% 2011:2012,
         sex == "M",
         ETH.group == "BAN") %>%
  arrange(year, age)


test_that("age population", {

  res <-
    run_model(dat_pop)

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

  res <-
    run_model(dat_pop,
              dat_births = dat_births)

  births2012 <-
    dat_births %>%
    filter(year == 2012,
           sex == "M",
           ETH.group == "BAN") %>%
    select(births)

  # direct value
  expect_true(
    res$`2012`$pop[res$`2012`$age == 0] == births2012)

})

test_that("deaths", {

  res <-
    run_model(dat_pop,
              dat_deaths = dat_deaths)

  deaths2012 <-
    dat_deaths %>%
    filter(year == 2012,
           sex == "M",
           age == 20,
           ETH.group == "BAN") %>%
    select(deaths)

  # one year younger minus direct death value
  expect_true(
    res$`2012`$pop[res$`2012`$age == 20] ==
      (res$`2011`$pop[res$`2011`$age == 19] - deaths2012))

})

test_that("inflow", {

  res <-
    run_model(dat_pop,
              dat_inflow = dat_inflow)

  inflow2012 <-
    dat_inflow %>%
    filter(year == 2012,
           sex == "M",
           age == 20,
           ETH.group == "BAN") %>%
    select(inmigrants)

  # one year younger plus direct inflow value
  expect_true(
    res$`2012`$pop[res$`2012`$age == 20] ==
      (res$`2011`$pop[res$`2011`$age == 19] + inflow2012))
})

test_that("outflow", {

  res <-
    run_model(dat_pop,
              dat_outflow = dat_outflow)

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
      (res$`2011`$pop[res$`2011`$age == 19] - outflow2012))
})


# total data set
dat_pop <- dat_pop0

test_that("age population", {

  res <-
    run_model(dat_pop)

  nsex <- length(unique(dat_pop$sex)) # 2
  neth <- length(unique(dat_pop$ETH.group)) # 12

  # everyone shift up one year with no new births
  # for 50 years and each subgroup
  expect_true(
    nrow(res$`2011`) == nrow(res$`2061`) + (nsex*neth*50))

  # same size pop one year older each year
  expect_true(
    all(res$`2011`$pop[res$`2011`$age == 1] == res$`2012`$pop[res$`2012`$age == 2]))

  expect_true(
    all(res$`2012`$pop[res$`2012`$age == 1] == res$`2022`$pop[res$`2022`$age == 11]))

  expect_true(
    all(res$`2011`$pop[res$`2011`$age == 1] == res$`2061`$pop[res$`2061`$age == 51]))


  # age 100 is 100 + 101
  # i.e. >= 100

  pop_99to100 <-
    res$`2011`[res$`2011`$age %in% 99:100, ] %>%
    group_by(ETH.group, sex) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()

  expect_equivalent(
    pop_99to100$pop, res$`2012`$pop[res$`2012`$age == 100])

  pop_50to100 <-
    res$`2011`[res$`2011`$age %in% 50:100, ] %>%
    group_by(ETH.group, sex) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()

  expect_equivalent(
    pop_50to100$pop, res$`2061`$pop[res$`2061`$age == 100])

})











# plot(res$`2022`$pop, type = "l")
# map(res, function(x) lines(x$pop, type = "l"))
