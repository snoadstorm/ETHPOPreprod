
library(testthat)
library(purrr)
library(readr)
library(dplyr)
library(demoSynthPop)

dat_pop0 <- read_csv("C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/clean_pop_Leeds2.csv")
dat_inflow <- read_csv("C:/Users/Nathan/Documents//R/cleanETHPOP/output_data/clean_inmigrants_Leeds2.csv")
dat_outflow <- read_csv("C:/Users/Nathan/Documents//R/cleanETHPOP/output_data/clean_outmigrants_Leeds2.csv")
dat_births <- read_csv("C:/Users/Nathan/Documents//R/cleanETHPOP/output_data/clean_births_Leeds2.csv")
dat_deaths <- read_csv("C:/Users/Nathan/Documents//R/cleanETHPOP/output_data/clean_deaths_Leeds2.csv")


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

  expect_true(all(names(res) == 2011:2012))

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
    run_model(dat_pop = dat_pop,
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

  deaths100 <-
    dat_deaths %>%
    filter(year == 2012,
           sex == "M",
           age == 100,
           ETH.group == "BAN") %>%
    select(deaths)

  # one year younger minus direct death value
  # and previous >= 100 year old
  expect_true(
    res$`2012`$pop[res$`2012`$age == 100] ==
      (res$`2011`$pop[res$`2011`$age == 99] + res$`2011`$pop[res$`2011`$age == 100] - deaths100)
    )
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

  inflow100 <-
    dat_inflow %>%
    filter(year == 2012,
           sex == "M",
           age == 100,
           ETH.group == "BAN") %>%
    select(inmigrants)

  expect_true(
    res$`2012`$pop[res$`2012`$age == 100] ==
      (res$`2011`$pop[res$`2011`$age == 99] + res$`2011`$pop[res$`2011`$age == 100] - inflow100)
  )
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

  outflow100 <-
    dat_outflow %>%
    filter(year == 2012,
           sex == "M",
           age == 100,
           ETH.group == "BAN") %>%
    select(outmigrants)

  expect_true(
    res$`2012`$pop[res$`2012`$age == 100] ==
      (res$`2011`$pop[res$`2011`$age == 99] + res$`2011`$pop[res$`2011`$age == 100] - outflow100)
  )
})



# -------------------------------------------------------------------------


dat_pop <- dat_pop0

test_that("age population total data", {

  res <-
    run_model(dat_pop = dat_pop)

  nsex <- length(unique(dat_pop$sex)) # 2
  neth <- length(unique(dat_pop$ETH.group)) # 12

  # set of ethnic group names each year
  ethgroups_each_year <- map(map(res, "ETH.group"), ~sort(unique(.x)))

  expect_true(all(map_lgl(ethgroups_each_year,
                          ~ all(.x == c("BAN", "BLA", "BLC", "CHI", "IND", "MIX", "OAS", "OBL", "OTH", "PAK", "WBI", "WHO")))))

  # everyone shift up one year with no new births
  # and there's absorbing in >= 100 years old
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


test_that("births total data", {

  res <-
    run_model(dat_pop = dat_pop,
              dat_births = dat_births)

  # set of ethnic group names each year
  ethgroups_each_year <- map(map(res, "ETH.group"), ~sort(unique(.x)))

  expect_true(all(map_lgl(ethgroups_each_year,
                          ~ all(.x == c("BAN", "BLA", "BLC", "CHI", "IND", "MIX", "OAS", "OBL", "OTH", "PAK", "WBI", "WHO")))))

  # all births for sex and ethnic groups in 2012
  births2012 <-
    dat_births %>%
    filter(year == 2012,
           sex != "person") %>%
    arrange(sex, ETH.group) %>%
    select(births) %>%
    unlist()

  # direct value
  expect_equivalent(
    res$`2012`[res$`2012`$age == 0, ] %>%
      arrange(sex, ETH.group) %>%
      select(pop) %>%
      unlist(), births2012)
})








# plot(res$`2022`$pop, type = "l")
# map(res, function(x) lines(x$pop, type = "l"))
