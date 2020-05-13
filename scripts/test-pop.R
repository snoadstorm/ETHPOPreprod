#
# unit tests
#


library(testthat)
library(purrr)


dat_pop <- read_csv("~/R/cleanETHPOP/output_data/clean_pop.csv")

# subset population data
dat_pop <-
  dat_pop %>%
  filter(year %in% 2011:2012,
         sex == "M",
         ETH.group == "BAN") %>%
  arrange(year, age)

res <-
  run_model(dat_pop)

# same size pop one year older
expect_true(
  all(res$`2011`$pop[1:99] == res$`2012`$pop[1:99]))

expect_true(
  all(res$`2011`$age[1:99] + 1 == res$`2012`$age[1:99]))

# age 100 is 100 + 101
# i.e. >= 100

expect_true(
  sum(res$`2011`$pop[100:101]) == res$`2012`$pop[100])


# births

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


# deaths
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


# inflow
res <-
  run_model(dat_pop,
            dat_inflow = dat_inflow)


# outflow
res <-
  run_model(dat_pop,
            dat_outflow = dat_outflow)


# plot(res$`2022`$pop, type = "l")
# map(res, function(x) lines(x$pop, type = "l"))
