
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
# original data:...
dat_pop <- read_csv("~/R/demoSynthPop/output_data/clean_census2011.csv",
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


# harmonise ETHPOP with initial population -------------------------------

dat_inflow <-
  dat_inflow %>%
  mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                            "BLA+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                            "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                            "CHI+OAS", ETH.group),
         age = ifelse(age %in% 90:100, 90, age)) %>%             # make 90 max single age
  group_by(sex, age, ETH.group, year) %>%
  summarise(inmigrants = sum(inmigrants)) %>%
  mutate(CoB = "Non-UK born")

dat_outflow <-
  dat_outflow %>%
  mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                            "BLA+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                            "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                            "CHI+OAS", ETH.group),
         age = ifelse(age %in% 90:100, 90, age)) %>%
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
  mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                            "BLA+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                            "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                            "CHI+OAS", ETH.group)) %>%
  group_by(sex, ETH.group, year) %>%
  summarise(births = sum(births)) %>%
  mutate(CoB = "UK born")

dat_deaths <-
  dat_deaths %>%
  mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                            "BLA+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                            "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                            "CHI+OAS", ETH.group),
         age = ifelse(age %in% 90:100, 90, age)) %>%
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
            n_years = 3,
            max_age = 90)

sim_pop <- bind_rows(res)



########
# plot #
########

sim_plot <-
  sim_pop %>%
  filter(sex == "M",
         ETH.group == "BAN",
         year %in% c(2011, 2020, 2030, 2040, 2050, 2060)) %>%
  mutate(year = as.factor(year))
# mutate(eth_sex_year = interaction(ETH.group, sex, year))

dat_plot <-
  dat_pop %>%
  filter(sex == "M",
         ETH.group == "BAN",
         year %in% c(2011, 2020, 2030, 2040, 2050, 2060)) %>%
  mutate(year = as.factor(year))


p1 <-
  ggplot(sim_plot, aes(x=age, y=pop, colour = year)) +
  geom_line() +
  ylim(0,11000)

p2 <-
  ggplot(dat_plot, aes(x=age, y=pop, colour = year)) +
  geom_line() +
  ylim(0,11000)

gridExtra::grid.arrange(p1, p2)


## differences

diff_plot <-
  merge(dat_plot, sim_plot,
        by = c("age", "ETH.group", "sex", "year"), suffixes = c(".eth", ".sim")) %>%
  mutate(diff_pop = pop.eth - pop.sim,
         scaled_diff = diff_pop/pop.eth)

p3 <-
  ggplot(diff_plot, aes(x=age, y=diff_pop, colour = year)) +
  ggtitle("ETHPOP - estimated populations") +
  geom_line()

p3
p3 + ylim(-2000, 1000)

p4 <-
  ggplot(diff_plot, aes(x=age, y=scaled_diff, colour = year)) +
  ggtitle("(ETHPOP - estimated populations)/ETHPOP") +
  geom_line()

p4
p4 + ylim(-2, 3)
