
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


# original data from where?
dat_pop <-
  read_csv("~/R/demoSynthPop/output_data/clean_census2011.csv",
           col_types = list(sex = col_character(),
                            year = col_double())) %>%
  rename(ETH.group = ethgrp,                                ##TODO: move to cleaning function...
         pop = population) %>%
  mutate(CoB = ifelse(CoB == "UK-born", "UK born", "Non-UK born"),
         age = ifelse(age %in% c("85-89", "90-94", "95-99", "100"), "85", age),
         age = as.numeric(age)) %>%
  group_by(CoB, ETH.group, age, sex, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()


# explicitly define sex column so not coerced to logical
dat_inflow <- read_csv("~/R/cleanETHPOP/output_data/clean_inmigrants_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_outflow <- read_csv("~/R/cleanETHPOP/output_data/clean_outmigrants_Leeds2.csv",
                        col_types = list(sex = col_character()))
dat_births <- read_csv("~/R/cleanETHPOP/output_data/clean_births_Leeds2.csv",
                       col_types = list(sex = col_character()))
dat_deaths <- read_csv("~/R/cleanETHPOP/output_data/clean_deaths_Leeds2.csv",
                       col_types = list(sex = col_character()))


# harmonise ETHPOP with initial population

dat_inflow  <- harmonise_ons_inflow(dat_inflow)
dat_outflow <- harmonise_ons_outflow(dat_outflow)
dat_births  <- harmonise_ons_births(dat_births)
dat_deaths  <- harmonise_ons_deaths(dat_deaths)


res <-
  run_model(dat_pop,
            dat_births,
            dat_deaths,
            dat_inflow,
            dat_outflow,
            n_years = 20,
            max_age = 85)

sim_pop <- bind_rows(res)

write.csv(sim_pop, file = "output_data/run_model_output_ons2011.csv")


########
# plot #
########


sim_plot <-
  sim_pop %>%
  filter(sex == "M",
         ETH.group == "Bangladeshi",
         year %in% c(2011, 2020, 2030)
  ) %>%
  mutate(year = as.factor(year))


ggplot(sim_plot, aes(x=age, y=pop, colour = interaction(year, CoB))) +
  scale_color_manual(values=c(rep("#CC6666",3), rep("#9999CC",3))) +
  geom_line() +
  ylim(0, 11000) + xlim(0,90)


# all ethnic groups by age ----

sim_plot <-
  sim_pop %>%
  filter(sex == "M",
         year %in% c(2011, 2020, 2030)
  ) %>%
  mutate(year = as.factor(year))

ggplot(sim_plot, aes(x=age, y=pop, colour = interaction(year, CoB))) +
  scale_color_manual(values=c(rep("#CC6666",3), rep("#9999CC",3))) +
  geom_line() +
  facet_grid(~ETH.group) +
  ylim(0, 11000) + xlim(0,90)


p <- list()
for (var in unique(sim_plot$ETH.group)) {
  pdat <-
    sim_plot %>%
    filter(ETH.group == var)

  # dev.new()
  p[[var]] <-
    ggplot(pdat, aes(x=age, y=pop, colour = interaction(year, CoB))) +
    geom_line() +
    ggtitle(var) +
    theme_bw() +
    ylim(0, NA) +#11000) +
    xlim(0,90)
}

q <-
  gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]],
                          p[[4]], p[[5]], p[[6]],
                          p[[7]], p[[8]], p[[9]], ncol =3)

ggsave(q, filename = "all_ethgrp_UKborn_age_lines.png", scale = 3)


# UK born/Non-UK born by year ----

sim_plot <-
  sim_pop %>%
  filter(sex == "M"
         # ETH.group == "Bangladeshi"
  ) %>%
  group_by(year, CoB, ETH.group) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year)))

q <-
  ggplot(sim_plot, aes(x=year, y=pop, colour = CoB)) +
  geom_line() +
  facet_wrap(~ETH.group, nrow = 2, scales = "free")

ggsave(q, filename = "ethgrp_UKborn_years_lines.png", scale = 3)


# % UK born over time ----

sim_plot <-
  sim_pop %>%
  # filter(age == 20) %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE),
         pop = ifelse(pop < 0 , 0 ,pop)) %>%                  ##TODO: why negative pop?
  group_by(CoB, ETH.group, agegrp, year) %>%
  summarise(pop = sum(pop)) %>%
  group_by(ETH.group, agegrp, year) %>%
  mutate(sum = sum(pop)) %>%
  ungroup() %>%
  mutate(prop = pop/sum)


sim_plot[sim_plot$agegrp == "[20,25)", ] %>%
  ggplot(aes(x=year, y=prop, colour = CoB)) +
  geom_line() +
  ylim(0, 1) +
  facet_grid(sex~ETH.group)


p <- list()
for (var in unique(sim_plot$agegrp)) {

  dat <- sim_plot[sim_plot$agegrp == var, ]

    p[[var]] <-
      # ggplot(dat, aes(x=year, y=prop, colour = CoB)) +  # proportion UK born/Non-UK born
      ggplot(dat, aes(x=year, y=pop, colour = CoB)) +     # absolute counts
      geom_line() +
      ylim(0, 30000) +
      # facet_grid(sex~ETH.group, scales = "free")
      facet_grid(~ETH.group, scales = "free")

    print(p[[var]] + ggtitle(var))
}
