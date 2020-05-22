
# combine LFS and ETHPOP
# to obtain population projections
# with UK born status included
#
# N Green


library(readr)
library(dplyr)


base_year <- 2011

# labour force survey

# load and clean
lfs <-
  read_rds("C:/Users/Nathan/Documents/R/tbinenglanddataclean/output_data/formatted_LFS_2011_2016.rds") %>%
  filter(
    Year == base_year,
    Country == "England") %>%                       # single year cohort
  mutate(CoB = forcats::fct_explicit_na(CoB)) %>%   # missing data category
  group_by(Age, CoB, ethgrp, Sex) %>%
  summarise(pop = sum(Weight, na.rm = TRUE)) %>%    # aggregate population counts
  ungroup() #%>%
  # na.omit()

# harmonise sex, age and ethgrp
# in order to join with ETHPOP
lfs <-
  lfs %>%
  mutate(Sex = ifelse(Sex == "Female", "F", "M"),
         ethgrp = ifelse(ethgrp == "Asian", "CHI+OAS", ethgrp),
         ethgrp = ifelse(ethgrp == "Bangladeshi", "BAN", ethgrp),
         ethgrp = ifelse(ethgrp == "Indian", "IND", ethgrp),
         ethgrp = ifelse(ethgrp == "Pakistani", "PAK", ethgrp),
         ethgrp = ifelse(ethgrp == "White", "WBI+WHO", ethgrp),
         ethgrp = ifelse(ethgrp == "Mixed", "MIX", ethgrp),
         ethgrp = ifelse(ethgrp == "Other", "OTH", ethgrp),
         ethgrp = ifelse(ethgrp == "Black/Black British",
                         "BLA+BAN+BLC+OBL", ethgrp),
         Age = as.character(Age),                   # otherwise factor conversion: age 0 => 1
         Age = ifelse(Age == "90+", "90", Age),
         Age = as.numeric(Age)) %>%
  rename(sex = Sex,
         age = Age,
         ETH.group = ethgrp)


# without UK born to compare directly
# with ETHPOP population
dat_lfs_total <-
  lfs %>%
  group_by(sex, age, ETH.group) %>%
  summarise(pop = sum(pop, na.rm = TRUE))

# proportion UK born
# in each age, ethnic group, sex
dat_lfs <-
  lfs %>%
  group_by(age, ETH.group, sex) %>%
  mutate(sum = sum(pop, na.rm = TRUE)) %>%
  group_by(CoB, add = TRUE) %>%
  mutate(p_CoB = pop/sum) %>%
  ungroup() %>%
  arrange(ETH.group, sex, age)


# ETHPOP

# load and clean
dat_pop <-
  read_csv("~/R/cleanETHPOP/output_data/clean_pop_Leeds2.csv",
           col_types = list(sex = col_character(),
                            age = col_double(),
                            year = col_double())) %>%
  filter(year == base_year)                                            # single base year cohort

# prep for LFS join
dat_pop <-
  dat_pop %>%
  mutate(ETH.group = ifelse(ETH.group %in% c("BAN","BAN","BLC","OBL"), # harmonise ethgrp with LFS
                            "BLA+BAN+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                            "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                            "CHI+OAS", ETH.group)) %>%
  mutate(age = ifelse(age %in% 90:100, 90, age)) %>%                   # make 90 max single age
  group_by(sex, age, ETH.group) %>%                                    # to match LFS
  summarise(pop = sum(pop))


# join ETHPOP and LFS

## check populations

dat <- merge(dat_lfs_total, dat_pop,
             by = c("sex", "age", "ETH.group"))


dat <- merge(dat_lfs, dat_pop,
             by = c("sex", "age", "ETH.group"))
