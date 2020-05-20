
# combine LFS and ETHPOP
# to obtain population projections
# with UK born status included
#
# N Green


# labour force survey

dat_lfs <-
  read_rds("C:/Users/Nathan/Documents/R/tbinenglanddataclean/output_data/formatted_LFS_2011_2016.rds") %>%
  filter(
    Year == 2011,
    Country == "England") %>%
  mutate(CoB = forcats::fct_explicit_na(CoB)) %>%
  group_by(Age, CoB, ethgrp, Sex) %>%
  summarise(pop = sum(Weight, na.rm = TRUE)) %>%
  na.omit()

dat_lfs <-
  dat_lfs %>%
  group_by(Age, ethgrp, Sex) %>%
  mutate(sum = sum(pop)) %>%
  group_by(CoB, add=TRUE) %>%
  mutate(pop/sum) %>%
  ungroup() %>%
  arrange(ethgrp, Sex, Age) %>%
  mutate(Sex = ifelse(Sex == "Female", "F", "M"),
         ethgrp = ifelse(ethgrp == "Asian", "CHI+OAS", ethgrp),
         ethgrp = ifelse(ethgrp == "Bangladeshi", "BAN", ethgrp),
         ethgrp = ifelse(ethgrp == "Indian", "IND", ethgrp),
         ethgrp = ifelse(ethgrp == "Pakistani", "PAK", ethgrp),
         ethgrp = ifelse(ethgrp == "White", "WBI+WHO", ethgrp),
         ethgrp = ifelse(ethgrp == "Mixed", "MIX", ethgrp),
         ethgrp = ifelse(ethgrp == "Other", "OTH", ethgrp),
         ethgrp = ifelse(ethgrp == "Black/Black British", "BLA+BAN+BLC+OBL", ethgrp)) %>%
  rename(sex = Sex,
         age = Age,
         ETH.group = ethgrp) %>%
  mutate(age = ifelse(age == "90+", 90, age))


# ETHPOP

dat_pop <-
  read_csv("~/R/cleanETHPOP/output_data/clean_pop_Leeds2.csv",
           col_types = list(sex = col_character(),
                            age = col_double(),
                            year = col_double())) %>%
  mutate(ETH.group = ifelse(ETH.group %in% c("BAN","BAN","BLC","OBL"), "BLA+BAN+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"), "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"), "CHI+OAS", ETH.group)) %>%
  mutate(age = ifelse(age %in% 90:100, 90, age)) %>%
  group_by(sex, age, ETH.group, year) %>%
  summarise(pop = sum(pop))


# join ETHPOP and LFS

dat <- merge(dat_lfs, dat_pop,
             by = c("sex", "age", "ETH.group", "year"))
