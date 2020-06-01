
# combine LFS and ETHPOP
# to obtain population projections including:
#
#   Non-UK born
#   time in UK
#
# N Green


library(readr)
library(dplyr)


base_year <- 2011

# labour force survey

# load and format
lfs <-
  read_rds("D:/data/formatted_LFS_2010_2012.rds") %>%
  filter(
    Year == base_year,
    Country == "England") %>%                       # single year cohort
  mutate(CoB = forcats::fct_explicit_na(CoB)) %>%   # missing data category
  group_by(Age, CoB, ethgrp, Sex, timeinUK) %>%
  summarise(pop = sum(Weight, na.rm = TRUE)) %>%    # aggregate population counts
  ungroup() %>%
  filter(CoB == "Non-UK born") %>%
  mutate(`timeinUK(0,1]` = ifelse(timeinUK == "(0,1]", TRUE, FALSE)) %>%
  group_by(Age, ethgrp, Sex, `timeinUK(0,1]`) %>%
  summarise(pop = sum(pop, na.rm = TRUE)) %>%
  ungroup()


# harmonise sex, age and ethgrp
# in order to join with ETHPOP
lfs <-
  lfs %>%
  mutate(Sex = ifelse(Sex == "Female", "F", "M"),
         ethgrp = case_when(
           ethgrp == "Asian" ~ "CHI+OAS",
           ethgrp == "Bangladeshi" ~ "BAN",
           ethgrp == "Indian" ~ "IND",
           ethgrp == "Pakistani" ~ "PAK",
           ethgrp == "White" ~ "WBI+WHO",
           ethgrp == "Mixed" ~ "MIX",
           ethgrp == "Other" ~ "OTH",
           ethgrp == "Black/Black British" ~ "BLA+BLC+OBL",
           TRUE ~ NA_character_),
         Age = as.character(Age),                   # otherwise factor conversion: age 0 => 1
         Age = ifelse(Age == "90+", "90", Age),
         Age = as.numeric(Age)) %>%
  rename(sex = Sex,
         age = Age,
         ETH.group = ethgrp)

all_groups <-
  expand.grid(
    `timeinUK(0,1]` = c(TRUE, FALSE),
    age = as.factor(0:90),
    ETH.group = unique(lfs$ETH.group)[!is.na(unique(lfs$ETH.group))],
    sex = c("M", "F"))

lfs <-
  lfs %>%
  merge(all_groups, all.y = TRUE)


# include proportion timeinUK(0,1]
# in each age, ethnic group, sex
dat_lfs <-
  lfs %>%
  group_by(age, ETH.group, sex) %>%
  mutate(sum = sum(pop, na.rm = TRUE)) %>%
  group_by(`timeinUK(0,1]`, add = TRUE) %>%
  mutate(p_01 = pop/sum) %>%
  ungroup() %>%
  arrange(ETH.group, sex, age)


# impute missing values with logistic regression ---------------------------

regn_dat <-
  dat_lfs %>%
  mutate("sum-pop" = sum - pop,
         # age = as.factor(age)) %>%
         age = as.numeric(as.character(age))) %>%
  filter(`timeinUK(0,1]` == TRUE)

# outcomes
res <- cbind(regn_dat$pop,
             regn_dat$`sum-pop`)

fit <- glm(res ~ ETH.group + age + sex,
           data = regn_dat,
           family = binomial("logit"))

all_agesexeth <-
  expand.grid(ETH.group = unique(lfs$ETH.group)[!is.na(unique(lfs$ETH.group))],
              # age = as.factor(0:90),
              age = as.numeric(0:90),
              sex = c("M", "F"))

pred <- predict(fit, newdata = all_agesexeth, type = "response")

all_agesexeth$`(0,1]` <- pred
all_agesexeth$`(1,100]` <- 1 - pred

pred_lfs <-
  reshape2::melt(all_agesexeth,
                 measure.vars = c("(0,1]", "(1,100]"),
                 variable.name = "timeinUK(0,1]",
                 value.name = "pred") %>%
  mutate(`timeinUK(0,1]` = ifelse(`timeinUK(0,1]` == "(0,1]", TRUE, FALSE)) %>%
  as_tibble()

dat_lfs <-
  pred_lfs %>%
  merge(dat_lfs, all.x = TRUE) %>%
  mutate(est_pop = round(sum*pred, 0),
         CoB = "Non-UK born") %>%
  as_tibble()


# ETHPOP ------------------------------------------------------------------

# load and format
dat_pop <-
  read_csv("output_data/joined_ETHPOP_LFS_2011.csv",
           col_types = list(sex = col_character(),
                            age = col_double(),
                            year = col_double())) %>%
  filter(year == base_year) %>%                          # single base year cohort
  select(-X1)


# join ETHPOP and LFS

dat <-
  merge(dat_lfs, dat_pop,
        by = c("sex", "age", "ETH.group", "CoB"),
        suffixes = c("_lfs", "_eth"), all.y = TRUE) %>%
  mutate(pop = round(pop_eth*pred, 0),
         year = base_year) %>%
  mutate(pop = ifelse(is.na(pop), pop_eth, pop)) %>%
  select(year, sex, age, ETH.group, CoB, `timeinUK(0,1]`, pop) %>%
  as_tibble()


# save output

write.csv(dat, file = "output_data/joined_ETHPOP_LFS_2011_timeinUK.csv")
