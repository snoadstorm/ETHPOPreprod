
# combine LFS and ETHPOP
# to obtain population projections
# with UK born status included
#
# N Green


library(readr)
library(dplyr)


base_year <- 2011

# labour force survey

# load and format
lfs <-
  # read_rds("C:/Users/Nathan/Documents/R/tbinenglanddataclean/output_data/formatted_LFS_2010_2016.rds") %>%
  read_rds("D:/data/formatted_LFS_2010_2012.rds") %>%
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
    CoB = c("UK born", "Non-UK born"),
    age = as.factor(0:90),
              ETH.group = unique(lfs$ETH.group)[!is.na(unique(lfs$ETH.group))],
              sex = c("M", "F"))

lfs <-
  lfs %>%
  merge(all_groups, all.y = TRUE)

# without UK born to compare directly
# with ETHPOP population
dat_lfs_total <-
  lfs %>%
  group_by(sex, age, ETH.group) %>%
  summarise(pop = sum(pop, na.rm = TRUE))

# include proportion UK born
# in each age, ethnic group, sex
dat_lfs <-
  lfs %>%
  group_by(age, ETH.group, sex) %>%
  mutate(sum = sum(pop, na.rm = TRUE)) %>%
  group_by(CoB, add = TRUE) %>%
  mutate(p_CoB = pop/sum) %>%
  ungroup() %>%
  arrange(ETH.group, sex, age)


# impute missing values with logistic regression ---------------------------

regn_dat <-
  dat_lfs %>%
  mutate("sum-pop" = sum - pop,
         age = as.factor(age)) %>%
  filter(CoB == "UK born")

res <- cbind(regn_dat$pop,
             regn_dat$`sum-pop`)

fit <- glm(res ~ ETH.group + age + sex,
           data = regn_dat,
           family = binomial("logit"))

all_agesexeth <-
  expand.grid(age = as.factor(0:90),
              ETH.group = unique(lfs$ETH.group)[!is.na(unique(lfs$ETH.group))],
              sex = c("M", "F"))

pred <- predict(fit, newdata = all_agesexeth, type = "response")

all_agesexeth$`UK born` <- pred
all_agesexeth$`Non-UK born` <- 1 - pred

pred_lfs <-
  reshape2::melt(all_agesexeth,
                 measure.vars = c("UK born", "Non-UK born"),
                 variable.name = "CoB",
                 value.name = "pred")

dat_lfs <-
  pred_lfs %>%
  merge(dat_lfs, all.x = TRUE) %>%
  mutate(est_pop = round(sum*pred, 0))


# ETHPOP ------------------------------------------------------------------

# load and format
dat_pop <-
  read_csv("~/R/cleanETHPOP/output_data/clean_pop_Leeds2.csv",
           col_types = list(sex = col_character(),
                            age = col_double(),
                            year = col_double())) %>%
  filter(year == base_year)                                            # single base year cohort

# prep for LFS join
dat_pop <-
  dat_pop %>%
  mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),       # harmonise ethgrp with LFS
                            "BLA+BLC+OBL", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                            "WBI+WHO", ETH.group),
         ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                            "CHI+OAS", ETH.group)) %>%
  mutate(age = ifelse(age %in% 90:100, 90, age)) %>%                   # make 90 max single age
  group_by(sex, age, ETH.group) %>%                                    # to match LFS
  summarise(pop = sum(pop)) %>%
  mutate(pop = round(pop, 0))


# join ETHPOP and LFS

# # check populations aggregated
  # dat <-
  # merge(dat_lfs, dat_pop,
  #       by = c("sex", "age", "ETH.group"),
  #       suffixes = c("_lfs", "_eth")) %>%
  # mutate(pop_raw = round(pop_eth*p_CoB, 0),       #
  #        pop_pred = round(pop_eth*pred, 0),
  #        year = base_year) %>%
  # select(year, sex, age, ETH.group, CoB, pop_eth, pop_lfs, pop_raw, pop_pred)

dat <-
  merge(dat_lfs, dat_pop,
        by = c("sex", "age", "ETH.group"),
        suffixes = c("_lfs", "_eth")) %>%
  mutate(pop = round(pop_eth*pred, 0),
         year = base_year) %>%
  select(year, sex, age, ETH.group, CoB, pop)


# save output

write.csv(dat, file = "output_data/joined_ETHPOP_LFS_2011.csv")
