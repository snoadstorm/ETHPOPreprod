
# increment everyone by one year
#
age_population <- function(pop,
                           max_age = 100) {

  pop %>%
    mutate(age = ifelse(age < max_age, age + 1, age), # age 100 means >=100
           year = year + 1) %>%
    # group_by(ETH.group, sex, year, age) %>%
    group_by(CoB, ETH.group, sex, year, age) %>%
    summarise(pop = sum(pop)) %>%             # sum all (previous and new) 100 ages
    ungroup()
}


# newborn population increase
#
add_births <- function(pop,
                       dat_births,
                       is_prop = FALSE) {

  if (any(pop$age == 0)) stop("Shouldn't be any 0 aged in population data")
  if (all(is.na(dat_births))) return(pop)

  ##TODO: include UK born in is_prop...
  # counts for eligible population
  if (is_prop) {

    dat_births <-
      pop %>%
      filter(sex == "F",            # childbearing aged women only
             age >= 15,             # could choose other denominator
             age <= 45) %>%
      group_by(year, ETH.group) %>%
      summarise(pop = sum(pop)) %>%
      ungroup() %>%
      merge(dat_births,                       # duplicate pop for each sex
            by = c("year", "ETH.group"),
            all.x = TRUE) %>%
      mutate(births = births_per_capita_15_45*pop) %>%
      select(year, sex, ETH.group, births)
  }

  dat_births %>%
    select_at(vars(-contains("X1"))) %>% # remove column
    mutate(age = 0,                      # everyone is: age 0
           CoB = "UK born") %>%          #              UK born
    filter(year == pop$year[1],
           ETH.group %in% unique(pop$ETH.group),
           sex %in% unique(pop$sex)) %>%
    rename(pop = births) %>%
    rbind.data.frame(pop) %>%
    arrange(year, ETH.group, sex, age)   # sort ages in ascending order
}

# increase
add_pop <- function(delta_col,
                    is_prop = FALSE) {

  delta_col <- enquo(delta_col)
  change_pop(delta_col, is_prop, direction = +1)
}

# decrease
rm_pop <- function(delta_col,
                   is_prop = FALSE) {

  delta_col <- enquo(delta_col)
  change_pop(delta_col, is_prop, direction = -1)
}

# generic
change_pop <- function(delta_col,
                       is_prop,
                       direction) {

  function(pop, dat) {

    if (all(is.na(dat))) return(pop)

    dat %>%
      select_at(vars(-contains("X1"))) %>%         # remove column
      merge(pop,
            by = c("year", "age", "ETH.group", "sex")) %>%
      mutate(is_prop = is_prop,
             pop = ifelse(is_prop,
                          yes = pop + direction*pop*(!!delta_col),
                          no  = pop + direction*(!!delta_col))) %>%
      arrange(year, ETH.group, sex, age) %>%
      select(-!!delta_col, -is_prop) %>%
      as_tibble()
  }
}

