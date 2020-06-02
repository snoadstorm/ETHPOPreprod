
# increment everyone by one year
#
##TODO: increment time in country for non-UK born
#
age_population <- function(pop,
                           max_age = 100) {

  pop %>%
    mutate(age = ifelse(age < max_age, age + 1, age), # e.g. age 100 means >=100
           year = year + 1) %>%
    group_by_at(vars(-pop)) %>%
    summarise(pop = sum(pop)) %>%             # sum all (previous and new) max ages
    ungroup()

  ##TODO:
  # max_timeinUK = 5
  # ifelse(timeinUK < max_timeinUK, timeinUK + 1, timeinUK)
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
    mutate(age = 0) %>%                  # everyone is age 0
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

    join_cols <- names(pop)[names(pop) %in% c("age", "ETH.group", "sex", "year", "CoB")]

    dat %>%
      select_at(vars(-contains("X1"))) %>%   # remove column
      filter(year == pop$year[1],
             ETH.group %in% unique(pop$ETH.group),
             sex %in% pop$sex) %>%
      merge(pop,
            by = join_cols,
            # all.y = TRUE) %>%    # assume all groups already in pop
            all = TRUE) %>%        # may not be some inflow already in pop
      mutate(is_prop = is_prop,
             adj = ifelse(is.na(!!delta_col),
                          yes = 0,
                          no = !!delta_col),
             pop = ifelse(is_prop,
                          yes = pop + direction*pop*adj,
                          no  = ifelse(is.na(pop),
                                       yes = direction*adj,
                                       no  = pop + direction*adj))) %>% # when pop is missing
      arrange(year, ETH.group, sex, age) %>%
      select(-!!delta_col, -is_prop, -adj) %>%
      as_tibble()
  }
}

