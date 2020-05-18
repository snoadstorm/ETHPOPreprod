
# increment everyone by one year
#
age_population <- function(pop,
                           max_age = 100) {

  pop %>%
    mutate(age = ifelse(age < max_age, age + 1, age), # age 100 means >=100
           year = year + 1) %>%
    group_by(ETH.group, sex, year, age) %>%
    summarise(pop = sum(pop)) %>%             # sum all (previous and new) 100 ages
    ungroup()
}


# newborn population increase
#
add_births <- function(pop, dat_births) {

  if (any(pop$age == 0)) stop("Shouldn't be any 0 aged in population data")
  if (all(is.na(dat_births))) return(pop)

  dat_births %>%
    select(-X1) %>%
    mutate(age = 0) %>%                # everyone is age 0
    filter(year == pop$year[1],
           ETH.group %in% unique(pop$ETH.group),
           sex %in% unique(pop$sex)) %>%
    rename(pop = births) %>%
    rbind.data.frame(pop) %>%
    arrange(year, ETH.group, sex, age) # sort ages in ascending order
}


add_pop <- function(delta_col) {

  delta_col <- enquo(delta_col)
  change_pop(delta_col, direction = +1)
}

rm_pop <- function(delta_col) {

  delta_col <- enquo(delta_col)
  change_pop(delta_col, direction = -1)
}


change_pop <- function(delta_col, direction) {

  function(pop, dat) {

    if (all(is.na(dat))) return(pop)

    dat %>%
      select(-X1) %>%
      merge(pop,
            by = c("year", "age", "ETH.group", "sex")) %>%
      mutate(pop = pop + direction*(!!delta_col)) %>%
      arrange(year, ETH.group, sex, age) %>%
      select(-!!delta_col) %>%
      as_tibble()
  }
}

