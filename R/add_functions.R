
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

# decrease population
#
add_deaths <- function(pop, dat_deaths) {

  if (all(is.na(dat_deaths))) return(pop)

  dat_deaths %>%
    select(-agegrp, -X1) %>%
    merge(pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - deaths) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-deaths) %>%
    as_tibble()
}

# inmigration population increase
#
add_inflow <- function(pop, dat_inflow) {

  if (all(is.na(dat_inflow))) return(pop)

  dat_inflow %>%
    select(-X1) %>%
    merge(pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop + inmigrants) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-inmigrants) %>%
    as_tibble()
}

# outmigration population decrease
#
add_outflow <- function(pop, dat_outflow) {

  if (all(is.na(dat_outflow))) return(pop)

  dat_outflow %>%
    select(-X1) %>%
    merge(pop,
          by = c("year", "age", "ETH.group", "sex")) %>%
    mutate(pop = pop - outmigrants) %>%
    arrange(year, ETH.group, sex, age) %>%
    select(-outmigrants) %>%
    as_tibble()
}

# increment everyone by one year
#
age_population <- function(pop) {

  pop %>%
    mutate(age = ifelse(age < 100, age + 1, age), # age 100 means >=100
           year = year + 1) %>%
    group_by(ETH.group, sex, year, age) %>%
    summarise(pop = sum(pop))             # sum all 100 ages
}
