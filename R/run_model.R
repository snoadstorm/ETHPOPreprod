
#' run_model
#'
#' if data = NA then skipped in calculation (identity function)
#' use dat_pop year range to simulate over
#'
#' @param dat_pop
#' @param dat_births
#' @param dat_deaths
#' @param dat_inflow
#' @param dat_outflow
#' @param year0
#' @param n_years including initial year
#' @param max_age
#' @param is_prop Per capita or counts data; TRUE/FALSE
#'
#' @import purrr, dplyr
#' @return
#' @export
#'
run_model <- function(dat_pop,
                      dat_births = NA,
                      dat_deaths = NA,
                      dat_inflow = NA,
                      dat_outflow = NA,
                      year0 = min(dat_pop$year),
                      n_years = NA,
                      max_age = 100,
                      is_prop = FALSE) {

  # define functions
  add_deaths <- rm_pop(deaths, is_prop)
  add_inflow <- add_pop(inmigrants)#, is_prop) ##TODO: not per capita
  add_outflow <- rm_pop(outmigrants, is_prop)
  add_newborn <- purrr::partial(add_births, is_prop = is_prop)
  age_pop <- purrr::partial(age_population, max_age = max_age)

  # sequence of years to estimate for
  if (is.na(n_years)) {
    years <- sort(unique(dat_pop$year))
  } else {
    years <- seq(from = year0, length.out = n_years)
  }

  # results list
  res <- vector("list", length(years))
  names(res) <- years

  # starting year population
  pop <-
    dat_pop %>%
    filter(year == year0) %>%
    select_at(vars(-contains("X1")))    # remove column

  res[[as.character(year0)]] <- pop

  # loop over years
  for (i in as.character(years[-1])) {

    pop <-
      pop %>%
      age_pop() %>%
      add_newborn(dat_births) %>%
      add_deaths(dat_deaths) %>%
      add_inflow(dat_inflow) %>%
      add_outflow(dat_outflow)

    res[[i]] <- pop

    message("year ", i)
  }

  res
}
