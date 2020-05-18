
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
#'
#' @return
#' @export
#'
run_model <- function(dat_pop,
                      dat_births = NA,
                      dat_deaths = NA,
                      dat_inflow = NA,
                      dat_outflow = NA,
                      year0 = min(dat_pop$year)) {

  add_deaths <- rm_pop(deaths)
  add_inflow <- add_pop(inmigrants)
  add_outflow <- rm_pop(outmigrants)

  # sequence of years to estimate for
  years <- sort(unique(dat_pop$year))

  # results list
  res <- vector("list", length(years))
  names(res) <- years

  # starting year population
  pop <- filter(dat_pop, year == year0) %>% select(-X1)
  res[[as.character(year0)]] <- pop

  # loop over years
  for (i in as.character(years[-1])) {

    pop <-
      pop %>%
      age_population() %>%
      add_births(dat_births) %>%
      add_deaths(dat_deaths) %>%
      add_inflow(dat_inflow) %>%
      add_outflow(dat_outflow)

    res[[i]] <- pop

    message("year ", i)
  }

  res
}
