
# harmonise ETHPOP in/out flow data
# with initial population
# from Labour Force Survey


#
harmonise_lfs_inflow <- function(dat_inflow) {

  dat_inflow %>%
    mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                              "BLA+BLC+OBL", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                              "WBI+WHO", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                              "CHI+OAS", ETH.group),
           age = ifelse(age %in% 90:100, 90, age)) %>%             # make 90 max single age
    group_by(sex, age, ETH.group, year) %>%
    summarise(inmigrants = sum(inmigrants)) %>%
    mutate(CoB = "Non-UK born")
}

#
harmonise_lfs_outflow <- function(dat_outflow,
                                  p_UKborn_outflow = 0.5) {

  dat_outflow %>%
    mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                              "BLA+BLC+OBL", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                              "WBI+WHO", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                              "CHI+OAS", ETH.group),
           age = ifelse(age %in% 90:100, 90, age)) %>%
    group_by(sex, age, ETH.group, year) %>%
    summarise(outmigrants = sum(outmigrants)) %>%
    ungroup() %>%
    mutate(`UK born` = outmigrants*p_UKborn_outflow,
           `Non-UK born` = outmigrants*(1 - p_UKborn_outflow)) %>%
    reshape2::melt(measure.vars = c("UK born", "Non-UK born"),
                   id.vars = c("sex", "age", "ETH.group", "year"),
                   variable.name = "CoB",
                   value.name = "outmigrants") %>%
    as_tibble()
}

#
harmonise_lfs_births <- function(dat_births) {

  dat_births %>%
    mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                              "BLA+BLC+OBL", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                              "WBI+WHO", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                              "CHI+OAS", ETH.group)) %>%
    group_by(sex, ETH.group, year) %>%
    summarise(births = sum(births)) %>%
    mutate(CoB = "UK born")
}

#
harmonise_lfs_deaths <- function(dat_deaths) {

  dat_deaths %>%
    mutate(ETH.group = ifelse(ETH.group %in% c("BLA","BLC","OBL"),
                              "BLA+BLC+OBL", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("WBI","WHO"),
                              "WBI+WHO", ETH.group),
           ETH.group = ifelse(ETH.group %in% c("CHI","OAS"),
                              "CHI+OAS", ETH.group),
           age = ifelse(age %in% 90:100, 90, age)) %>%
    group_by(sex, age, ETH.group, year) %>%
    summarise(deaths = sum(deaths)) %>%
    ungroup() %>%
    mutate(`UK born` = deaths*0.5,                   # assume 50/50 between UK born/Non-UK born
           `Non-UK born` = deaths*0.5) %>%
    reshape2::melt(measure.vars = c("UK born", "Non-UK born"),
                   id.vars = c("sex", "age", "ETH.group", "year"),
                   variable.name = "CoB",
                   value.name = "deaths") %>%
    as_tibble()
}
