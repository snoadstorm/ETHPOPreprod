
# harmonise ETHPOP in/out flow data
# with initial population
# from ONS census 2011

#
harmonise_ons_inflow <- function(dat_inflow){

  dat_inflow %>%
    mutate(ETH.group =
             case_when(
               ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
               ETH.group %in% c("WBI","WHO") ~ "White",
               ETH.group == "BAN" ~ "Bangladeshi",
               ETH.group == "BLA" ~ "Black-African",
               ETH.group == "BLC" ~ "Black-Caribbean",
               ETH.group == "OBL" ~ "Black-Other",
               ETH.group == "CHI" ~ "Chinese",
               ETH.group == "IND" ~ "Indian",
               ETH.group == "PAK" ~ "Pakistan"),
           age = ifelse(age %in% 85:100, 85, age)) %>%             # make 90 max single age
    group_by(sex, age, ETH.group, year) %>%
    summarise(inmigrants = sum(inmigrants)) %>%
    mutate(CoB = "Non-UK born")
}

#
harmonise_ons_outflow <- function(dat_outflow,
                                  prob_UKborn = 0.5) {
  dat_outflow %>%
    mutate(ETH.group =
             case_when(
               ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
               ETH.group %in% c("WBI","WHO") ~ "White",
               ETH.group == "BAN" ~ "Bangladeshi",
               ETH.group == "BLA" ~ "Black-African",
               ETH.group == "BLC" ~ "Black-Caribbean",
               ETH.group == "OBL" ~ "Black-Other",
               ETH.group == "CHI" ~ "Chinese",
               ETH.group == "IND" ~ "Indian",
               ETH.group == "PAK" ~ "Pakistan"),
           age = ifelse(age %in% 85:100, 85, age)) %>%
    group_by(sex, age, ETH.group, year) %>%
    summarise(outmigrants = sum(outmigrants)) %>%
    ungroup() %>%
    mutate(`UK born` = outmigrants*prob_UKborn,                   # assume 50/50 between UK born/Non-UK born
           `Non-UK born` = outmigrants*(1 - prob_UKborn)) %>%
    reshape2::melt(measure.vars = c("UK born", "Non-UK born"),
                   id.vars = c("sex", "age", "ETH.group", "year"),
                   variable.name = "CoB",
                   value.name = "outmigrants") %>%
    as_tibble()
}

#
harmonise_ons_births <- function(dat_births) {

  dat_births %>%
    mutate(ETH.group =
             case_when(
               ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
               ETH.group %in% c("WBI","WHO") ~ "White",
               ETH.group == "BAN" ~ "Bangladeshi",
               ETH.group == "BLA" ~ "Black-African",
               ETH.group == "BLC" ~ "Black-Caribbean",
               ETH.group == "OBL" ~ "Black-Other",
               ETH.group == "CHI" ~ "Chinese",
               ETH.group == "IND" ~ "Indian",
               ETH.group == "PAK" ~ "Pakistan")) %>%
    group_by(sex, ETH.group, year) %>%
    summarise(births = sum(births)) %>%
    mutate(CoB = "UK born")
}

#
harmonise_ons_deaths <- function(dat_deaths,
                                 prob_UKborn = 0.5) {

  dat_deaths %>%
    mutate(ETH.group =
             case_when(
               ETH.group %in% c("MIX","OAS","OTH") ~ "Mixed/Other",
               ETH.group %in% c("WBI","WHO") ~ "White",
               ETH.group == "BAN" ~ "Bangladeshi",
               ETH.group == "BLA" ~ "Black-African",
               ETH.group == "BLC" ~ "Black-Caribbean",
               ETH.group == "OBL" ~ "Black-Other",
               ETH.group == "CHI" ~ "Chinese",
               ETH.group == "IND" ~ "Indian",
               ETH.group == "PAK" ~ "Pakistan"),
           age = ifelse(age %in% 85:100, 85, age)) %>%
    group_by(sex, age, ETH.group, year) %>%
    summarise(deaths = sum(deaths)) %>%
    ungroup() %>%
    mutate(`UK born` = deaths*prob_UKborn,                   # assume 50/50 between UK born/Non-UK born
           `Non-UK born` = deaths*(1 - prob_UKborn)) %>%
    reshape2::melt(measure.vars = c("UK born", "Non-UK born"),
                   id.vars = c("sex", "age", "ETH.group", "year"),
                   variable.name = "CoB",
                   value.name = "deaths") %>%
    as_tibble()
}
