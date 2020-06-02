
## prep data

# test for subset of population
ETHNICGRP <- "Bangladeshi"
UKBORN <- "Non-UK born"

dat <-
  sim_pop %>%
  filter(ETH.group == ETHNICGRP,
         sex == "M",
         CoB == UKBORN)

dat_d <-
  dat_deaths %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE)) %>%
  group_by(agegrp, sex, ETH.group, year, CoB) %>%
  summarise(pop = sum(deaths)) %>%
  filter(ETH.group == ETHNICGRP,
         sex == "M",
         CoB == UKBORN)
dat_in <-
  dat_inflow %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE)) %>%
  group_by(agegrp, sex, ETH.group, year, CoB) %>%
  summarise(pop = sum(inmigrants)) %>%
  filter(ETH.group == ETHNICGRP,
         sex == "M",
         CoB == UKBORN)

dat_out <-
  dat_outflow %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE)) %>%
  group_by(agegrp, sex, ETH.group, year, CoB) %>%
  summarise(pop = sum(outmigrants)) %>%
  filter(ETH.group == ETHNICGRP,
         sex == "M",
         CoB == UKBORN)

# dat_b <-
#   dat_births %>%
#   filter(ETH.group == "Bangladeshi")


## main loop

lage <- 5
uage <- 9
agegroup <- "[5,10)"
out <- NULL

for (year in 2012:2050){

  age_in  <- if (lage == 0) {
    dat_b$pop[dat_b$year == year]                        # births
  } else {
    dat$pop[dat$year == year - 1 & dat$age == lage - 1]  # previous year, one year younger
  }

  age_out <- dat$pop[dat$year == year       & dat$age == uage]
  death   <- dat_d$pop[dat_d$year == year   & dat_d$agegrp == agegroup]
  inflow  <- dat_in$pop[dat_in$year == year  & dat_in$agegrp == agegroup]
  outflow <- dat_out$pop[dat_out$year == year & dat_out$agegrp == agegroup]

  out <- rbind(out, c(year,
                      ifelse(length(age_in) > 0, age_in, NA),
                      ifelse(length(age_out) > 0, age_out, NA),
                      ifelse(length(death) > 0, death, NA),
                      ifelse(length(inflow) > 0, inflow, NA),
                      ifelse(length(outflow) > 0, outflow, NA)))

}

colnames(out) <-
  c("year", "age_in", "age_out", "death", "inflow", "outflow")

out

## plots
y <- 30

counts <- rbind(
  c(out[y, "age_out"], out[y, "age_in"], out[y, "inflow"], out[y, "outflow"]),
  c(out[y, "death"],0,0,0))

barplot(counts, main = y + 2011 - 1,
        xlab = "", col=c("blue","red"),
        legend = rownames(counts),
        names.arg = c("age out/deaths", "age in", "inflow", "outflow"))



