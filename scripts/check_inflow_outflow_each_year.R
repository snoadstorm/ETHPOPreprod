
## prep data

dat <-
  sim_pop %>%
  filter(ETH.group == "Bangladeshi",
         sex == "M",
         CoB == "Non-UK born")

dat_d <-
  dat_deaths %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE)) %>%
  group_by(agegrp, sex, ETH.group, year, CoB) %>%
  summarise(pop = sum(deaths)) %>%
  filter(ETH.group == "Bangladeshi",
         sex == "M",
         CoB == "Non-UK born")
dat_in <-
  dat_inflow %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE)) %>%
  group_by(agegrp, sex, ETH.group, year, CoB) %>%
  summarise(pop = sum(inmigrants)) %>%
  filter(ETH.group == "Bangladeshi",
         sex == "M",
         CoB == "Non-UK born")

dat_out <-
  dat_outflow %>%
  mutate(agegrp = cut(age, seq(0,100,by = 5), right = FALSE)) %>%
  group_by(agegrp, sex, ETH.group, year, CoB) %>%
  summarise(pop = sum(outmigrants)) %>%
  filter(ETH.group == "Bangladeshi",
         sex == "M",
         CoB == "Non-UK born")


## main loop

lage <- 10#5
uage <- 14#9
agegroup <- "[10,15)"#"[5,10)"
out <- NULL

for (year in 2012:2050){

  age_in  <- dat$pop[dat$year == year - 1   & dat$age == lage - 1]
  age_out <- dat$pop[dat$year == year       & dat$age == uage]
  death   <- dat_d$pop[dat_d$year == year   & dat_d$agegrp == agegroup]
  inflow  <- dat_in$pop[dat_d$year == year  & dat_d$agegrp == agegroup]
  outflow <- dat_out$pop[dat_d$year == year & dat_d$agegrp == agegroup]

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

counts <- rbind(
  c(out[1, "age_out"], out[1, "age_in"], out[1, "inflow"], out[1, "outflow"]),
  c(out[1, "death"],0,0,0))

barplot(counts, main = "",
        xlab = "", col=c("blue","red"),
        legend = rownames(counts))
