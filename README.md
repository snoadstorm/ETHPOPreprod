
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demoSynthPop

<!-- badges: start -->

<!-- badges: end -->

The original goal of demoSynthPop is to recreate the ETHPOP population
data using the ETHPOP birth, death, inmigration and outmigration data.
This is a check for internal consistency and we will use the same
approach to extend for UK born and non-UK born individuals too.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ETHPOP-and-ETS/demoSynthPop")
```

## Input data

ETHPOP data can be downloaded from [here](http://www.ethpop.org).

The ETHPOP data are:

  - Births by year, for ethnic group and sex
  - Deaths by year, for ethnic group, sex and age
  - In-migration by year, for ethnic group, sex and age
  - Out-migration by year, for ethnic group, sex and age

The ethnic groups are: BAN (Bangladeshi), BLA (Black African), BLC
(Black Caribbean), CHI (Chinese), IND (Indian), MIX (Mixed), OAS (Other
Asian), OBL (Other Black), OTH (Other), PAK (Pakistani), WBI (White
British), WHO (White Other)

The ages are individual years from 0 to 99 and a single group for
\(\geq\) 100.

The calendar years are 2011 to 2061.

## Example

The main analysis is in [main.R](scripts/main.R) and uses the
`run_model()` function. This loops over each year performing the
‘demographic arithmetic’ of removing and adding individuals according
to the ETHPOP tables.

There are two ways in which we can do this:

  - absolute population counts
  - Proportions

The algorithm in `run_model()` takes the following steps:

1.  Age the population by one year, using `age_population()`.
2.  Include new births at age 0, using `add_births()`.
3.  Remove individuals due to mortality, using `add_deaths()`.
4.  Include in-migrants, using `add_inflow()`.
5.  Remove individuals due to out-migration, using `add_outflow()`.

<!-- end list -->

``` r
library(demoSynthPop)
```

The full output from running [main.R](scripts/main.R) is
[here](https://ethpop-and-ets.github.io/demoSynthPop/main.html).
