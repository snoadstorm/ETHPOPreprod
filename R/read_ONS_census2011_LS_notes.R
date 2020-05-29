
#' read_ONS_census2011
#'
#' @param dir_name
#' @param file_name
#' @param save_dir
#' @param save_filename
#'
#' @import readxl, dplyr, tidyr, tibble, stringr
#' @return
#' @export
#'
read_ONS_census2011 <- function(dir_name = here::here("raw data"),
                                file_name = "CT0596_2011 Census - Sex by age by COB by ethnic group by YOA- Merged LA.xlsx",
                                save_dir = here::here("output_data"),
                                save_filename = "ONS_census2011") {

  file_loc <- paste(dir_name, file_name, sep = "/")
# assigning an object file_loc to raw data/CT0...
  dat <-
    readxl::read_xlsx(file_loc, range = "A10:AJC15", sheet = "All usual residents", col_names = FALSE) %>%
    t() %>%
    as_tibble()
# dat goes to read the xlsx file at the file location specified above, reading just the cells in specified
# in this sheet, then false col_names means that the first rows are not column headings, it's true by default
# then (pipe) transposing this and creating a tibble that's long rather than wide
  dat <-
    dat %>%
    fill(V1, .direction = "down") %>%
    fill(V2, .direction = "down") %>%
    fill(V3, .direction = "down") %>%
# Because the xlsx had loads of gaps in it (they only put one heading across several columns)
# here we're filling down so every part of the tibble so that when we move stuff around it's
# identifiable.
    `names<-`(c("sex", "agegrp", "cob", "ethgrp", "pop_EW", "pop_E")) %>%
# this has renamed the columns as sex, agegrp etc
# NB the names() function retrieves or sets (with <- after) the names of an object
    na.omit() %>%
# this removes the rows with NA in them completely from the tibble
    mutate(agegrp = gsub(agegrp, pattern = "Age", replacement = ""),
           sex = ifelse(sex == "Males", "M", ifelse(sex == "Females", "F", "persons")))
# mutate adds new variables and preserves existing ones, whereas transmute adds new variables and drops existing ones
# both functions preserve the number of rows, and overwite existing variables of the same name
# gsub is a form of grep function, which here is going to everything in agegrp, and substituting
# age with nothing (aka removing age)
# ifelse runs a test and returns values matching the test so ifelse(thing its test, if yes, if no)
# so here it's doing if the sex is males, return M, if not - is sex is females, return f, if not
# return persons

  ## mapping ethnic groups to ETHPOP
  # BAN (Asian/Asian British: Bangladeshi),
  # BLA (Black/African/Caribbean/Black British),
  # BLC (Black/African/Caribbean/Black British),
  # CHI (Asian/Asian British: Chinese),
  # IND (Asian/Asian British: Indian),
  # MIX (Mixed/multiple ethnic groups: White and Black Caribbean/ White and Black African/White and Asian/Other Mixed, Asian/Asian British: Other Asian, Other ethnic group: Arab, Other ethnic group: Any other ethnic group),
  # OAS (Mixed/multiple ethnic groups: White and Black Caribbean/ White and Black African/White and Asian/Other Mixed, Asian/Asian British: Other Asian, Other ethnic group: Arab, Other ethnic group: Any other ethnic group),
  # OBL (Other Black),
  # OTH (Mixed/multiple ethnic groups: White and Black Caribbean/ White and Black African/White and Asian/Other Mixed, Asian/Asian British: Other Asian, Other ethnic group: Arab, Other ethnic group: Any other ethnic group),
  # PAK (Asian/Asian British: Pakistani),
  # WBI (White: English/Welsh/Scottish/Northern Irish/British/Irish/Gypsy or Irish Traveller/Other White),
  # WHO (White: English/Welsh/Scottish/Northern Irish/British/Irish/Gypsy or Irish Traveller/Other White)

  if (!is.na(save_filename)) {
# if there's not any NA (! makes is negative) save file name
# below is the body of the function,aka save function read_ONS_census2011 to the specified
# save directory, with a backslash, with the specified file name as an rds
    saveRDS(dat, file = paste0(save_dir, "/", save_filename, ".Rds", sep = ""))
    write.csv(dat, file = paste0(save_dir, "/", save_filename, ".csv", sep = ""))
  }
# write CSV with the object we've created above (dat), in this place with this name
  return(dat)
}
# return is the closing part of the function, so we've created this function and we need to
# finish it by telling it to return what we told it to do.
