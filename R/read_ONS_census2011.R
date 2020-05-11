
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

  dat <-
    readxl::read_xlsx(file_loc, range = "A10:AJC15", sheet = "All usual residents") %>%
    t() %>%
    as_tibble()

  dat <-
    dat %>%
    fill(V1, .direction = "down") %>%
    fill(V2, .direction = "down") %>%
    `names<-`(c("agegrp", "cob", "ethgrp", "pop_EW", "pop_E")) %>%
    na.omit() %>%
    mutate(agegrp = gsub(agegrp, pattern = "Age", replacement = ""))

  if (!is.na(save_filename)) {

    saveRDS(dat, file = paste0(save_dir, "/", save_filename, ".Rds", sep = ""))
    write.csv(dat, file = paste0(save_dir, "/", save_filename, ".csv", sep = ""))
  }

  return(dat)
}
