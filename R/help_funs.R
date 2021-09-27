
#' Fix number format in vector
#'
#' This function takes a vector and converts numbers or percentages into
#' numeric form while ignoring character columns for easy full data frame procesing.
#' (With dplyr::across for example)
#'
#'
#' @param nums Range of years to pull from nflreadr rosters. Defaults to 2020
#' @return converted data to correct numeric form (eg. "20%" to 0.2 and "2.3" to 2.3)
#' @importFrom magrittr "%>%"
#' @export




fix_num <- function(nums) {
  if (stringr::str_detect(nums[1], "%$")) {
    nums <- nums %>%
      stringr::str_remove(., "%") %>%
      as.numeric()
    nums / 100
  } else if (stringr::str_detect(nums[1], "[0-9]\\.?,?[0-9]*$")) {
    nums %>%
      stringr::str_remove(., ",") %>%
      as.numeric()
  } else{
    nums
  }
}


#' Standardize variable
#'
#' This function takes a vector of a numeric variable and converts them to a
#' standardized version of said variable
#'
#'
#' @param var Numeric Variable
#' @return Standardized variable
#' @importFrom magrittr "%>%"
#' @export

standardize <- function(var){
  mean <- mean(var, na.rm = T)
  sd <- sd(var, na.rm = T)
  purrr::map_dbl(var, function(x){(x-mean)/sd})
}


