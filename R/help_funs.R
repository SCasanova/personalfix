
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
  as.numeric(scale(var))
}

#' Custom ggplot theme
#'
#' Applies a custom theme to ggplot object
#'
#'
#' @param size font size value (default = 13)
#' @return ggplot2t heme
#' @importFrom magrittr "%>%"
#' @export

theme_cas <- function(size = 13){
  ggplot2::theme_minimal()+
    ggplot2::theme(
    title =   ggplot2::element_text(family = 'mono'),
    plot.title =   ggplot2::element_text(size = 18, face = 'bold'),
    plot.subtitle =   ggplot2::element_text(size = size),
    panel.grid.minor =   ggplot2::element_blank(),
    panel.grid.major =   ggplot2::element_line(color = '#c7bfbd', linetype = 3),
    plot.background = element_rect(fill = '#f8f2e4', color = "#f8f2e4")

  )
}





