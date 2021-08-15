#' Clean Rosters
#'
#' This function is a wrapper for nflreadr::load_rosters() and outputs all
#' headshot urls with https: prefix and integrates FB, HB and RB into RB.
#' Includes draft data
#'
#'
#'
#' @param seasons Range of years to pull from nflreadr rosters. Defaults to 2020
#' @return Clean/prepared roster data
#' @importFrom magrittr "%>%"
#' @import nflreadr
#' @import ffscrapr
#' @export



clean_roster <- function(seasons = 2020) {
  nflreadr::load_rosters(seasons) %>%
    dplyr::mutate(
      position = dplyr::case_when(position == 'FB' |
                                    position == 'HB' ~ 'RB',
                                  T ~ position),
      headshot_url = sapply(headshot_url, function(x) {
        gsub('http:', 'https:', x, fixed = T)
      })
    ) %>% dplyr::left_join(
      ffscrapr::dp_playerids() %>% dplyr::select(gsis_id, draft_year, draft_round, draft_pick) %>% dplyr::filter(!is.na(gsis_id)),
      by = 'gsis_id',
      na.matches = 'never'
    ) %>% dplyr::mutate(draft_round = ifelse(is.na(draft_round), 'UDFA', draft_round))
}

#' Merge Name
#'
#' This function takes a vector of NFL names and converts them to merge-ready format
#'
#'
#'
#' @param name vector of NFL names to be converted to merge-ready format
#' @param draft_year vector of draft years in YYYY format
#' @param draft_round vector of single digit or character (1-7 or UDFA) indicating draft round
#' @param position position in uppercase format (eg. RB, WR)
#' @return simplified name (using on ffscrapr mismatches, draft year, round and position)
#' @importFrom magrittr "%>%"
#' @import ffscrapr
#' @export

name_key <- function(name, draft_year, draft_round, position){
  key <- ffscrapr::dp_clean_names(name, lowercase = T)
  paste0(
    sapply(key, function(x){substr(strsplit(x, ' ')[[1]][1], 1,3)}),
    sapply(key, function(x){strsplit(x, ' ')[[1]][2]}),
    substr(draft_year, 3, 4),
    draft_round,
    position
  )
}


#' FOutsiders Login
#'
#' This function takes a football ousiders page name, season as well as
#' credentials and outputs said table. Page name should be in the format
#' "team-offense", "basic-offensive-line", etc.
#'
#'
#'
#' @param page page to scrape
#' @param season season to scrapte
#' @param user Football Outsiders authorized username
#' @param pass Football Outsiders authorized password
#' @return table with requested data
#' @importFrom magrittr "%>%"
#' @import rvest
#' @export

foutsiders_data <- function(page = 'team-offense', season = 2020, user='', pass='') {
    if( user=='' | pass =='' ) {warning('No user or pass detected, data may be incomplate')}

  login <-
    paste0('https://www.footballoutsiders.com/stats/nfl/',page,'/',season)

  pgsession <- rvest::session(login)
  pgform <- rvest::html_form(pgsession)[[3]]
  filled_form <-
    rvest::html_form_set(pgform, name = user, pass = pass)
  rvest::session_submit(pgsession, filled_form)

  page_read <-
    rvest::session_jump_to(pgsession,
                    login)
  table <- (page_read %>% rvest::html_table(fill = T))[[1]]

  if(stringr::str_detect(page, 'line')){
    colnames(table) <- table[1,]
    table <- table[-1,]
  }

  options(warn =-1)
  table <- table %>%
    dplyr::select(which(sapply(.,is.character))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), fix_num))
  options(warn=1)

  table

}

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
  if (stringr::str_detect(nums[1], "%")) {
    nums <- nums %>%
      gsub("%", "", ., fixed = T) %>%
      as.numeric()

    nums / 100
  } else if (!is.na(as.numeric(nums[1])) &
             is.numeric(as.numeric(nums[1]))) {
    nums %>% as.numeric()
  } else{
    nums
  }
}
