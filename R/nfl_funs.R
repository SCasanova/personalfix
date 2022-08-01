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
#' @export



clean_roster <- function(seasons = 2021) {
  nflreadr::load_rosters(seasons) %>%
    dplyr::mutate(
      position = dplyr::case_when(position == 'FB' |
                                    position == 'HB' ~ 'RB',
                                  T ~ position),
      headshot_url = stringr::str_replace(headshot_url, 'http:', 'https:')
    ) %>% dplyr::left_join(
      ffscrapr::dp_playerids() %>% dplyr::select(gsis_id, espn_id, draft_year, draft_round, draft_pick, draft_ovr) %>% dplyr::filter(!is.na(gsis_id)),
      by = 'gsis_id',
      na.matches = 'never'
    ) %>%
   dplyr::mutate(espn_id =  dplyr::coalesce(as.character(espn_id.x), as.character(espn_id.y)),
                 draft_round = ifelse(is.na(draft_round), 'UDFA', draft_round),
                 headshot_url = ifelse(is.na(headshot_url), paste0('https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/', espn_id, '.png'), headshot_url)) %>%
    select(-c(espn_id.x,espn_id.y))
}

#' Merge Name
#'
#' This function takes a vector of NFL names and converts them to merge-ready format.
#' All arguments but name are optional and draft_year is optional but data entered must be in year format.
#'
#'
#'
#' @param name vector of NFL names to be converted to merge-ready format
#' @param arg_1 (optional) additional argument to add specificity. (Draft Round recommended fo consistency reasons)
#' @param arg_2 (optional) additional argument to add specificity. (position recommended fo consistency reasons)
#' #' @param draft_year (optional) vector of years in YYYY format
#' @return simplified name (using on ffscrapr mismatches, draft year, and additional arguments)
#' @importFrom magrittr "%>%"
#' @export

name_key <- function(name, arg_1='', arg_2='', draft_year='') {
  key <- ffscrapr::dp_clean_names(name, lowercase = T)
  if(draft_year[1] == ''){
    paste0(
    purrr::map_chr(key, function(x) {
      stringr::str_sub(stringr::str_split(x, ' ')[[1]][1], 1, 3)
    }),
    purrr::map_chr(key, function(x) {
      stringr::str_split(x, ' ')[[1]][2]
    }),
    arg_1,
    arg_2
    )
  } else{
    paste0(
    purrr::map_chr(key, function(x) {
      stringr::str_sub(stringr::str_split(x, ' ')[[1]][1], 1, 3)
    }),
    purrr::map_chr(key, function(x) {
      stringr::str_split(x, ' ')[[1]][2]
    }),
    stringr::str_sub(draft_year, 3, 4),
    arg_1,
    arg_2
  )
  }

}


#' FOutsiders Login
#'
#' This function takes a football ousiders page name, season as well as
#' credentials and outputs said table. Page name should be in the format
#' "team-offense","team-defense","special-teams","qb","wr","rb","te",
#' "basic-offensive-line","basic-defensive-line","pace-stats",
#' "overall-drive-stats", etc.
#'
#'
#'
#' @param page page to scrape
#' @param season season to scrape
#' @param user Football Outsiders authorized username
#' @param pass Football Outsiders authorized password
#' @param talben table number to return (usefull for pace page)
#' @param strip disable removing % (for pace and drive pages)
#' @return table with requested data
#' @importFrom magrittr "%>%"
#' @export

foutsiders_data <-
  function(page = 'team-offense',
           season = 2021,
           user = '',
           pass = '',
           talben = 1,
           strip = 1) {
    if (user == '' | pass == '') {
      warning('No user or pass detected, data may be incomplate')
    }

    login <- paste0('https://www.footballoutsiders.com/stats/nfl/',page,'/',season)

    pgsession <- rvest::session(login)
    pgform <- rvest::html_form(pgsession)[[2]]
    filled_form <- rvest::html_form_set(pgform, name = user, pass = pass)
    rvest::session_submit(pgsession, filled_form)

    page_read <- rvest::session_jump_to(pgsession,login)
    table <- (page_read %>% rvest::html_table(fill = T))[[tablen]]
    if (is.na(table[6, 2]) | table[6, 2] == '') {
      warning('Login failed, check your credentials. Data may be incomplate')
    }
    if (stringr::str_detect(page, 'line')) {
      colnames(table) <- table[1, ]
      table <- table[-1, ]
    }
    if(strip==1){
      options(warn = -1)
      table <- table %>%
        dplyr::select(which(sapply(., is.character))) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), fix_num))
      options(warn = 1)
    }
    table

  }



