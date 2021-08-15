
#' Pull NFL Scrimmage Stats
#'
#' This function takes an argument for desired years and outputs a data.table
#' containing all Pro Football Reference scrimmage stats from selected years.
#'
#'
#'
#' @param seasons Range of years to pull from pfref. Defaults to 2020
#' @return A data table including scrimmage stats from selected years
#' @importFrom magrittr "%>%"
#' @import data.table

pull_scm <- function(seasons=2020){
  data_name <- data.table::data.table()
  for (x in seasons){
    y <- (xml2::read_html(url(paste0("https://www.pro-football-reference.com/years/",x,"/scrimmage.htm"))) %>% #read and interpret html table
            rvest::html_table(fill = T))[[1]]
    colnames(y) <- y[1,]
    colnames(y)[10] <- 'Rec Yds'
    colnames(y)[12] <- 'Rec TDs'
    colnames(y)[13] <- '1D Rec'
    colnames(y)[16] <- 'Rec Yds/G'
    colnames(y)[20] <- 'Rush Yds'
    colnames(y)[21] <- 'Rush TDs'
    colnames(y)[22] <- '1D Rush'
    colnames(y)[23] <- 'Lng Rush'
    colnames(y)[25] <- 'Rush Yds/G'
    y <- y[-(y[, 1]=='Rk'),] %>%
      data.table()
    y[, Season := x]
    y[,ProBowl := ifelse(grepl("*", y$Player, fixed = T), 1, 0)] #dummy variable
    y[,AllPro := ifelse(grepl("+", y$Player, fixed = T), 1, 0)]
    data_name <- rbind(data_name, y)
  }


  #Clean Player names
  remove_vec <- c("*", "+", ".", "Jr", "Sr", "III", "II")
  data_name$Player <- clean_name(remove_vec, data_name$Player)
  data_name$`Ctch%` <- gsub("%", "", data_name$`Ctch%`, fixed = T)
  data_name <- data_name %>% dplyr::mutate_at(c(6:34), as.numeric)
  data_name$Ctch_pct <- data_name$Ctch_pct/100
  data_name[is.na(data_name)] <- 0
  data_name <- data_name %>%
    dplyr::mutate(Tm = dplyr::case_when(
      Tm == 'GNB'~'GB',
      Tm == 'SFO'~'SF',
      Tm == 'KAN'~'KC',
      Tm == 'NWE'~'NE',
      Tm == 'TAM'~'TB',
      Tm == 'NOR'~'NO',
      Tm == 'LVR'~'LV',
      Tm == 'SDG'~'SD',
      Tm == 'LAR'~'LA',
      TRUE ~ Tm
    )) %>%
    dplyr::filter(Tm != '2TM',
           Tm != '3TM')

  return(data_name)
}

#' Pull NFL Passing Stats
#'
#' This function takes an argument for desired years and outputs a data.table
#' containing all Pro Football Reference passing stats from selected years.
#'
#'
#'
#' @param seasons Range of years to pull from pfref. Defaults to 2020
#' @return A data table including passsing stats from selected years
#' @importFrom magrittr "%>%"
#' @import data.table

pull_pass <- function(seasons=2020){
  data_name <- data.table()
  for (x in seasons){
    y <- (xml2::read_html(url(paste0("https://www.pro-football-reference.com/years/",x,"/passing.htm")))%>%
            rvest::html_table(fill = T))[[1]]
    delete <- rownames(y[y$Rk == 'Rk',]) %>% as.numeric()
    y <- y[-delete,]
    y <- y %>% as.data.table()
    colnames(y) <- c('Rk', 'Player','Team', 'Age', 'Pos',  'G', 'GS',
                     'QBrec', 'Cmp', 'Att','Cmp_pct', 'Yds', 'TD', 'TD_pct', 'Int','Int_pct',
                     'FirstD','Lng','YpA',  'AYpA','YpC', 'YpG', 'Passer_rate',  'QBR',   'Sk',  'Sk_yds', 'NYpA', 'ANYpA', 'Sk_pct', '4QC', 'GWD')

    y[, Season := x]
    y[,ProBowl := ifelse(Att >=50, ifelse(grepl("*", y$Player, fixed = T), 1, 0), 0)]
    y[,AllPro :=  ifelse(Att <= 50, ifelse(grepl("+", y$Player, fixed = T), 1, 0), 0)]
    data_name <- rbind(data_name, y)
  }

  remove_vec <- c("*", "+", ".", "Jr", "Sr", "III", "II")
  data_name$Player <- clean_name(remove_vec, data_name$Player)
  data_name <- data_name %>% dplyr::mutate_at(c(1,4,6,7,9:34), as.numeric)
  data_name <- data_name %>% dplyr::mutate(Tm = dplyr::case_when(Tm == 'LAR'~'LA',
                                                          Tm == 'TAM'~'TB',
                                                          Tm == 'NWE'~'NE',
                                                          Tm == 'GNB'~'GB',
                                                          Tm == 'KAN'~'KC',
                                                          Tm == 'SFO'~'SF',
                                                          Tm == 'NOR'~'NO',
                                                          Tm == 'LVR'~'LV',
                                                          TRUE ~ Tm))
}

#' Clean name data
#'
#' This function takes a character vector to remove and a vector to be cleaned.
#' Outputs the second vector without the characters in first vector and applies
#' trimws function
#'
#'
#'
#' @param to_remove Vector with characters to search and remove in data_clean
#' @param data_clean Vector suceptible to cleaning
#' @return Second vector without characters from first vector and without ws
#'

clean_name <- function(to_remove, data_clean){
  for (i in 1:length(to_remove)){
    data_clean <- gsub(paste0(to_remove[i]), "", data_clean, fixed = T)
    data_clean <- trimws(data_clean)
  }

  return(data_clean)

}

#' Pull NFL Advanced Team Passing Stats
#'
#' This function takes an argument for desired years and outputs a data.table
#' containing all Pro Football Reference team advanced passing stats from selected years.
#'
#'
#'
#' @param seasons Range of years to pull from pfref. Defaults to 2020
#' @return A data table including team advancedpasssing stats from selected
#' years
#' @importFrom magrittr "%>%"
#' @import data.table

pull_teamAdvPass <- function(seasons=2020){
  data_name <- data.table::data.table()
  for (x in seasons){
    adv <- (xml2::read_html(url(paste0("https://www.pro-football-reference.com/years/",x,"/advanced.htm"))) %>% #read and interpret html table
              rvest::html_table(fill = T))[[1]]
    colnames(adv) <- adv[1,]
    delete <- rownames(adv[adv$Tm == 'Tm',])
    for (i in 2:3){
      tab <- (xml2::read_html(url(paste0("https://www.pro-football-reference.com/years/",x,"/advanced.htm"))) %>% #read and interpret html table
                rvest::html_table(fill = T))[[i]]
      colnames(tab) <- tab[1,]
      delete <- rownames(tab[tab$Tm == 'Tm',]) %>% #delete intermediate column-name rows
        as.numeric()
      tab <- tab[-delete, c(1, 6:ncol(tab))]
      adv <- merge(adv, tab, by = 'Tm')
    }
    adv <- data.table::data.table(adv)
    adv[, Season := x]
    data_name <- rbind(data_name, adv)
  }

  #Clean data
  data_name$`Drop%` <- gsub("%", "", data_name$`Drop%`, fixed = T)
  data_name$`Bad%` <- gsub("%", "", data_name$`Bad%`, fixed = T)
  data_name$`OnTgt%` <- gsub("%", "", data_name$`OnTgt%`, fixed = T)
  data_name$`Prss%` <- gsub("%", "", data_name$`Prss%`, fixed = T)

  data_name <- data_name %>% dplyr::mutate_at(c(2:30), as.numeric)
  data_name$`Drop%` <- data_name$`Drop%`/100
  data_name$`Bad%` <- data_name$`Bad%`/100
  data_name$`OnTgt%` <- data_name$`OnTgt%`/100
  data_name$`Prss%` <- data_name$`Prss%`/100
  data_name[is.na(data_name)] <- 0

  data_name <- data_name %>% dplyr::mutate(Tm = dplyr::case_when(Tm == 'LAR'~'LA',
                                                          Tm == 'TAM'~'TB',
                                                          Tm == 'NWE'~'NE',
                                                          Tm == 'GNB'~'GB',
                                                          Tm == 'KAN'~'KC',
                                                          Tm == 'SFO'~'SF',
                                                          Tm == 'NOR'~'NO',
                                                          Tm == 'LVR'~'LV',
                                                          TRUE ~ Tm))

  return(data_name)
}

#' Pull NFL Advanced Passing Stats
#'
#' This function takes an argument for desired years and outputs a data.table
#' containing all Pro Football Reference individual advanced passing stats from selected years.
#'
#'
#'
#' @param seasons Range of years to pull from pfref. Defaults to 2020
#' @return A data table including individual advanced passsing stats from
#' selected years
#' @importFrom magrittr "%>%"
#' @import data.table

pull_advPass <- function(seasons=2020){
  try(if(min(seasons) < 2019) stop('No advanced data available for seasons before 2019', call. = F))
  data_name <- data.table::data.table()
  for (x in seasons){
    adv <- (xml2::read_html(url(paste0('https://www.pro-football-reference.com/years/',x,'/passing_advanced.htm#all_ks_passing_detailed_air_yards'))) %>% #read and interpret html table
              rvest::html_table())[[1]]
    colnames(adv) <- adv[1,]
    adv <- adv[-(adv[, 1]=='Rk'),] %>%
      data.table()
    for (i in 2:3){
      tab <- (xml2::read_html(url(paste0("https://www.pro-football-reference.com/years/",x,"/passing_advanced.htm"))) %>% #read and interpret html table
                rvest::html_table(fill = T))[[i]]
      colnames(tab) <- tab[1,]
      tab <- tab[-(tab[, 1]=='Rk'),]
      tab <- tab[, c(2, 3,11:ncol(tab))]
      adv <- dplyr::inner_join(adv, tab, by = c('Player', 'Tm')) %>%
        data.table::data.table()
    }
    adv <- data.table::data.table(adv)

    adv[, Season := x]
    adv[,ProBowl := ifelse(Att >=50, ifelse(grepl("*", adv$Player, fixed = T), 1, 0), 0)]
    adv[,AllPro :=  ifelse(Att <= 50, ifelse(grepl("+", adv$Player, fixed = T), 1, 0), 0)]
    data_name <- rbind(data_name, adv)
  }

  #Clean data
  remove_vec <- c("*", "+", ".", "Jr", "Sr", "III", "II")
  data_name$Player <- clean_name(remove_vec, data_name$Player)
  data_name$`Drop%` <- gsub("%", "", data_name$`Drop%`, fixed = T)
  data_name$`Bad%` <- gsub("%", "", data_name$`Bad%`, fixed = T)
  data_name$`OnTgt%` <- gsub("%", "", data_name$`OnTgt%`, fixed = T)
  data_name$`Prss%` <- gsub("%", "", data_name$`Prss%`, fixed = T)

  data_name <- data_name %>% dplyr::mutate_at(c(6:38), as.numeric)
  data_name$`Drop%` <- data_name$`Drop%`/100
  data_name$`Bad%` <- data_name$`Bad%`/100
  data_name$`OnTgt%` <- data_name$`OnTgt%`/100
  data_name$`Prss%` <- data_name$`Prss%`/100
  data_name[is.na(data_name)] <- 0

  data_name <- data_name %>% dplyr::mutate(Tm = dplyr::case_when(Tm == 'LAR'~'LA',
                                                          Tm == 'TAM'~'TB',
                                                          Tm == 'NWE'~'NE',
                                                          Tm == 'GNB'~'GB',
                                                          Tm == 'KAN'~'KC',
                                                          Tm == 'SFO'~'SF',
                                                          Tm == 'NOR'~'NO',
                                                          Tm == 'LVR'~'LV',
                                                          TRUE ~ Tm))

  return(data_name)
}

