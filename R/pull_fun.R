
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
#' @export

pull_scm <- function(seasons=2020){
  data_name <- data.table::data.table()
  for (x in seasons){
    y <- (xml2::read_html(url(paste0("https://www.pro-football-reference.com/years/",x,"/scrimmage.htm"))) %>% #read and interpret html table
            rvest::html_table(fill = T))[[1]]
    colnames(y) <- y[1,]
    delete <- rownames(y[y$Rk == 'Rk',]) %>% #delete intermediate column-name rows
      as.numeric()
    y <- y[-delete,]
    y <- data.table::data.table(y)
    colnames(y) <- c('Rk', 'Player','Team', 'Age', 'Pos',  'G', 'GS',
                     'Tgt', 'Rec', 'Yds','YpR', 'TD_Rec', 'FirstD_Rec', 'Lng', 'RpG','YpG_Rec',  'Ctch_pct','YpTgt',
                     'Att',  'Yds_Rush','TD_Rush', 'FirstD_Rush', 'Lng_Rush',  'YpA',   'YpG',  'ApG', 'Touch', 'YpTch', 'YScm', 'RRTD', 'Fmb')
    y[, Season := x]
    y[,ProBowl := ifelse(grepl("*", y$Player, fixed = T), 1, 0)] #dummy variable
    y[,AllPro := ifelse(grepl("+", y$Player, fixed = T), 1, 0)]
    data_name <- rbind(data_name, y)
  }


  #Clean Player names
  data_name$Player <- gsub("*", "", data_name$Player, fixed = T)
  data_name$Player <- gsub(".", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("+", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("Jr", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("Sr", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("II", "", data_name$Player, fixed = T)
  data_name$Ctch_pct <- gsub("%", "", data_name$Ctch_pct, fixed = T)
  data_name$Player <- trimws(data_name$Player)
  data_name <- data_name %>% dplyr::mutate_at(c(6:34), as.numeric)
  data_name$Ctch_pct <- data_name$Ctch_pct/100
  data_name[is.na(data_name)] <- 0

  roster <- nflfastR::fast_scraper_roster(seasons)  #nflfastR roster data to merge positions, heigh, weight, etc

  pos_data <- roster %>%
    dplyr::select(position, full_name, team, season) %>%
    dplyr::filter(position %in% c('QB', 'WR', 'RB', 'FB', 'TE')) %>%
    dplyr::rename(short_team = team)

  pos_data$full_name <- gsub(".", "", pos_data$full_name, fixed = T)

  full_data <- unique(merge(data_name, pos_data, by.x = c('Player', 'Season'), by.y = c('full_name', 'season'), all.x = T, allow.cartesian = T)) %>%
    as.data.table()
  full_data <- full_data %>% dplyr::filter(!(is.na(position))) %>%
    dplyr::select(-Rk, -Pos)
  full_data[, Opps := Tgt+Att]
  full_data[, Fpts := (Rec*0.5)+ (YScm*0.1)+ (RRTD*6)]
  full_data[, PPR_Fpts := (Rec*1)+ (YScm*0.1)+ (RRTD*6)]
  full_data[, FirstD_Rec_pct := ifelse(Tgt == 0, 0, round(FirstD_Rec/Tgt, 3))]
  full_data[, FirstD_Rush_pct := ifelse(Att == 0, 0, round(FirstD_Rush/Att, 3))]
  return(full_data)
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
#' @export

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

  data_name$Player <- gsub("*", "", data_name$Player, fixed = T)
  data_name$Player <- gsub(".", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("+", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("Jr", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("Sr", "", data_name$Player, fixed = T)
  data_name$Player <- gsub("II", "", data_name$Player, fixed = T)
  data_name$Player <- trimws(data_name$Player)
  data_name <- data_name %>% dplyr::mutate_at(c(9:34), as.numeric)
}



