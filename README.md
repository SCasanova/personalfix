# personalfix

A set of functions to easily access Football Outsiders data, clean NFL names and 
a wrapper for nflreadr::load_rosters()

## Installation

```{r eval = FALSE}
if (!require("remotes")) install.packages("remotes")
remotes::install_github("SCasanova/personalfix")
```

## Functions

`clean_rosters()`
This function is a wrapper for nflreadr::load_rosters() and outputs all
headshot urls with https: prefix and integrates FB, HB and RB into RB.
Also includes draft data from ffscrapr::dp_playerids()

`name_key()`
This functions takes arguments name, draft_year, draft_round and position 
(easily obtainable from clean_rosters) and outputs a merge-ready name for 
situations where IDs are not available

Example:
```{r}
name_key(c('Zeke Elliott', 'Josh Jacobs'), c(2017, 2019), c(1,1), c('RB','RB'))
```

`fix_num()`
This function will take a vector, detect its format (character, numeric or percentage) and will adjust to numeric when possible

Example:
```{r}
fix_num("20%")
fix_num("20")
fix_num("twenty")
```



`foutsiders_data()`
This function accepts page name, season, username, password to scrape from 
Football Outsiders. 
Current supported pages are:

"team-offense"
"team-defense"
"special-teams"
"qb"
"wr"
"rb"
"te"
"basic-offensive-line"
"basic-defensive-line"



