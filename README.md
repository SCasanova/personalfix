# personalfix

A set of functions to easily access Football Outsiders data, merge-ready
NFL names and a wrapper for nflreadr::load_rosters()

## Installation

```{r eval = FALSE}
if (!require("remotes")) install.packages("remotes")
remotes::install_github("SCasanova/personalfix")
```

## Functions

### Football Outsiders Data
`foutsiders_data()`
This function accepts page name, season, username and password to scrape from 
Football Outsiders. 
Current supported pages are:

  * "team-offense"
  * "team-defense"
  * "special-teams"
  * "qb"
  * "wr"
  * "rb"
  * "te"
  * "basic-offensive-line"
  * "basic-defensive-line"

**Example:**
```{r}
foutsiders_data("team-offense", 2020, "username", "password")

# A tibble: 32 × 10
   Team  `Total DVOA` `Weighted DVOA` `Pass DVOA` `Rush DVOA`
   <chr>        <dbl>           <dbl>       <dbl>       <dbl>
 1 GB           0.291           0.316       0.52        0.029
 2 KC           0.239           0.234       0.49       -0.057
 3 TB           0.198           0.246       0.371      -0.02 
 4 TEN          0.184           0.191       0.395       0.061
 5 BUF          0.156           0.178       0.433      -0.151
 6 SEA          0.137           0.095       0.304      -0.014
 7 NO           0.107           0.094       0.166       0.095
 8 MIN          0.063           0.06        0.188       0.007
 9 CLE          0.054           0.081       0.209      -0.008
10 LAR          0.044          -0.048       0.124       0.035
# … with 22 more rows, and 5 more variables:
#   Unadj. Total VOA <dbl>, Unadj. Pass VOA <dbl>,
#   Unadj. Rush VOA <dbl>, Variance <dbl>, Schedule <dbl>
```

### Adjust Numeric Formats
`fix_num()`
This function will take a vector, detect its format (character, numeric or percentage) and will adjust to numeric when possible

**Examples:**
```{r}
fix_num("20%")
[1] 0.2

fix_num("20")
[1] 20

fix_num("twenty")
[1] "twenty"

df %>% dplyr::mutate(dplyr::across(dplyr::everything(), fix_num))
```
### Merge-ready NFL Names
 `name_key()`
This functions takes arguments name, draft_year, draft_round and position 
(easily obtainable from `clean_rosters()`) and outputs a merge-ready name for 
situations where IDs are not available. It also uses ffscrapr's mismatched names
database to fix name variations. 

Alternatively other variables could be used in place of draft_round and position

**Examples:**
```{r}
name_key(c('Zeke Elliott', 'Josh Jacobs'), c(2017, 2019), c(1,1), c('RB','RB'))
[1] "ezeelliott171RB" "josjacobs191RB" 

df %>% dplyr::mutate(merge_name = name_key(name, draft_year, draft_round, position))
```


### Modified Rosters
`clean_rosters()`
This function is a wrapper for` nflreadr::load_rosters()` and outputs all
headshot urls with https: prefix and integrates FB, HB and RB into RB.
Also includes draft data from `ffscrapr::dp_playerids()`

```{r}
clean_rosters() %>% dplyr::select(full_name, headshot_url, draft_year)
```


