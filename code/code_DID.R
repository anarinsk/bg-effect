### Initialize ----

source("./code_header.R")
dir0 <- function(file,base=NA) {
  if (is.na(base)) {
    here::here() %.>% str_glue("{.}/{file}")
  } else {
    here::here() %.>% str_glue("{.}/{base}/{file}")
  }
}

c("lubridate") %>% install_load_package()

### Load data. Manipulate tbls ----

dir0("appannie_reduced.rds", "rds") %.>% readRDS(.) -> tbl0

tbl0 %>% dplyr::filter(!is.na(rank)) -> tbl1 

tbl1 %>% 
  select(
    -one_of("rank", "value", "unit")
  ) %>% 
  distinct( 
    app_name, date, .keep_all=TRUE  
  )-> common 

tbl1 %>% 
  select(one_of("rank", "value", "unit", "app_name", "date")
  ) %>% 
  dplyr::filter(unit == "Downloads") %>% 
  rename(
    download = value, 
    rank_dl = rank
  ) %>% 
  select( 
    -unit
  ) -> download

tbl1 %>% 
  select(
    one_of("rank", "value", "unit", "app_name", "date") 
  ) %>% 
  dplyr::filter(unit == "USD") %>% 
  rename(
    revenue = value, 
    rank_rv = rank
  ) %>% 
  select(-unit) -> usd

download %>% 
  left_join(usd, by = c("app_name", "date")) %>% 
  select(rank_dl, rank_rv, app_name, date, revenue, download) %>% 
  left_join(common, by = c("app_name", "date")) %>% 
  arrange(date, -revenue, -download) %>% 
  group_by(date) %>% 
  dplyr::filter(!is.na(revenue)) %>% 
  mutate(rank_rv2 = row_number(-revenue), 
         rank_dl2 = row_number(-download)) %>% 
  select(date, rank_rv2, rank_rv, rank_dl, everything()) -> tbl0 


rm(download, tbl1, usd, common)


### SHFWILF ----

### See date dist

tbl0 %>% 
  group_by(year = year(date), rank_rv2) %>% 
  count(rank_rv2) %>% 
  spread(key=year, value=n) %>% 
  dplyr::filter(`2017`==57 & `2018`==57) -> tbl1  

### Make tbl for DID, pDID 
### 20180516, 20170517 - Wendnesday 

### fun var 1: rank_rv2, rank_dl2 
### fun var 2: rank cut 
### fun var 3: date-cut range 
### fun env: df 

add_date <- function(x, y){ymd(x) + days(y)}
gen_df4did <- function(var, rank, d_range, df){
  
  var_exp <- enquo(var)
  date_2017 <- ymd("2017-05-17") 
  date_2018 <- ymd("2018-05-16") 
  
  df %>%
    ungroup() %>% 
    dplyr::filter( !date %in% c(date_2017, date_2018) ) %>% 
    dplyr::filter( between(!!var_exp, rank[[1]], rank[[2]]) ) %>% 
    arrange(date, !!var_exp) %>% 
    mutate(
      year = year(date), 
      ttm = case_when(
        date < date_2017 ~ "before", 
        date < date_2018 & date > ymd("2018-01-01") ~ "before", 
        date > date_2017 & date < ymd("2018-01-01") ~ "after", 
        TRUE ~ "after"
      ) 
    ) %>% 
    dplyr::filter( 
      between(date, add_date(date_2017, -d_range), add_date(date_2017, d_range)) |
      between(date, add_date(date_2018, -d_range), add_date(date_2018, d_range))
    ) %>% 
    select(date, !!var_exp, app_name, everything()) %>% 
    mutate(
      fct_year = factor(year), 
      fct_ttm = factor(ttm), 
      fct_franchise = if_else(is.na(app_franchise), FALSE, TRUE), 
      fct_domestic = if_else(hq_country == "South Korea", TRUE, FALSE), 
      age = date - app_release_date
    )
}

### Unit test 
gen_df4did(rank_dl2, c(1,100), 7, tbl0) -> df_reg

### Regression Test ----

gen_df4did(rank_rv2, c(1,30), 14, tbl0) %>% 
  group_by(rank_rv2, fct_year, fct_ttm) %>% 
  summarise(revenue = sum(revenue), download = sum(download)) %>% 
  lm(revenue ~ download + age + star_rating +fct_domestic + fct_franchise + fct_year*fct_ttm, data =.) %>% 
  summary()

