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

tbl0 %>%
  ungroup() %>% 
  dplyr::filter(!date %in% c(ymd("2017-05-17", "2018-05-16"))) %>% 
  dplyr::filter(between(rank_rv2, 1, 100)) %>% 
  mutate(
    year = year(date), 
    ttm = case_when(
      date < ymd("2017-05-17") ~ "before", 
      date > ymd("2017-05-17") & date < ymd("2018-01-01") ~ "after", 
      date < ymd("2018-05-16") & date > ymd("2018-01-01") ~ "before", 
      TRUE ~ "after"
    )
  ) %>% 
  count(year, ttm)






