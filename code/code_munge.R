### Initialize ----

source("./code_header.R")
dir0 <- function(file,base=NA) {
  if (is.na(base)) {
    here::here() %.>% str_glue("{.}/{file}")
  } else {
    here::here() %.>% str_glue("{.}/{base}/{file}")
  }
}

### Fix cols and select necessary cols ----

dir0("appannie.rds", "rds") %.>% readRDS(.) -> tbl0

# Use janitor 

library(janitor)
library(lubridate)

tbl0 %>%
  clean_names() %>%
  remove_empty(c("rows", "cols"))  -> tbl0

### Remove cols & fix some small things ----

tbl0 %.>% 
  select(., -one_of(
    "category", "country", "app_id", "app_url", "publisher_id", "publisher_name", 
    "app_franchise_id", "company_id", "parent_company_id", 
    "version", "value_type", "change_percent", "change_rank"
    )
  ) %.>% 
  mutate(., 
    date = word(period, 1, 1, "~") %.>% ymd(.)
  ) %.>% 
  mutate(., 
    last_update = ymd(last_update), 
    app_release_date = ymd(app_release_date)
  ) %.>%
  mutate_at(., c(
    "rank", "value", "star_rating", "ratings"
    ), as.numeric
  ) %.>% 
  select(., -period) %.>% 
  select(., rank, date, app_name, everything()) %.>% 
  saveRDS(., dir0("appannie_reduced.rds", "rds")) 





### End of code ----