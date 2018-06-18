### Initialize ----

source("./code_header.R")
dir0 <- function(file,base=NA) {
  if (is.na(base)) {
    here::here() %.>% str_glue("{.}/{file}")
  } else {
    here::here() %.>% str_glue("{.}/{base}/{file}")
  }
}

### Load Excel files ----

extract_filename <- function(year){
  here::here("raw_data", str_c(year)) %.>% list.files(.)
  }
tibblify <- function(one_file, base){
  one_file %.>% dir0(., base) %.>% read_excel(., skip=5, col_names=T) 
}

#UT
#lst_excel[1] %.>% tibblify(., "raw_data/2017") 

### Tibblify Excel data & save it to rds direc ----
extract_filename(2017) %.>% map_df(., function(x){tibblify(x, "raw_data/2017")}) -> tbl1
extract_filename(2018) %.>% map_df(., function(x){tibblify(x, "raw_data/2018")}) -> tbl2
bind_rows(tbl1, tbl2) %.>% saveRDS(., dir0("appannie.rds", "rds")) 

### End of code 



