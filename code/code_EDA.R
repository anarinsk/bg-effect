### Initialize ----

source("./code_header.R")
dir0 <- function(file,base=NA) {
  if (is.na(base)) {
    here::here() %.>% str_glue("{.}/{file}")
  } else {
    here::here() %.>% str_glue("{.}/{base}/{file}")
  }
}

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
  left_join(common, by = c("app_name", "date")) -> tbl0 

### Explore ----

#devtools::install_github("business-science/tibbletime")
install_load_package("tibbletime")
install_load_package("lubridate")

tbl0 %>% 
  distinct(date) %>% 
  mutate( 
    year = year(date) 
  ) %>% 
  group_by(year) %>% 
  mutate(
   n_date = seq_len(n())
  ) -> date

tbl0 %>% 
  mutate(
    year = year(date)
  ) %>% 
  left_join(
    date, by=c("date", "year")
  ) -> tbl1 

# break setting 1
c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000) -> mybreaks 
c("t10", "t20", "t30", "t40", "t50", "t60", "t70", "t80", "t90", "t100", "margin") -> mylabels

# break setting 2
c(0, 3, 25, 100, 1000) -> mybreaks 
c("t3", "t25", "t100", "remain") -> mylabels

my_cut <- function(vc, breaks0=mybreaks, labels0=mylabels){
  cut(vc, breaks = breaks0, labels=mylabels) 
}
summarise_bg <- function(year0, df, do_daily=T){
  df %>% 
    dplyr::filter(year == year0, !is.na(revenue)) %>% 
    group_by(date) %>% 
    arrange(date, -revenue) %>%
    mutate(
      rank_rv2 = seq_len(n()), 
      cat_rv = my_cut(rank_rv2)
    ) %>% 
    group_by(date, cat_rv) %>% 
    summarise(
      sum_rv = sum(revenue), 
      fct_bg = if_else(first(n_date) <29, "BBG", "ABG"), 
      n_date = first(n_date)
    )
}

summarise_bg(2018, tbl1) %>% mutate(year = year(date)) -> tbl2 
summarise_bg(2017, tbl1) %>% mutate(year = year(date)) -> tbl3
bind_rows(tbl3, tbl2) %>% as_tibble(.) -> tbl4 


ymd("2017-05-01", "2017-05-02", "2018-05-01", "2018-05-02",
    "2017-06-01", "2017-06-02", "2018-06-01", "2018-06-02") -> dates_ex

tbl4 %>% 
  dplyr::filter(!date %in% dates_ex) %>%
  group_by(n_date, cat_rv) %>% 
  summarise(dff = sum_rv[2] - sum_rv[1]) -> tbl5 

### Visualization 

install_load_package("ggsci")

tbl2 %.>% 
  ggplot(.) + 
  aes(x=date, y=sum_rv, color=cat_rv) +
  geom_point(alpha = 0.5) + 
  geom_vline(xintercept=ymd("2018-05-16"), col="black", size=1, alpha=0.5, linetype=1) +
  geom_line(alpha = 0.5) + 
  stat_smooth(geom="line", alpha=0.6, size=2, span=0.5) + 
  ggtitle(str_glue("Before and After PUBG Mobile")) + 
  xlab("date") + 
  ylab("revenue") -> p1

tbl5 %.>% 
  ggplot(.) + 
  aes(x=n_date, y=dff, color=cat_rv) +
  geom_point(alpha = 0.5) + 
  geom_vline(xintercept = 29, size=1, alpha=0.5, col="black", linetype=1) +
  geom_line(alpha = 0.5) + 
  stat_smooth (geom="line", alpha=0.6, size=2, span=0.5) + 
  ggtitle("DID of PUBG Mobile") + 
  xlab("date count") + 
  ylab("revenue") -> p2

# Y2018 trend of Before and After PUBG Mobile
p1 + theme_minimal() + scale_color_simpsons()
# DID for PUBG Mobile
p2 + theme_minimal() + scale_color_simpsons()

tbl0 %>%
  mutate(
    is_topdl = if_else(rank_dl==1, TRUE, FALSE)
  ) %>% 
  group_by(date, is_topdl) %>% 
  summarise(
    download = sum(download)
  ) %>% 
  group_by(date) %>% 
  mutate(
    download_top = download[is_topdl==T],
    shr_top = download_top / sum(download)
  ) -> topdl 

topdl %>% 
  dplyr::filter(date >= ymd("2018-01-01")) %>% 
  ggplot() + 
  aes(x=date, y=download_top) +
  geom_col(alpha=0.5) + 
  geom_vline(xintercept=ymd("2018-05-16"), col="black", size=1, alpha=0.5, linetype=1) +
  ggtitle("The Most Downloaded Mobile App") + 
  xlab("date") + 
  ylab("number of downloads") -> p3

topdl %>% 
  dplyr::filter(date >= ymd("2018-01-01")) %>%
  ggplot() + 
  aes(x=date, y=shr_top) +
  geom_col(alpha=0.5) +
  geom_vline(xintercept=ymd("2018-05-16"), col="black", size=1, alpha=0.5, linetype=1) +
  ggtitle("The Share of the Most Downloaded Mobile App") + 
  xlab("date") + 
  ylab("number of downloads")-> p4 

topdl %>% 
  dplyr::filter(date < ymd("2018-01-01")) %>% 
  ggplot() + 
  aes(x=date, y=download_top) +
  geom_col(alpha=0.5) +
  ggtitle("The Most Downloaded Mobile App") + 
  xlab("date") + 
  ylab("number of downloads") -> p5

topdl %>% 
  dplyr::filter(date < ymd("2018-01-01")) %>%
  ggplot() + 
  aes(x=date, y=shr_top) +
  geom_col(alpha=0.5) +
  ggtitle("The Share of the Most Downloaded Mobile App") + 
  xlab("date") + 
  ylab("number of downloads")-> p6

install_load_package("patchwork")

{p3 + p4} / {p5 + p6}

tbl0 %>%
  dplyr::filter(rank_rv==1|app_name=="PUBG Mobile", date >= ymd("2018-01-01")) %>%
  group_by(date) %>% 
  dplyr::filter(n() == 2) %>% 
  group_by(date) %>% 
  summarise(
    rev = revenue[app_name=="PUBG Mobile"],
    shr = rev / revenue[rank_rv==1]
  ) -> rev_compare 
