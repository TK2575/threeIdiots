library(baseballr)
library(tidyverse)
library(janitor)
library(magrittr)
library(purrr)
library(rvest)

get_weekly_standings <- function(date=Sys.Date()) {
  map(.x = sundays(),
      .f = get_standings,
      from = FALSE) %>% 
    bind_rows()
}

get_standings <- function(html_file, date=Sys.Date(), from=FALSE) {
  divisions <- c(
    "NL East",
    "NL Central",
    "NL West",
    "AL East",
    "AL Central",
    "AL West"
  )
  
  map(.x = divisions, 
      .f = get_standings_division,
      date = date,
      from = from,
      html_file = html_file) %>% 
    bind_rows()
}

get_standings_division <- function(division, date = Sys.Date(), from = FALSE, html_file = NULL) {
  df <- NULL
  if (is.null(html_file)) {
    df <- standings_on_date_bref(date = date, 
                                 from = from, 
                                 division = division)
  } else {
    df <- standings_on_date_html(date = date,
                                 from = from,
                                 division = division,
                                 html_file = html_file)
  }
  df %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(division = division) -> result
  
  if (from) {
    result %>% 
      mutate(since = date)
  } else {
    result %>% 
      mutate(as_of = date)
  }
}

sundays <- function(thru = Sys.Date()) {
  rslt <- seq(
    from=as.Date("2019-03-31"),
    to=thru,
    by="1 week"
  )
  
  if (!thru %in% rslt) {
    rslt <- c(rslt, thru)
  }
  
  rslt
}

standings_on_date_html <- function(date, division, html_file, from = FALSE) {
  all_divisions <- c("AL East", "AL Central", "AL West", "AL Overall", 
                     "NL East", "NL Central", "NL West", "NL Overall")
  if (!(division %in% all_divisions)) {
    stop("Please select a division in the following: \n'AL East', 'AL Central', 'AL West', 'AL Overall',\n'NL Central', 'NL West', 'NL Overall'")
  }
  
  html_doc <- html_file %>% xml2::read_html()
  tables <- html_doc %>% rvest::html_elements("table")
  min <- length(tables)
  max <- length(tables) - 15
  tables <- tables[min:max] %>% rvest::html_table()
  table_names <- c("NL Overall", "AL Overall", "NL West", 
                   "NL Central", "NL East", "AL West", "AL Central", 
                   "AL East", "NL Overall", "AL Overall", "NL West", 
                   "NL Central", "NL East", "AL West", "AL Central", 
                   "AL East")
  table_names[1:8] <- paste0(table_names[1:8], "_after_", 
                             date)
  table_names[9:16] <- paste0(table_names[9:16], "_up to_", 
                              date)
  names(tables) <- table_names
  after <- tables[1:8]
  current <- tables[9:16]
  if (from == FALSE) {
    div_date <- paste0(division, "_up to_", date)
    x <- current[div_date]
    x <- x[[1]]
  } else if (from != FALSE) {
    div_date <- paste0(division, "_after_", date)
    x <- after[div_date]
    x <- x[[1]]
  }
  x <- x %>% mock_baseballr_data("MLB Standings on Date data from baseball-reference.com", 
                                 Sys.time())
  x
}

mock_baseballr_data <- function(df, type, timestamp) 
{
  out <- df %>% tidyr::as_tibble()
  class(out) <- c("baseballr_data", "tbl_df", "tbl", "data.table", 
                  "data.frame")
  attr(out, "baseballr_timestamp") <- timestamp
  attr(out, "baseballr_type") <- type
  return(out)
}
