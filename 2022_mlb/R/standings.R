library(baseballr)
library(tidyverse)
library(janitor)
library(magrittr)
library(purrr)

get_weekly_standings <- function(date=Sys.Date()) {
  map(.x = sundays(),
      .f = get_standings,
      from = FALSE) %>% 
    bind_rows()
}

get_standings <- function(date=Sys.Date(), from=FALSE) {
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
      from = from) %>% 
    bind_rows()
}

get_standings_division <- function(division, date = Sys.Date(), from = FALSE) {
  standings_on_date_bref(date = date, 
                         from = from, 
                         division = division) %>% 
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
