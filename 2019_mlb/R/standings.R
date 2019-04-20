library(baseballr)
library(tidyverse)
library(janitor)
library(magrittr)

get_standings <- function(date=Sys.Date(), from=FALSE) {
  divisions <- c(
    "NL East",
    "NL Central",
    "NL West",
    "AL East",
    "AL Central",
    "AL West"
  )
  
  purrr::map(.x = divisions, 
             .f = get_standings_division, 
             date = Sys.Date(),
             from = from) %>% 
    bind_rows()
}

get_standings_division <- function(date, from, division) {
  standings_on_date_bref(date = date, 
                         from = from, 
                         division = division) %>% 
    extract2(1) %>% 
    clean_names() %>% 
    as_tibble() %>% 
    mutate(division = division) -> result
  
  if (from) {
    result %>% 
      mutate(since = date)
  } else {
    result %>% 
      mutate(asOf = date)
  }
}
