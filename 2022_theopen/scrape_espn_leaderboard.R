require(rvest)
require(magrittr)
require(dplyr)
require(janitor)

get_espn_leaderboard <- function() {
  "https://www.espn.com/golf/leaderboard" %>% 
    read_html() %>% 
    html_element(".Table") %>% 
    html_table() %>%
    janitor::clean_names() %>% 
    dplyr::select(-1)
}