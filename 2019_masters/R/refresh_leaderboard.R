refresh_leaderboard_remote <- function() {
  sess <- remoteDriver(
    remoteServerAddr = "192.168.0.246",
    port = 4445L,
    browser = "firefox")
  
  sess$open()
  sess$navigate(url)
  Sys.sleep(2)
  sess$getPageSource()[[1]] -> html_source
  sess$close()
  
  html_source
}

refresh_leaderboard_local <- function() {
  sess <- rsDriver(
    remoteServerAddr = "192.168.99.100",
    port = 4445L,
    verbose = FALSE, 
    browser = "firefox")
  
  sess$client$navigate(url)
  Sys.sleep(2)
  sess$client$getPageSource()[[1]] -> html_source
  sess$client$close()
  
  html_source
}

refresh_leaderboard <- function(mode) {
  if (!mode %in% c("local","remote")) {
    stop("invalid mode")
  } else if (mode == "local") {
    html_source <- refresh_leaderboard_local()
  } else {
    html_source <- refresh_leaderboard_remote()
  }
  
  html_source %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table(fill=TRUE) %>% 
    as_tibble() -> leaderboard
  
  draft_picks <- read_csv("data/picks.csv") %>% 
    clean_names() %>% 
    rename(draft_pick = pick)
  
  leaderboard[3,] -> colnames(leaderboard)
  
  leaderboard[-(1:3),] %>% 
    clean_names() %>% 
    rename(strokes = total_2) %>%  
    replace_with_na_all(condition = ~.x == "") %>% 
    filter(name != "0", !is.na(name))-> leaderboard
  
  leaderboard[is.na(leaderboard)] <- 0
  
  left_join(leaderboard, draft_picks, by = "name")
}