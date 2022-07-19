library(dplyr)
source("scrape_espn_leaderboard.R")

dad <- c("Cameron Smith", 
         "Scottie Scheffler",
         "Xander Schauffele",
         "Patrick Cantlay",
         "Collin Morikawa",
         "Tommy Fleetwood",
         "Justin Thomas",
         "Matt Fitzpatrick",
         "Jon Rahm",
         "Max Homa")

tom <- c("Rory McIlroy",
         "Tommy Fleetwood",
         "Justin Thomas",
         "Shane Lowry",
         "Adam Scott",
         "Jon Rahm",
         "Will Zalatoris",
         "Max Homa",
         "Tony Finau",
         "Marc Leishman")


picks <- 
  get_espn_leaderboard() %>% 
  mutate(tom = player %in% tom,
         dad = player %in% dad) %>% 
  filter(tom | dad)

bettor_leaderboard <- function() {
  (picks %>% 
     dplyr::filter(tom) %>% 
     dplyr::filter(row_number() <= 5) %>% 
     dplyr::mutate(bettor = "Tom")
  ) %>% 
    dplyr::bind_rows(
      picks %>% 
        dplyr::filter(dad) %>% 
        dplyr::filter(row_number() <= 5) %>% 
        dplyr::mutate(bettor = "Dad")
    ) %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::select(-(tom:dad)) %>% 
    dplyr::relocate(bettor)
}  

