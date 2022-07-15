library(dplyr)
source("scrape_espn_leaderboard.R")

leaderboard <- get_espn_leaderboard()

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

leaderboard %>% 
  mutate(tom = player %in% tom,
         dad = player %in% dad) %>% 
  filter(tom | dad)
