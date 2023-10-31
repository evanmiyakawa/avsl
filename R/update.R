library(tidyverse); options(tibble.width = Inf)
library(magrittr)
theme_set(hrbrthemes::theme_ipsum_pub())
theme_update(
plot.title = element_text(hjust = 0.5),
axis.title.x = element_text(hjust = 0.5),
axis.title.y = element_text(hjust = 0.5))

# games <- scrape_league(league_id)
# games

source("R/functions.R")

# update AVSL Champions

champions_league_id <- "317-1296-9a42f23f"
update_league(champions_league_id)

# sim_one_game(team_ratings_legends$team[1], team_ratings_legends$team[2], league_id = legends_league_id)

# update AVSL Legends

legends_league_id <- "317-1866-ac2791d0"
update_league(legends_league_id)

# sim_one_game(team_ratings_legends$team[1], team_ratings_legends$team[2], league_id = legends_league_id)
