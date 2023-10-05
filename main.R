if (!require(dplyr)) {
  install.packages('dplyr')
  library(dplyr)
}
if (!require(janitor)) {
  install.packages('janitor')
  library(janitor)
}

source("R/functions.R")

team_ratings <- read.csv('ratings.csv') |> janitor::clean_names()

print(team_ratings)

sim_one_game('Bowlero', "Lama Dog")
