if (!require(dplyr)) {
  install.packages('dplyr')
  library(dplyr)
}
if (!require(janitor)) {
  install.packages('janitor')
  library(janitor)
}

source("R/functions.R")

team_ratings <- readRDS('Data/r') |> janitor::clean_names()

print(team_ratings)

sim_one_game('Bowlero', "Topa Topa")
