library(dplyr)
library(janitor)

source("R/functions.R")

team_ratings <- read.csv('ratings.csv') |> janitor::clean_names()

print(team_ratings)

sim_one_game('Bowlero', "Lama Dog")
