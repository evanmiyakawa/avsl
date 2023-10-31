library(rvest)

update_league <- function(league_id) {
  games_champions <- scrape_league(league_id, save = TRUE)
  team_ratings <- update_team_ratings(league_id)
}

update_team_ratings <- function(league_id) {
  library(tidyverse); options(tibble.width = Inf)
  library(magrittr)
  theme_set(hrbrthemes::theme_ipsum_pub())
  theme_update(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))
  library(cmdstanr)
  
  games <- readRDS(glue::glue("Data/games_{league_id}.RDS"))
  
  games <- games %>% 
    select(t1, t2, t1_score, t2_score) %>% 
    as_tibble() |> 
    filter(!is.na(t1_score))
  
  teams <- c(games$t1, games$t2) %>% unique() %>% sort()
  
  teams_df <- tibble(
    team = teams, 
    id = 1:length(teams)
  )
  
  games <- games %>% 
    rowwise() %>% 
    mutate(
      t1_id = which(teams_df$team == t1),
      t2_id = which(teams_df$team == t2)
    ) %>% 
    ungroup()
  
  games
  
  
  ### Get standings #####
  
  standings <- teams_df %>% 
    mutate(w = 0, l = 0, t = 0, gf = 0, ga = 0)
  
  for (i in 1:nrow(games)) {
    g <- games %>% slice(i)
    t1_id <- g$t1_id
    t2_id <- g$t2_id
    if (g$t1_score > g$t2_score) {
      standings$w[t1_id] <- standings$w[t1_id] + 1
      standings$l[t2_id] <- standings$l[t2_id] + 1
    } else if (g$t1_score < g$t2_score) {
      standings$l[t1_id] <- standings$l[t1_id] + 1
      standings$w[t2_id] <- standings$w[t2_id] + 1
    } else {
      standings$t[t1_id] <- standings$t[t1_id] + 1
      standings$t[t2_id] <- standings$t[t2_id] + 1
      
    }
    
    standings$gf[t1_id] <- standings$gf[t1_id] + g$t1_score
    standings$gf[t2_id] <- standings$gf[t2_id] + g$t2_score
    standings$ga[t2_id] <- standings$ga[t2_id] + g$t1_score
    standings$ga[t1_id] <- standings$ga[t1_id] + g$t2_score
    
  }
  
  standings %<>% 
    mutate(gd = gf - ga, pts = w * 3 + t * 1, gp = w + l + t) %>% 
    arrange(-pts) %>% 
    select(team, w:pts, id, everything())
  
  standings
  
  games_long <- games %>% 
    select(t1_score, t1_id, t2_id) %>% 
    rename(off_id = t1_id, def_id = t2_id, goals = t1_score) %>% 
    bind_rows(
      games %>% 
        select(t2_score, t1_id, t2_id) %>% 
        rename(off_id = t2_id, def_id = t1_id, goals = t2_score)
    )
  
  stan_data <- list(
    'N' = nrow(games_long),
    'N_coeffs' = length(teams),
    'goals' = games_long$goals,
    'off_id' = games_long$off_id,
    'def_id' = games_long$def_id,
    'beta_sd' = 0.5
  )
  
  # stan_file <- "avsl.stan"
  # stan_file <- "avsl_off_def.stan"
  stan_file <- "R/avsl_off_def_beta_sigma.stan"
  stan_mod <- stan_file %>% cmdstan_model()
  n_cores <- 10
  
  mcmc_iters <- function(mcmc_draws = 20000, warmup = 2000, n_chains = 4, rstan = T) {
    iter_per_chain <- round(mcmc_draws / n_chains)
    if (rstan) {
      iter_per_chain <- iter_per_chain + warmup
    }
    iter_per_chain
  }
  
  n_chains <- n_cores
  n_warmup <- 1000  # works with alpha model for large data
  n_iter <- mcmc_iters(10000, n_warmup, n_chains, rstan = F)
  
  init_fun <- function() {
    list(
      # 'alpha' = stan_data$goals %>% mean(),
      # 'beta' = rep(0, stan_data$N_coeffs),
      # 'beta_off' = rep(0, stan_data$N_coeffs),
      # 'beta_def' = rep(0, stan_data$N_coeffs),
      'beta_sd' = 0.5
    )
  }
  
  stan_res <- stan_mod$sample(
    data = stan_data,
    chains = n_chains,
    parallel_chains = n_cores,
    iter_warmup = n_warmup,
    iter_sampling = n_iter,
    init = init_fun,
    refresh = round((n_iter + n_warmup) / 5)
  )
  
  stan_res_tb <- stan_res$summary() %>% as_tibble(rownames = "var")
  
  stan_res_tb
  
  log_alpha <- stan_res_tb$mean[stan_res_tb$variable == 'log_alpha']
  
  # stan_res_tb %>% 
  #   print(n = Inf) 
  
  # beta_coeffs <- stan_res_tb$mean[str_detect(stan_res_tb$variable, 'log_beta')]
  # beta_coeffs <- stan_res_tb$mean[str_detect(stan_res_tb$variable, 'log_beta')]
  
  
  ratings <- teams_df %>% 
    mutate(
      off_xg = stan_res_tb$mean[str_detect(stan_res_tb$variable, 'off_xg')],
      def_xg = stan_res_tb$mean[str_detect(stan_res_tb$variable, 'def_xg')],
      net_xg = off_xg - def_xg,
      # beta = beta_coeffs,
      # beta = stan_res_tb$mean[str_detect(stan_res_tb$variable, 'log_beta')],
      log_beta_off = stan_res_tb$mean[str_detect(stan_res_tb$variable, 'log_beta_off')],
      log_beta_def = stan_res_tb$mean[str_detect(stan_res_tb$variable, 'log_beta_def')],
      log_beta_net = log_beta_off + log_beta_def,
      sd_off = stan_res_tb$sd[str_detect(stan_res_tb$variable, 'off_xg')],
      sd_def = stan_res_tb$sd[str_detect(stan_res_tb$variable, 'def_xg')],
      sd_net = sqrt(sd_off ^ 2 + sd_def ^ 2)
    )
  
  ratings_full <- ratings |> 
    mutate(
      log_beta_off_sd = stan_res_tb$sd[str_detect(stan_res_tb$variable, 'log_beta_off')],
      log_beta_def_sd = stan_res_tb$sd[str_detect(stan_res_tb$variable, 'log_beta_def')]
    )
  
  
  final_ratings <- ratings %>% 
    left_join(standings) %>% 
    select(team, off_xg, def_xg, net_xg, gp, w:pts, everything()) %>% 
    select(-id) %>% 
    arrange(-log_beta_net)
  
  final_ratings
  
  saveRDS(ratings_full, file = glue::glue("Data/ratings_full_{league_id}.RDS"))
  saveRDS(log_alpha, file = glue::glue("Data/log_alpha_{league_id}.RDS"))
  
  
  final_ratings |> 
    rename(
      "Team" = team,
      'Offensive XG' = off_xg,
      "Defensive XG" = def_xg,
      "Net XG" = net_xg,
      "GP" = gp,
      "Wins" = w,
      "Losses" = l,
      "Draws" = t,
      "Goals For" = gf,
      "Goals Against" = ga,
      "Goal Diff" = gd,
      "Points" = pts,
      "Offensive Rating" = log_beta_off,
      "Defensive Rating" = log_beta_def,
      "Net Rating" = log_beta_net
    ) |> select(-sd_off, -sd_def, -sd_net) |> 
    # write.csv("ratings.csv")
    saveRDS(file = glue::glue("Data/ratings_{league_id}.RDS"))
  
  final_ratings
}

sim_one_game <- function(t1, t2, league_id) {
  ratings_full <- readRDS(glue::glue("Data/ratings_full_{league_id}.RDS"))
  log_alpha <- readRDS(glue::glue("Data/log_alpha_{league_id}.RDS"))
  
  t1_ratings <- ratings_full |> filter(team == t1)
  t2_ratings <- ratings_full |> filter(team == t2)
  
  t1_beta_off_sim <- rnorm(10000, mean = t1_ratings$log_beta_off, sd = t1_ratings$log_beta_off_sd)
  t1_beta_def_sim <- rnorm(10000, mean = t1_ratings$log_beta_def, sd = t1_ratings$log_beta_def_sd)
  t2_beta_off_sim <- rnorm(10000, mean = t2_ratings$log_beta_off, sd = t2_ratings$log_beta_off_sd)
  t2_beta_def_sim <- rnorm(10000, mean = t2_ratings$log_beta_def, sd = t2_ratings$log_beta_def_sd)
  
  t1_off_rating_game_sim <- exp(log_alpha + t1_beta_off_sim - t2_beta_def_sim)
  t2_off_rating_game_sim <- exp(log_alpha + t2_beta_off_sim - t1_beta_def_sim)
  
  t1_goals_sim <- rpois(10000, t1_off_rating_game_sim)  
  t2_goals_sim <- rpois(10000, t2_off_rating_game_sim)  
  
  tibble(
    t1 = t1,
    t2 = t2,
    t1_goals = mean(t1_goals_sim),
    t2_goals = mean(t2_goals_sim),
    prob_t1_win = mean(t1_goals_sim > t2_goals_sim),
    prob_t2_win = mean(t2_goals_sim > t1_goals_sim),
    prob_draw = mean(t2_goals_sim == t1_goals_sim)
  )
  
  
}


scrape_league <- function(league_id, save = TRUE) {
  league_url <- glue::glue("https://sports.playmetrics.com/external/league/{league_id}")
  standings_page <- glue::glue("{league_url}/standings.html")
  
  webpage <- read_html(standings_page)
  
  standings <- webpage |> html_table() |> pluck(1)
  
  team_ids <- webpage |> html_elements(".team-name") |> html_attr("href") |> 
    str_extract("(?<=/)[0-9]+(?=/)")
  
  team_id_df <- tibble(
    team = standings$Team,
    id = team_ids,
    league_url = league_url,
    url = glue::glue("{league_url}/{id}/schedule.html?page=Standings")
  )
  
  rw <- 1
  parse_one_team <- function(rw, team_id_df) {
    webpage <- read_html(team_id_df$url[rw])
    
    tb <- webpage |> html_table()
    team_name <- team_id_df$team[rw]
    
    # game <- tb[[9]]
    parse_one_game <- function(game, team) {
      opponent <- game$Game |> str_extract("(?<=\\s)[A-Z].+")
      game_result <- game$Score |> str_sub(1, 1)
      game_score <- game$Score |> str_extract("(?<=\\s)[0-9].+") |> 
        str_remove_all("\\s")
      
      if (is.na(game_score)) {
        team_score <- NA
        opp_score <- NA
      } else {
        team_score = game_score |> str_extract(".+(?=\\-)") |> as.numeric()
        opp_score = game_score |> str_extract("(?<=\\-).+") |> as.numeric()
      }
      
      team_vec_sorted <- sort(c(team, opponent))
      
      if (team_vec_sorted[1] == team) {
        df <- tibble(
          t1 = team,
          t2 = opponent,
          t1_score = team_score,
          t2_score = opp_score,
          time = game$Time
        )
        
      } else {
        df <- tibble(
          t1 = opponent,
          t2 = team,
          t1_score = opp_score,
          t2_score = team_score,
          time = game$Time
        )
        
      }
      
      df
    }
    
    team_results <- tb[2:length(tb)] |> 
      map_dfr(parse_one_game, team_name) |> 
      mutate(game_day = 1:n())
    
    team_results
    
  }
  
  
  games <- 1:nrow(team_id_df) |> 
    map_dfr(parse_one_team, team_id_df) |> 
    distinct() |> 
    arrange(game_day)
  
  if (save) {
    saveRDS(games, file = glue::glue("Data/games_{league_id}.RDS"))
  }
  
  games
}

# games <- scrape_league(league_id)
# games




