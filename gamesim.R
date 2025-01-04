# Simulate free throws
simulate_free_throws <- function(ft_percentage, num_shots) {
  successful_shots <- sum(runif(num_shots) < ft_percentage)
  return(successful_shots)
}

# Simulate possession
simulate_possession <- function(team_stats, opponent_stats) {
  score <- 0
  possession_active <- TRUE
  offensive_rebounds <- 0
  turnovers <- 0
  shots_attempted_2 <- 0
  shots_made_2 <- 0
  shots_attempted_3 <- 0
  shots_made_3 <- 0
  
  while (possession_active) {
    # Turnover check
    if (runif(1) < team_stats$turnover_rate) {
      turnovers <- turnovers + 1
      possession_active <- FALSE
      next
    }
    
    # Determine if the shot is a 2-pointer or 3-pointer (62-38% split)
    shot_type <- sample(c("2-pointer", "3-pointer"), size = 1, prob = c(0.62, 0.38))
    
    if (shot_type == "2-pointer") {
      shots_attempted_2 <- shots_attempted_2 + 1
      # Check if the shot is successful
      if (runif(1) < team_stats$FG) {
        score <- score + 2
        shots_made_2 <- shots_made_2 + 1
      }
      # Check for a foul (17% chance)
      if (runif(1) < 0.17) {
        score <- score + simulate_free_throws(team_stats$FT, num_shots = 2)
      }
    } else {
      shots_attempted_3 <- shots_attempted_3 + 1
      # Check if the shot is successful
      if (runif(1) < team_stats$TP) {
        score <- score + 3
        shots_made_3 <- shots_made_3 + 1
      }
      # Check for a foul (2% chance)
      if (runif(1) < 0.02) {
        score <- score + simulate_free_throws(team_stats$FT, num_shots = 3)
      }
    }
    
    # Offensive rebound check
    if (runif(1) < team_stats$offensive_rebound_rate) {
      offensive_rebounds <- offensive_rebounds + 1
      next
    }
    
    possession_active <- FALSE
  }
  
  return(list(score = score, 
              offensive_rebounds = offensive_rebounds, 
              turnovers = turnovers,
              shots_attempted_2 = shots_attempted_2,
              shots_made_2 = shots_made_2,
              shots_attempted_3 = shots_attempted_3,
              shots_made_3 = shots_made_3))
}

# Simulate game
simulate_game <- function(team_a_stats, team_b_stats) {
  avg_pace <- round((team_a_stats$pace + team_b_stats$pace) / 2)
  team_a_possessions <- avg_pace
  team_b_possessions <- avg_pace
  
  team_a_stats_total <- list(score = 0, offensive_rebounds = 0, turnovers = 0,
                             shots_attempted_2 = 0, shots_made_2 = 0, 
                             shots_attempted_3 = 0, shots_made_3 = 0, possessions = 0)
  team_b_stats_total <- team_a_stats_total
  
  for (i in 1:team_a_possessions) {
    result <- simulate_possession(team_a_stats, team_b_stats)
    team_a_stats_total$score <- team_a_stats_total$score + result$score
    team_a_stats_total$offensive_rebounds <- team_a_stats_total$offensive_rebounds + result$offensive_rebounds
    team_a_stats_total$turnovers <- team_a_stats_total$turnovers + result$turnovers
    team_a_stats_total$shots_attempted_2 <- team_a_stats_total$shots_attempted_2 + result$shots_attempted_2
    team_a_stats_total$shots_made_2 <- team_a_stats_total$shots_made_2 + result$shots_made_2
    team_a_stats_total$shots_attempted_3 <- team_a_stats_total$shots_attempted_3 + result$shots_attempted_3
    team_a_stats_total$shots_made_3 <- team_a_stats_total$shots_made_3 + result$shots_made_3
    team_a_stats_total$possessions <- team_a_stats_total$possessions + 1
  }
  
  for (i in 1:team_b_possessions) {
    result <- simulate_possession(team_b_stats, team_a_stats)
    team_b_stats_total$score <- team_b_stats_total$score + result$score
    team_b_stats_total$offensive_rebounds <- team_b_stats_total$offensive_rebounds + result$offensive_rebounds
    team_b_stats_total$turnovers <- team_b_stats_total$turnovers + result$turnovers
    team_b_stats_total$shots_attempted_2 <- team_b_stats_total$shots_attempted_2 + result$shots_attempted_2
    team_b_stats_total$shots_made_2 <- team_b_stats_total$shots_made_2 + result$shots_made_2
    team_b_stats_total$shots_attempted_3 <- team_b_stats_total$shots_attempted_3 + result$shots_attempted_3
    team_b_stats_total$shots_made_3 <- team_b_stats_total$shots_made_3 + result$shots_made_3
    team_b_stats_total$possessions <- team_b_stats_total$possessions + 1
  }
  
  data.frame(
    Team = c("Team A", "Team B"),
    Points = c(team_a_stats_total$score, team_b_stats_total$score),
    Possessions = c(team_a_stats_total$possessions, team_b_stats_total$possessions),
    Offensive_Rebounds = c(team_a_stats_total$offensive_rebounds, team_b_stats_total$offensive_rebounds),
    Turnovers = c(team_a_stats_total$turnovers, team_b_stats_total$turnovers),
    Two_Point_Attempts = c(team_a_stats_total$shots_attempted_2, team_b_stats_total$shots_attempted_2),
    Two_Point_Makes = c(team_a_stats_total$shots_made_2, team_b_stats_total$shots_made_2),
    Three_Point_Attempts = c(team_a_stats_total$shots_attempted_3, team_b_stats_total$shots_attempted_3),
    Three_Point_Makes = c(team_a_stats_total$shots_made_3, team_b_stats_total$shots_made_3)
  )
}



# Example usage
team_a_stats <- list(FG = 0.51, TP = 0.38, FT = 0.78, pace = 100, offensive_rating = 112,
                     defensive_rating = 108, turnover_rate = 0.10, offensive_rebound_rate = 0.10)
team_b_stats <- list(FG = 0.50, TP = 0.35, FT = 0.80, pace = 98, offensive_rating = 110,
                     defensive_rating = 110, turnover_rate = 0.10, offensive_rebound_rate = 0.10)

game_results <- simulate_game(team_a_stats, team_b_stats)
print(game_results)


