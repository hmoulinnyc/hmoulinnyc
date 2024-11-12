# Install necessary packages (if you haven't already)
#install.packages("dplyr")
#devtools::install_github("nflverse/nflreadr")  # if not already installed
library(dplyr)
library(nflreadr)

# Load the schedules for 2000 to 2024
schedules <- load_schedules(2000:2024)

team_record <- filter(schedules, !is.na(home_score) & !is.na(away_score))

mutated <- mutate(team_record, home_win = ifelse(home_score > away_score, 1, 0), away_win = ifelse(away_score > home_score, 1, 0))

group_by(mutated, season, home_team)

# Calculate the win/loss record for each team by season
team_records <- schedules %>%
  filter(!is.na(home_score) & !is.na(away_score)) %>%  # Filter out games with no scores
  mutate(
    home_win = ifelse(home_score > away_score, 1, 0),
    away_win = ifelse(away_score > home_score, 1, 0)
  ) %>%
  group_by(season, home_team) %>%
  summarize(home_wins = sum(home_win), home_losses = n() - sum(home_win)) %>%
  bind_rows(
    schedules %>%
      mutate(
        away_win = ifelse(away_score > home_score, 1, 0),
        away_losses = ifelse(away_score < home_score, 1, 0)
      ) %>%
      group_by(season, away_team) %>%
      summarize(away_wins = sum(away_win), away_losses = n() - sum(away_win))
  ) %>%
  ungroup()

# Now filter for teams that went 11-0 (home or away)
teams_11_0 <- team_records %>%
  filter((home_wins == 11 & home_losses == 0) | (away_wins == 11 & away_losses == 0))

# View the results
print(teams_11_0)
