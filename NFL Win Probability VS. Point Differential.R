
library(dplyr)
library(nflreadr)
library(ggplot2)
library(nflplotR)


games <- nflreadr::load_schedules(2019:2024)
str(games)

# Assuming your data frame is called 'df' and the column is named 'column_name'
unique_values <- unique(games$season)

# Print the unique values
print(unique_values)


home <- games %>%
  filter(game_type == 'REG') %>% 
  filter(!is.na(home_score) & !is.na(away_score)) %>%
  select(season, week, home_team, result, week) %>%
  rename(team = home_team)
home %>% head(5)



away <- games %>%
  filter(game_type == 'REG') %>%
  filter(!is.na(home_score) & !is.na(away_score)) %>% 
  select(season, week, away_team, result, week) %>%
  rename(team = away_team) %>%
  mutate(result = -result)
away %>% head(5)



results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0
    ),
    loss = case_when(
      result < 0 ~ 1,
      result > 0 ~ 0,
      result == 0 ~ 0
    ),
    tie = case_when(
      result < 0 ~ 0,
      result > 0 ~ 0,
      result == 0 ~ 1
    )
  )




#results <- results %>% filter(week <= 18)



team_wins <- results %>%
  group_by(team, season) %>%
  summarize(
    wins = sum(win),
    losses = sum(loss),
    ties = sum(tie),
    point_diff = sum(result)) %>%
  ungroup()

team_wins$games_played = team_wins$wins + team_wins$losses + team_wins$ties
team_wins$win_percentage = (team_wins$wins + 0.5*(team_wins$ties))/team_wins$games_played
team_wins$avg_point_diff = team_wins$point_diff/team_wins$games_played

team_wins <- team_wins %>%
  arrange(-win_percentage, avg_point_diff)

nine_wins_list <- team_wins %>% 
  filter(wins==9, losses==0, ties==0)



week_minimum = 9
week_maximum = 18

df <- data.frame()

for(i in week_minimum:week_maximum) {
  
  if (i>10) {
    filtered_results <- results %>% filter(season!=2024)
    filtered_results <- filtered_results %>% filter(week <= i)
  }else {
    filtered_results <- results %>% filter(week <= i)
  } #2024 season has only had 10 weeks, no need to run the other weeks for 2024

  
  team_results <- filtered_results %>%
    group_by(team, season) %>%
    summarize(
      wins = sum(win),
      losses = sum(loss),
      ties = sum(tie),
      point_diff = sum(result)) %>%
    ungroup()
  
  team_results$games_played = team_results$wins + team_results$losses + team_results$ties
  team_results$win_percentage = (team_results$wins + 0.5*(team_results$ties))/team_results$games_played
  team_results$avg_point_diff = team_results$point_diff/team_results$games_played
  team_results$week = i
  
  team_results <- team_results %>%
    arrange(-win_percentage, avg_point_diff)
  
  df <- rbind(df, team_results)
  
}


df <- df %>%
  arrange(-win_percentage, avg_point_diff)



  # Add a new column to identify whether each team-year combination should be highlighted
  over_700$highlight <- ifelse(
    (over_700$team == "PHI" & over_700$season == 2023) |
      (over_700$team == "KC" & over_700$season == 2024) |
      (over_700$team == "MIN" & over_700$season == 2022),
    over_700$team, 
    "Other"
  )
  
  # over_700 <- over_700 %>%
  #   mutate(highlight = recode(highlight, "KC" = "2024 KC", "PHI" = "2023 PHI", "MIN" = "2022 MIN"))
  
  # Create the plot
  ggplot(over_700, aes(x = win_percentage, y = avg_point_diff)) +
    geom_point(aes(color = highlight), size = 2, show.legend = TRUE) +
    nflplotR::scale_color_nfl(type = "primary") +
    
    labs(
      title = "Win Percentage vs. Point Differential Per Game",
      x = "Win Percentage",
      y = "Point Differential Per Game",
      color = "Legend"
    ) +
    theme_minimal()
  
  
  
  
  library(ggplot2)
  library(nflplotR)
  
  # Create the plot with conditional text labels
  ggplot(over_700, aes(x = win_percentage, y = avg_point_diff)) +
    geom_point(aes(color = highlight), size = 2, show.legend = TRUE) +
    nflplotR::scale_color_nfl(type = "primary") +
    
    # Add text labels conditionally based on the 'highlight' variable
    geom_text(aes(label = ifelse(highlight != "Other", highlight, NA)), 
              vjust = -1,  # Adjust vertical position
              hjust = 0,   # Adjust horizontal position
              color = "black", # Text color
              size = 3) +  # Adjust text size
    
    labs(
      title = "Win Percentage vs. Point Differential Per Game",
      x = "Win Percentage",
      y = "Point Differential Per Game",
      color = "Legend"
    ) +
    theme_minimal()
  
  
  


# 
# 
# # Load the ggplot2 package
# library(ggplot2)
# 
# # Example data frame
# df <- data.frame(
#   win_percentage = c(0.6, 0.7, 0.8, 0.5, 0.9),
#   point_differential = c(5, 10, 15, -3, 20)
# )
# 
# # Create the improved plot
# ggplot(df, aes(x = win_percentage, y = avg_point_diff)) +
#   geom_point(size = 4, color = "#2c7fb8", alpha = 0.8) +      # Customize points with color and transparency
#   geom_smooth(method = "lm", color = "#feb24c", linetype = "dashed", se = FALSE) + # Add regression line
#   labs(
#     title = "Win Percentage vs. Point Differential",
#     subtitle = "Analyzing the relationship between team success and point differential",
#     x = "Win Percentage",
#     y = "Point Differential Per Game"
#   ) +
#   theme_minimal(base_size = 14) + # Use a minimal theme with a larger font size
#   theme(
#     plot.title = element_text(face = "bold", size = 16, hjust = 0.5),     # Bold and center the title
#     plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"), # Center and style subtitle
#     axis.title.x = element_text(margin = margin(t = 10)), # Add space between axis and label
#     axis.title.y = element_text(margin = margin(r = 10)),
#     panel.grid.major = element_line(color = "gray80"), # Light gray grid lines for a cleaner look
#     panel.grid.minor = element_blank()                 # Remove minor grid lines
#   )



#Create a table of teams of all winning percentages/records and their point differentials
#Chart it
#Maybe initial filter of 8 games played minimum 
#highest win percentage/lowest point differential 
#Chart with x-axis of win percentage
#Chart with y-axis of point differential