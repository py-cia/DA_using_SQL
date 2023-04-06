# rm(list = ls())
library(RSQLite)
library(DBI)
library(dplyr)
library(tidyverse)

# connect to your SQL database
con <- (dbConnect(
  drv = SQLite(),
  dbname = "database.sqlite"
))

# List all tables in the data base
dbListTables(con)

# tbl displays a "head()" of the table
tbl(con, "Country")

tbl(con, "League")

# we want a table with id, country name, league_name, season, stage, date, home_team,
# away_team, and scores from home_team_goal and away_team_goal

list_tbl <- dbListTables(con)
list_tbl

# has country name and country id
tbl(con, "Country")

# has league name and country id
tbl(con, "League")

# has season, stage, date, home_team, away_team, and goals
tbl(con, "Match")

# Had to use join twice on team table
detailed_matches <- dbGetQuery(con, "SELECT
                 date,
                 stage,
                 season,
                 Country.name AS country_name,
                 League.name AS league_name,
                 HT.team_long_name AS home_team,
                 AT.team_long_name AS away_team,
                 home_team_goal AS home_goal,
                 away_team_goal AS away_goal
                 FROM Match
                 JOIN Country ON Match.country_id = Country.id
                 JOIN League ON Match.league_id = League.country_id
                 JOIN Team AS HT ON Match.home_team_api_id = HT.team_api_id
                 JOIN Team AS AT ON Match.away_team_api_id = AT.team_api_id")

# at this point I can use R or stick with SQL to perform analysis. I will do both. First with R

# save my results from before into a data frame using the collect function
detailed_matches_df <- detailed_matches %>% collect()
print(head(detailed_matches_df))

detailed_matches_df %>% group_by(season, country_name, league_name) %>%
                        summarise(stage = length(unique(stage)),
                                  n_teams = n_distinct(home_team),
                                  avg_home_goal = mean(home_goal),
                                  avg_away_goal = mean(away_goal),
                                  total_goal = sum(home_goal + away_goal),
                                  avg_total_goal = mean(home_goal + away_goal),
                                  avg_goal_diff = mean(home_goal - away_goal)) %>% 
                        filter(country_name %in% c("Spain", "Germany", "France", "Italy", "England"), stage > 10) %>%
                        arrange(country_name, league_name, desc(season)) %>% collect()

# SQL time
dbGetQuery(con, "SELECT season,
                 Country.name AS country_name,
                 count(DISTINCT HT.team_long_name) AS n_teams,
                 count(DISTINCT stage) AS n_stage,
                 avg(home_team_goal) AS avg_home_goal,
                 avg(away_team_goal) AS avg_away_goal,
                 avg(home_team_goal - away_team_goal) AS avg_diff_goal,
                 sum(home_team_goal + away_team_goal) AS total_goal,
                 avg(home_team_goal + away_team_goal) AS avg_goal
                 FROM Match
                 JOIN League ON Match.league_id = League.country_id
                 JOIN Country ON Match.country_id = Country.id
                 JOIN Team AS HT ON Match.home_team_api_id = HT.team_api_id
                 JOIN Team AS AT ON Match.away_team_api_id = AT.team_api_id
                 WHERE country_name in ('Spain', 'Germany', 'France', 'Italy', 'England')
                 GROUP BY Country.name, League.name, season
                 HAVING count(DISTINCT stage) > 10
                 ORDER BY Country.name, League.name, season DESC")
                 
# Plots using ggplot
# arrange again so that season is in chronological order
df <- detailed_matches_df %>% group_by(season, country_name, league_name) %>%
  summarise(stage = length(unique(stage)),
            n_teams = n_distinct(home_team),
            avg_home_goal = mean(home_goal),
            avg_away_goal = mean(away_goal),
            total_goal = sum(home_goal + away_goal),
            avg_total_goal = mean(home_goal + away_goal),
            avg_goal_diff = mean(home_goal - away_goal)) %>% 
  filter(country_name %in% c("Spain", "Germany", "France", "Italy", "England"), stage > 10) %>%
  arrange(country_name, league_name, season) %>% collect()

# be careful, season is not numeric
ggplot(data = df, mapping = aes(x = season, y = avg_total_goal, color = country_name)) +
  geom_line(mapping = aes(group = country_name)) +
  labs(title = "Average Goals per Game Over Time")

ggplot(data = df, mapping = aes(x = season, y = avg_goal_diff, color = country_name)) +
  geom_line(mapping = aes(group = country_name)) +
  labs(title = "Average Goal Difference Over Time")

# using R 
temp <- tbl(con, "Player_Attributes")

# uses the latest player record for player data
grp_df <- temp %>%
          group_by(player_api_id) %>%
          filter(row_number() == 1) %>% collect()
# time to join
copy_to(con, grp_df, temporary = FALSE)
dbListTables(con)

player_stat <- dbGetQuery(con, "SELECT player_name,
                 height,
                 weight,
                 overall_rating,
                 potential
                 FROM Player
                 JOIN grp_df on Player.player_api_id = grp_df.player_api_id") %>% 
                 collect()
player_stat[2] <- round(player_stat$height)

player_stat$height[player_stat$height < 165] = 165
# making sure it worked
min(player_stat$height)

player_stat$height[player_stat$height > 195] = 195
max(player_stat$height)

sum_df <- player_stat %>%
  group_by(height) %>%
  summarise(avg_weight = mean(weight),
            avg_rating = mean(overall_rating),
            avg_potential = mean(potential),
                        n = length(height)) %>% collect()

# Using SQL
dbGetQuery(con, "SELECT 
       avg(weight) AS avg_weight,
       avg(overall_rating) AS avg_overall_rating,
       avg(potential) AS avg_potential_rating,
       CASE 
       WHEN ROUND(height) < 165 THEN 165
       WHEN ROUND(height) > 195 THEN 195
       ELSE ROUND(height)
       END AS calc_height,
       COUNT(height)
       FROM Player
       JOIN
       (SELECT MIN(player_api_id) AS first_player_record,
       player_api_id,
       overall_rating,
       potential
       FROM Player_Attributes
       GROUP BY player_api_id)
       AS temp ON Player.player_api_id = temp.player_api_id
       GROUP BY calc_height")

# similar to R
ggplot(data = sum_df, mapping = aes(x = height, y = avg_potential)) +
  geom_line() + 
  labs(title = "Potential vs Height", x = "Height", y = "Avg_Potential") + 
  theme(panel.background = element_rect(fill = NA))




