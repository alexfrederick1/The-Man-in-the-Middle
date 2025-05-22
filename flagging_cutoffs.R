library(arrow)
library(tidyverse)
library(ggforce)
library(gganimate)

data_directory <- "~/SMT-Data-Challenge-2025"

# load data 
game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                     partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                       partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),  
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_position <- data.frame(
  code = as.integer(c(1:13, 255, 14, 15, 16, 17, 18, 19)),
  position = c(
    "pitcher", "catcher", "first baseman", "second baseman", "third baseman",
    "shortstop", "left field", "center field", "right field", "batter",
    "runner on first base", "runner on second base", "runner on third base",
    "ball event with no player (e.g., ball bounce)", "home plate umpire",
    "field umpire", "field umpire", "field umpire",
    "first base coach", "third base coach"
  ),
  stringsAsFactors = FALSE
  
)

event_code <- data.frame(
  code = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16)),
  play_type = c(
    "pitch", "ball acquired", "throw (ball-in-play)", "ball hit into play", 
    "end of play", "pickoff throw", "ball acquired - unknown field position", 
    "throw (ball-in-play) - unknown field position", "ball deflection", 
    "ball deflection off of wall", "home run", "ball bounce"
  ),
  stringsAsFactors = FALSE
  
)

# filter for one game

big_ie <- game_events %>%
  left_join(game_info, by = c('game_str','play_per_game'), suffix = c("", "_dup")) %>%
  select(-ends_with("_dup")) %>%
  mutate(player_position = cast(player_position, int32())) %>%
  mutate(event_code = cast(event_code, int32())) %>%
  left_join(player_position, by = c('player_position' = 'code')) %>%
  left_join(event_code, by = c('event_code' = 'code')) %>% collect()

big_cutoffs <- big_ie %>%
  filter(!is.na(second_baserunner) | !is.na(first_baserunner)) %>%
  group_by(game_str, play_id) %>%
  filter(
    n_distinct(position[play_type == 'ball acquired']) >= 2,
    any(play_type == 'ball bounce'),
    any(position %in% c('left field', 'center field', 'right field'))
  ) %>%
  ungroup() %>%
  arrange(play_id, timestamp) %>%
  collect()

game_info_demo <- game_info %>% 
  filter(game_str == 'y1_d049_JZK_RZQ')

game_events_demo <- game_events %>%
  filter(game_str == 'y1_d049_JZK_RZQ')


info_events_1 <- game_events_demo %>%
  left_join(game_info_demo, by = c('play_per_game'), suffix = c("", "_dup")) %>%
  select(-ends_with("_dup")) %>%
  mutate(player_position = cast(player_position, int32())) %>%
  mutate(event_code = cast(event_code, int32())) %>%
  left_join(player_position, by = c('player_position' = 'code')) %>%
  left_join(event_code, by = c('event_code' = 'code')) %>%
  collect()

potential_cutoffs <- info_events_1 %>%
  filter(!is.na(second_baserunner) | !is.na(first_baserunner)) %>%
  group_by(play_id) %>%
  filter(
    n_distinct(position[play_type == 'ball acquired']) >= 2,
    any(play_type == 'ball bounce'),
    any(position %in% c('left field', 'center field', 'right field'))
  ) %>%
  ungroup() %>%
  arrange(play_id, timestamp) %>%
  collect()
  
  
  
  
  
  
  