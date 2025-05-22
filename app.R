library(shiny)
library(ggforce)
library(gganimate)
library(arrow)
library(shinycssloaders)
library(tidyverse)

data_directory <- "~/SMT-Data-Challenge-2025"

ui <- fluidPage(
  titlePanel("SMT Data Challenge Play Animation Viewer"),
  h3('By: Pranav Rajaram | @_pranavrajaram'),
  p('Animations take about 10 seconds to load.'),
  sidebarLayout(
    sidebarPanel(
      textInput("game_str", "Enter game_str:", value = "y1_d049_JZK_RZQ"),
      numericInput("play_id", "Enter play_id:", value = 135, min = 1)
    ),
    mainPanel(
      withSpinner(imageOutput("play_anim"), type = 1)
    )
  )
)

server <- function(input, output, session) {
  
  output$play_anim <- renderImage({
    
    # load data
    player_pos <- open_csv_dataset(paste0(data_directory,"/player_pos"),
                                   partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),
                                   col_names = c("game_str", "play_id", "timestamp",
                                                 "player_position", "field_x", "field_y"),
                                   hive_style = FALSE, unify_schemas = TRUE,
                                   na = c("", "NA", "NULL", NA, "\\N"))
    
    ball_pos <- open_csv_dataset(paste0(data_directory,"/ball_pos"),
                                 partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),
                                 col_names = c("game_str", "play_id", "timestamp",
                                               "ball_position_x", "ball_position_y", "ball_position_z"),
                                 hive_style = FALSE, unify_schemas = TRUE,
                                 na = c("", "NA", "NULL", NA, "\\N"))
    
    # filter data based on user input
    plot_df <- player_pos %>%
      filter(game_str == input$game_str, play_id == input$play_id) %>%
      collect()
    
    ball_df <- ball_pos %>%
      filter(game_str == input$game_str, play_id == input$play_id) %>%
      collect()
    
    offense <- c(10, 11, 12, 13)
    defense <- c(1:9)
    
    plot_df <- plot_df %>%
      mutate(field_x = as.numeric(field_x),
             field_y = as.numeric(field_y),
             timestamp = as.factor(timestamp),
             side = case_when(
               player_position %in% offense ~ "offense",
               player_position %in% defense ~ "defense",
               TRUE ~ "coach/ump"
             )) %>%
      filter(side != "coach/ump") %>%
      left_join(ball_df %>%
                  mutate(ball_position_x = as.numeric(ball_position_x),
                         ball_position_y = as.numeric(ball_position_y)) %>%
                  select(timestamp, ball_position_x, ball_position_y),
                by = "timestamp")
    
    # animation
    p <- ggplot() +
      geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 150, start = -pi/4, end = pi/4),
                   fill = "tan", color = "brown", inherit.aes = FALSE) +
      geom_segment(aes(x = 0, y = 0, xend = 250, yend = 250), color = "black") +
      geom_segment(aes(x = 0, y = 0, xend = -250, yend = 250), color = "black") +
      geom_point(aes(x = 0, y = 0), size = 4, shape = 15) +
      geom_point(aes(x = 63.64, y = 63.64), size = 4, shape = 15) +
      geom_point(aes(x = 0, y = 127), size = 4, shape = 15) +
      geom_point(aes(x = -63.64, y = 63.64), size = 4, shape = 15) +
      geom_point(data = plot_df, aes(x = ball_position_x, y = ball_position_y),
                 color = "gray36", size = 4, shape = 21, stroke = 1.5, fill = "gray83") +
      geom_point(data = plot_df, aes(x = field_x, y = field_y, color = side), size = 3) +
      coord_fixed() +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      labs(title = 'Timestamp: {closest_state}', x = "Field X", y = "Field Y") +
      transition_states(timestamp, transition_length = 1, state_length = 1) +
      ease_aes('linear')
    
    # save to temp file and render
    outfile <- tempfile(fileext = ".gif")
    anim_save(outfile, animate(p, fps = 10, width = 800, height = 600))
    
    list(src = outfile, contentType = 'image/gif')
  }, deleteFile = TRUE)
}

shinyApp(ui = ui, server = server)
