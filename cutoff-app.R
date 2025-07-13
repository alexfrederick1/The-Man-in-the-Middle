library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggplot2)

# Load data with filtering for selected teams
team_df <- read_csv("team_cutoff_analysis.csv") %>%
  filter(fielding_team %in% c('QEA', 'YJD', 'RZQ'))

fielder_df <- read_csv("~/SMT-Data-Challenge-2025/fielder_eval.csv")  %>%
  filter(team %in% c('QEA', 'YJD', 'RZQ')) %>%
  filter(total_actions >= 4)

all_mistakes <- read_csv('fielder_mistakes_grouped.csv')

ui <- dashboardPage(
  dashboardHeader(title = "Cutoff Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Analysis", tabName = "team", icon = icon("users")),
      menuItem("Fielder Analysis", tabName = "fielder", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Team Tab
      tabItem(
        tabName = "team",
        fluidRow(
          box(
            title = "Select Team", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("team_select", "Team:", choices = unique(team_df$fielding_team))
          ),
          valueBoxOutput("team_plays"),
          valueBoxOutput("team_accuracy"),
          valueBoxOutput("team_ev_loss")
        ),
        fluidRow(
          box(
            title = "Most Vulnerable Paths", width = 6, 
            plotOutput("team_accuracy_bar")
          ),
          box(
            title = "EV Loss Distribution (All Teams)", width = 6, 
            plotOutput("ev_loss_plot")
          )
        )
      ),
      
      # Fielder Tab
      tabItem(
        tabName = "fielder",
        fluidRow(
          box(
            title = "Select Fielder", status = "info", solidHeader = TRUE, width = 4,
            selectInput("fielder_select", "Fielder (cutoff_id):", choices = unique(fielder_df$cutoff_id))
          ),
          valueBoxOutput("fielder_plays"),
          valueBoxOutput("fielder_accuracy"),
          valueBoxOutput("fielder_position")
        ),
        fluidRow(
          box(
            title = "Fielder Accuracy Comparison (Top 20)", width = 6, 
            plotOutput("fielder_accuracy_plot")
          ),
          box(
            title = "Most Common Mistakes (Selected Fielder)", width = 6, 
            plotOutput("fielder_mistake_plot")
          )
        ),
        fluidRow(
          box(
            title = "Overall Most Common Mistakes (All Fielders)", width = 12,
            plotOutput("overall_mistake_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  ### TEAM TAB ###
  # TEAM VALUEBOXES
  output$team_plays <- renderValueBox({
    plays <- team_df %>% filter(fielding_team == input$team_select) %>% pull(total_cutoff_plays)
    valueBox(plays, "Total Plays", icon = icon("baseball-ball"), color = "blue")
  })
  
  output$team_accuracy <- renderValueBox({
    acc <- team_df %>% filter(fielding_team == input$team_select) %>% pull(accuracy_rate)
    valueBox(scales::percent(acc), "Optimal Decision Rate", icon = icon("check-circle"), color = "green")
  })
  
  output$team_ev_loss <- renderValueBox({
    loss <- team_df %>% filter(fielding_team == input$team_select) %>% pull(avg_ev_penalty)
    valueBox(round(loss, 3), "Avg EV Loss", icon = icon("chart-line"), color = "red")
  })
  
  # TEAM PLOTS with highlighting for selected team:
  output$ev_loss_plot <- renderPlot({
    team_df %>%
      mutate(highlight = ifelse(fielding_team == input$team_select, "Selected", "Other")) %>%
      ggplot(aes(x = reorder(fielding_team, -avg_ev_penalty), y = avg_ev_penalty, fill = highlight)) +
      geom_col() +
      scale_fill_manual(values = c("Selected" = "#FFCD00", "Other" = "#182B49")) +
      labs(x = "Team", y = "Avg EV Loss", title = "EV Loss by Team") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position = "none")
  })
  
  output$team_accuracy_bar <- renderPlot({
    team_df %>%
      filter(fielding_team == input$team_select) %>%
      select(fielding_team, contains("->")) %>%
      pivot_longer(cols = contains("->"),
                   names_to = "basepath",
                   names_prefix = "ev_loss_",
                   values_to = "ev_loss") %>%
      ggplot(aes(x = reorder(basepath, -ev_loss), y = ev_loss)) +
      geom_col(fill = "mediumpurple") +
      labs(
        x = "Base Running Path",
        y = "Avg EV Loss",
        title = paste("EV Loss by Baserunner Path —", input$team_select)
      ) +
      theme_minimal() +
      coord_flip()
  })
  
  ### FIELDER TAB ###
  # FIELDER VALUEBOXES
  output$fielder_plays <- renderValueBox({
    plays <- fielder_df %>% filter(cutoff_id == input$fielder_select) %>% pull(total_actions)
    valueBox(plays, "Total Plays", icon = icon("person-running"), color = "purple")
  })
  
  output$fielder_accuracy <- renderValueBox({
    acc <- fielder_df %>% filter(cutoff_id == input$fielder_select) %>% pull(accuracy)
    valueBox(scales::percent(acc), "Optimal Decision Rate", icon = icon("check"), color = "green")
  })
  
  output$fielder_position <- renderValueBox({
    pos <- fielder_df %>% filter(cutoff_id == input$fielder_select) %>% pull(position) %>% unique()
    valueBox(ifelse(length(pos) > 0, pos, NA), "Position", icon = icon("id-badge"), color = "yellow")
  })
  
  # FIELDER PLOTS: Top 20 Fielder Accuracy with highlighting for selected fielder.
  output$fielder_accuracy_plot <- renderPlot({
    top20 <- fielder_df %>%
      arrange(desc(accuracy)) %>%
      slice_head(n = 28) %>%
      mutate(highlight = ifelse(cutoff_id == input$fielder_select, "Selected", "Other"))
    
    ggplot(top20, aes(x = reorder(cutoff_id, accuracy), y = accuracy, fill = highlight)) +
      geom_col() +
      scale_fill_manual(values = c("Selected" = "#FFCD00", "Other" = "#182B49")) +
      coord_flip() +
      labs(x = "Fielder", y = "Optimal %", title = "Top 20 Fielder Accuracies") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # FIELDER MISTAKES: Show most common mistakes for selected fielder.
  output$fielder_mistake_plot <- renderPlot({
    fielder_mistakes <- all_mistakes %>%
      filter(cutoff_id == input$fielder_select, !is.na(mistake_type))
    
    ggplot(fielder_mistakes, aes(x = reorder(mistake_type, count), y = count)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(x = "Mistake Type", y = "Count", title = paste("Mistakes for", input$fielder_select)) +
      theme_minimal()
  })
  
  # Overall mistakes across all fielders
  output$overall_mistake_plot <- renderPlot({
    overall <- all_mistakes %>%
      filter(!is.na(mistake_type)) %>%
      group_by(mistake_type) %>%
      summarize(total_count = sum(count, na.rm = TRUE)) %>%
      arrange(desc(total_count)) %>%
      slice_head(n = 10)
    
    ggplot(overall, aes(x = reorder(mistake_type, total_count), y = total_count)) +
      geom_col(fill = "darkorchid") +
      coord_flip() +
      labs(x = "Mistake Type", y = "Total Count", title = "Overall Most Common Mistakes") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
