library(shiny)
library(dplyr)
library(DT) 
library(ggplot2)
library(ggsoccer)
library(viridis)


# Sample data (replace with your actual data)
data <- read.csv("combined_data.csv")

ui <- fluidPage(
  fluidRow(
    column(6, 
           titlePanel("Premier League Offensive Insights Hub"),
           h4("An all-encompassing solution/tool for dissecting offensive strategies within the league. 
              Dive into the offensive data for each season, individual players")  # Subheading
           # Title Panel
    ),
    column(6, div(class = "header-images",
                  tags$img(src = "t2.jpg", style = "width: 100%; height: auto;")
    ))
  ), 
  # Top panel with buttons
  fluidRow(
    column(12,
           wellPanel(
             actionButton("season_stats", "Season Stats", class = "btn-primary"),
             actionButton("player_stats", "Player Stats", class = "btn-primary")#,
           #  actionButton("team_stats", "Team Stats", class = "btn-primary")
           )
    )
  ),
  
  # Footer
  tags$footer(
    style = "position: fixed; left: 0; bottom: 0; width: 100%; text-align: center; background-color: lightgray; color: black; padding: 10px;",
    "© 2024 KBanalytics"
  ),
  
  # Gap between the buttons and the output panel
  tags$hr(),
  
  # Panel for Season Stats
  uiOutput("season_stats_panel"),
  
  # Panel for Player Stats
  uiOutput("player_stats_panel")
)




server <- function(input, output, session) {
  active_panel <- reactiveVal("none")
  
  observeEvent(input$season_stats, {
    active_panel("season")
  })
  
  observeEvent(input$player_stats, {
    active_panel("player")
  })
  
  # Dynamic UI for Season Stats
  output$season_stats_panel <- renderUI({
    if (active_panel() == "season") {
      fluidRow(
        column(12,
               span("Examine the offensive performance metrics for a particular season: "),
               selectInput("season", "", choices = c("", sort(unique(data$season), decreasing = TRUE)), selected = sort(unique(data$season), decreasing = TRUE)[1]),
               DTOutput("summary_table")
        )
      )
    }
  })

  #dynamic UI for player stats
  output$player_stats_panel <- renderUI({
    if (active_panel() == "player") {
      fluidRow(
        column(3, selectInput("player_search", "Search for Player:", choices = c("", unique(data$player)), selected = "Erling Haaland")),
        column(2, selectInput("season_player", "Select EPL Season:", choices = c("", unique(data$season)), selected = sort(unique(data$season), decreasing = TRUE)[1])),
        column(8, DTOutput("player_stats_table")),
        column(6,
               wellPanel(
                 plotOutput("player_plot"),
                 title = "Player Plot"  # You can add a title for this panel
               )
        ),
        column(6,
               wellPanel(
                 plotOutput("goal_minute_plot"),
                 title = "Goal Minute Plot"  # You can add a title for this panel
               )
        )
      )
    }
  })
  

  # Define a reactive expression for season data
  season_summary_data <- reactive({
    req(input$season)  # Ensure a season is selected
    # Filter the data based on the selected season
    selected_season_data <- data %>%
      filter(season == input$season)
    
    # Calculate the total assists and other stats within the reactive context
    assist_counts <- selected_season_data %>%
      filter(!is.na(player_assisted), result == "Goal") %>%
      group_by(player_assisted) %>%
      summarize(Total_Assists = n()) %>%
      ungroup()
    
    
    player_stats <- selected_season_data %>%
      group_by(player) %>%
      summarize(
        Total_Goals = sum(result == "Goal", na.rm = TRUE),
        Total_xG = sum(xG, na.rm = TRUE),
        Average_xG = mean(xG, na.rm = TRUE),
        Total_Shots = n(),
        Shots_on_TargetPg = sum(result == "Goal" | result == "SavedShot") / n_distinct(match_id),
        Conversion_Rate = (Total_Goals / Total_Shots) * 100,
        Average_Minute_per_Goal = mean(minute[result == "Goal"]),
        Home_Goals = sum(result == "Goal" & h_a == "h"),
        Away_Goals = sum(result == "Goal" & h_a == "a"),
        Penalty_Scored = sum(result == "Goal" & situation == "Penalty"),  # Count of penalties scored
        Penalty_Taken = sum(situation == "Penalty"),  # Count of penalties taken
        Headers = sum(result == "Goal" & shotType == "Head" )
      ) %>%
      ungroup() %>%
      mutate(
        Total_xG = round(Total_xG, 2),
        Average_xG = round(Average_xG, 2),
        Conversion_Rate = round(Conversion_Rate, 2),
        Shots_on_TargetPg = round(Shots_on_TargetPg, 2),
        Average_Minute_per_Goal = round(Average_Minute_per_Goal, 2),
        Penalties_Converted = paste(Penalty_Scored, Penalty_Taken, sep = "/")  # Combine counts
      )
    
    
    # Merge and return the final data
    player_stats %>%
      left_join(assist_counts, by = c("player" = "player_assisted")) %>%
      select(player, Total_Goals, Total_Assists, Total_xG, Average_xG, Total_Shots, 
             Shots_on_TargetPg, Conversion_Rate, Headers,Average_Minute_per_Goal,
             Home_Goals, Away_Goals, Penalties_Converted) %>%
      arrange(desc(Total_Goals))
  })
  
  # Render the summary table for the selected season
  output$summary_table <- renderDT({
    datatable(season_summary_data(), options = list(pageLength = 20, autoWidth = TRUE))
  })
  
  player_specific_season_data <- reactive({
    req(input$player_search, input$season_player)
    # Filter with standardized values
    selected_player_season_data <- data %>%
      filter(player == input$player_search, season == input$season_player)
    
    # Calculate statistics for the selected player
    assist_counts <- data %>%
      group_by(player_assisted) %>%
      filter(result == "Goal", !is.na(player_assisted)) %>%
      summarise(Total_Assists = n())%>%
      ungroup()
   
    player_stats <- selected_player_season_data %>%
      group_by(player) %>%
      summarize(
        Total_Goals = sum(result == "Goal", na.rm = TRUE),
        Total_xG = sum(xG, na.rm = TRUE),
        Average_xG = mean(xG, na.rm = TRUE),
        Total_Shots = n(),
        Shots_on_TargetPg = sum(result == "Goal" | result == "SavedShot") / n_distinct(match_id),
        Conversion_Rate = (Total_Goals / Total_Shots) * 100,
        Average_Minute_per_Goal = mean(minute[result == "Goal"]),
        Home_Goals = sum(result == "Goal" & h_a == "h"),
        Away_Goals = sum(result == "Goal" & h_a == "a"),
        Penalty_Scored = sum(result == "Goal" & situation == "Penalty"),  # Count of penalties scored
        Penalty_Taken = sum(situation == "Penalty"),  # Count of penalties taken
        Headers = sum(result == "Goal" & shotType == "Head" )
      ) %>%
      ungroup() %>%
      mutate(
        Total_xG = round(Total_xG, 2),
        Average_xG = round(Average_xG, 2),
        Conversion_Rate = round(Conversion_Rate, 2),
        Shots_on_TargetPg = round(Shots_on_TargetPg, 2),
        Average_Minute_per_Goal = round(Average_Minute_per_Goal, 2),
        Penalties_Converted = paste(Penalty_Scored, Penalty_Taken, sep = "/")  # Combine counts
      )
    # Merge and return the final data
    player_stats %>%
      left_join(assist_counts, by = c("player" = "player_assisted")) %>%
      select(player, Total_Goals, Total_Assists, 
             Total_xG, Average_xG, Total_Shots, Shots_on_TargetPg, Conversion_Rate,
             Headers,Average_Minute_per_Goal,Home_Goals, Away_Goals, Penalties_Converted)
    
  })
  # Plot
  player_specific_plot <- reactive({
    req(input$player_search, input$season_player)
    # Filter with standardized values
    plot_data <- data %>%
      filter(player == input$player_search, season == input$season_player)
    
    ggplot(plot_data, aes(x = X*100, y = Y*100, color = result, size = xG)) +
      annotate_pitch(colour = "white",
                     fill   = "seagreen",
                     limits = FALSE) +
      geom_point() +
      theme_pitch() +
      theme(panel.background = element_rect(fill = "seagreen"),
            legend.text = element_text(size = 12)) +
      coord_flip(xlim = c(49, 101)) +
      scale_y_reverse() +
      scale_color_manual(values = c("Goal" = "chartreuse", 
                                    "MissedShots" = "orangered1", 
                                    "SavedShot" = "blue", 
                                    "BlockedShot" = "yellow",
                                    "OwnGoal" = "pink",
                                    "ShotOnPost" = "peru"))+
      labs(title = paste("Shot Map of", input$player_search))
  })
  
  player_goal_min__plot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter with standardized values
    plot_data <- data %>%
      filter(player == input$player_search, season == input$season_player, 
             result == "Goal")
    
    ggplot(plot_data, aes(x = minute, y = ..count..)) +
      geom_freqpoly(binwidth = 5, aes(color = ..count..)) +
      scale_color_viridis(option = "magma", begin = 0.3, end = 0.9, direction = 1) +
      scale_x_continuous(breaks = seq(0, 120, by = 5)) +
      labs(title = "Distribution of Goals Scored by Minute",
           x = "Minute of the Match",
           y = "Number of Goals") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "black", color = "grey"),
            plot.background = element_rect(fill = "grey", color = "grey"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) # Change fill to the desired color
  })
  
  
  player_key_pass_plot <- reactive({
    req(input$player_search, input$season_player)
    # Filter to only include rows where a player_assisted is available
    valid_passes <- data %>% 
      filter(player_assisted == input$player_search, season == input$season_player, result %in% c("Goal", "SavedShot", "MissedShots"))
    
    assist_counts <- data %>%
      group_by(player_assisted) %>%
      filter(result == "Goal", !is.na(player_assisted)) %>%
      summarise(Total_Assists = n())%>%
      ungroup()
    
    # Plot the number of assist attempts
    ggplot(valid_passes, aes(x = result, y = ..count..)) +
      geom_bar(stat = "identity") +
      coord_flip() +  # Flip coordinates to make it easier to read player names
      labs(x = "Player Assisted", y = "Assist Attempts", title = "Number of Key Passes per Player") +
      theme_minimal()
  })
  
  
  
  # Render the player specific data table
  output$player_stats_table <- renderDT({
    data <- player_specific_season_data()
    if(is.data.frame(data) && nrow(data) > 0) {
      datatable(data)
    } else {
      # No data available, display an appropriate message or an empty table
      datatable(data.frame(message = "No data available for the selected player and season."))
    }
  })
  
  # Render the player-specific plot
  output$player_plot <- renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    player_specific_plot()  # Pass player_search as an argument to the reactive function
  })
  
  
  output$goal_minute_plot <- renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    player_goal_min__plot()  # Pass player_search as an argument to the reactive function
  })
  
  output$key_pass_plot <-renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    player_key_pass_plot()
  })
}


shinyApp(ui, server)
