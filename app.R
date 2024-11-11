library(shiny)
library(readxl)
library(DT) 
library(dplyr)
library(ggplot2)
library(caTools)
library(shinydashboard)
library(shinyWidgets) 
library(shinyBS)
library(plotly)
library(shinyjs)
library(igraph)
library(viridis)
library(ggsoccer)


# Sample data (replace with your actual data)
data <- read.csv("combined_data.csv")
sorted_players <- sort(unique(data$Player))
# Define a named vector for situation labels
situation_labels <- c("OpenPlay" = "Open Play", 
                      "FromCorner" = "From Corner", 
                      "SetPiece" = "Set Piece", 
                      "Penalty" = "Penalty")
data$situation <- situation_labels[data$situation]
# Define a named vector for shotType labels
shot_type_labels <- c(
  "Head" = "Header",
  "LeftFoot" = "Left Foot",
  "RightFoot" = "Right Foot",
  "OtherBodyPart" = "Other Body Part"
)
data$shotType <- shot_type_labels[data$shotType]
data$xG <- round(data$xG, 2)


ui <- dashboardPage(
  dashboardHeader(title = "Premier League Insights Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "title_page", icon = icon("home")),
      menuItem("Player Stats", tabName = "player_stats", icon = icon("user")),
      menuItem("Season Stats", tabName = "season_stats", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
    #no_data_message {
      color: red;
      font-weight: bold;
      font-size: 20px;
      text-align: center;
    }
  ")),
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background: url('t11.jpg') no-repeat center center fixed;
          background-size: cover;
        }
        .card {
          padding: 20px;
          margin: 20px;
          border-radius: 20px;  /* Changed to give a rectangular card look */
          text-align: center;
          font-size: 22px;
          color: white;
          background: rgba(52, 152, 219, 0.85);  /* Semi-transparent background */
          transition: all 0.3s ease;
          box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2); /* Added box-shadow */
        }
        .card:hover {
          background: rgba(46, 204, 113, 0.85);
          cursor: pointer;
          box-shadow: 0px 8px 16px rgba(0, 0, 0, 0.4);  /* Hover shadow effect */
        }
        .card .icon {
          font-size: 70px;  /* Increased icon size */
          margin-bottom: 10px;
        }
        .card p {
          font-size: 28px;
          font-weight: bold;
        }
        .title-text {
          text-align: center;
          font-size: 48px;  /* Increased the title size */
          font-weight: bold;
          margin-top: 20px;
          color: #f9f9f9;  /* Use a brighter color for better contrast */
          text-shadow: 3px 3px 6px rgba(0, 0, 0, 0.7);  /* Stronger shadow for contrast */
          background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent black background */
          padding: 10px;
          border-radius: 10px;
        }
        .subtitle-text {
          text-align: center;
          font-size: 20px;
          color: #f9f9f9;  /* Use a brighter color for better contrast */
          margin-bottom: 40px;
          text-shadow: 2px 2px 5px rgba(0, 0, 0, 0.7);  /* Stronger shadow */
          background-color: rgba(0, 0, 0, 0.4); /* Semi-transparent black background */
          padding: 10px;
          border-radius: 8px;
        }
        .logo-img {
          display: block;
          margin: 0 auto;
          width: 70%;  /* Increased the logo size */
          border-radius: 15px;  /* Added border-radius */
          box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.6);  /* Shadow around the logo */
        }
        .text-center img {
          border-radius: 15px;
          box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.6); /* Shadow for images */
        }
      "))
    ),
    tabItems(
      # Title Page Tab
      tabItem(
        tabName = "title_page",
        fluidRow(
          column(12, div(class = "title-text", "Welcome to Premier League Offensive Insights Dashboard")),
          column(12, div(class = "subtitle-text", "Analyze player performance, view season stats, and gain insights into the Premier League's top attacking talents.")),
          column(12, 
                 div(class = "text-center", img(src = "t2.jpg", class = "logo-img"))  # Added logo-img class
          ),
          fluidRow(
            column(4,  
                   div(class = "card", 
                       icon("calendar", class = "icon-large"),
                       h3("Seasons Available - ", textOutput("num_seasons")),  # Dynamically display number of seasons
                       p(textOutput("season_range"))
                   )
            ),
            column(4, 
                   div(class = "card", 
                       icon("users", class = "icon-large"),
                       h4("Players Data Available"),
                       h2(textOutput("num_players"))
                   )
            ),
            column(4, 
                   div(class = "card", 
                       icon("futbol", class = "icon-large"),
                       h4("Available Teams Data"),
                       h2(textOutput("num_teams"))
                   )
            )
          )
          
        )
      ),
      # Player Stats Tab
      tabItem(tabName = "player_stats",
              fluidRow(
                # Title for Player Offensive Performance Analysis
                column(12, div(style = "text-align:left; font-size: 30px; font-weight:bold; color: white; 
                    background-color: rgba(0, 0, 0, 0); padding: 10px; border-radius: 10px;",
                               "Player Offensive Performance Analysis")),
                
                # Player Selection and Player Stats side by side
                column(6, 
                       box(title = "Player Selection", status = "primary", solidHeader = TRUE, width = 12,
                           selectizeInput("player_search", "Player:", 
                                          choices = c("", sorted_players), 
                                          selected = "Erling Haaland"),
                           selectInput("season_player", "Season:", 
                                       choices = c("", unique(data$season)), 
                                       selected = sort(unique(data$season), decreasing = TRUE)[1]),
                           p(textOutput("no_data_message"), style = "color:red; font-weight:bold; font-size:20px !important; text-align:center;")
                       )
                ),
                column(6, 
                       box(title = "Player Stats", status = "primary", solidHeader = TRUE, width = 12, 
                           id = "player_stats_panel",  # ID for shinyjs
                           style = "min-height: 250px;",  # Adjust height
                           fluidRow(
                             # Split into two columns
                             column(6,
                                    p("Total Games: ", strong(textOutput("total_games", inline = TRUE)), style = "font-size:16px;"),  
                                    p("Total Goals: ", strong(textOutput("total_goals", inline = TRUE)), style = "font-size:16px;"),
                                    p("Total Assists: ", strong(textOutput("total_assists", inline = TRUE)), style = "font-size:16px;"),
                                    p("Total xG: ", strong(textOutput("total_xG", inline = TRUE)), style = "font-size:16px;"),
                                    p("Total Shots: ", strong(textOutput("total_shots", inline = TRUE)), style = "font-size:16px;"),
                                    p("Shots on Target P/G: ", strong(textOutput("shots_on_target_pg", inline = TRUE)), style = "font-size:16px;"),
                                    p("Conversion Rate(Full Season): ", strong(textOutput("conversion_Rate_Per_Game", inline = TRUE)), style = "font-size:16px;")
                             ),
                             column(6, 
                                    p("Headers: ", strong(textOutput("headers", inline = TRUE)), style = "font-size:16px;"),
                                    p("Average Minute per Goal: ", strong(textOutput("avg_minute_per_goal", inline = TRUE)), style = "font-size:16px;"),
                                    p("Home Goals: ", strong(textOutput("home_goals", inline = TRUE)), style = "font-size:16px;"),
                                    p("Away Goals: ", strong(textOutput("away_goals", inline = TRUE)), style = "font-size:16px;"),
                                    p("Penalties Converted: ", strong(textOutput("penalties_converted", inline = TRUE)), style = "font-size:16px;"),
                                    p("Left Foot Goals: ", strong(textOutput("left_foot_goals", inline = TRUE)), style = "font-size:16px;"),   
                                    p("Right Foot Goals: ", strong(textOutput("right_foot_goals", inline = TRUE)), style = "font-size:16px;")   
                             )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(title = "Interactive Shot Map", status = "primary", solidHeader = TRUE, width = 12,
                           id = "interactive_plot_panel",  
                           fluidRow(
                             column(4, 
                                    selectInput("situation_filter", "Situation:", 
                                                choices = c("All", unique(data$situation)), selected = "Open Play"),
                                    selectInput("result_filter", "Result:", 
                                                choices = c("All", unique(data$result)), selected = "All"),
                                    selectInput("shottype_filter", "Shot Type:", 
                                                choices = c("All", unique(data$shotType)), selected = "All"),
                                    # Change slider title to be more informative
                                    #sliderInput("xg_filter", "Filter Shots by xG (Expected Goals):", 
                                    #            min = 0, max = 1, value = 1, step = 0.05),
                                    # Add a note below the slider for clarity
                                    p("Note: Each point represents the location from which a shot is taken, and the size of the point indicates the xG value (Expected Goals).",
                                      style = "font-size:14px; color:grey; text-align:center;"),
                                    #p("Adjust the slider to display only shots with an xG less than or equal to the selected value.", 
                                     # style = "font-size:14px; color:grey; text-align:center;")
                             ),
                             column(8, 
                                    plotlyOutput("interactive_plot", height = "600px")
                             )
                           )
                       )
                )
              ),
              # Goal Minute Distribution
              fluidRow(
                column(12,
                       box(
                         title = textOutput("goal_minute_title"),  # Dynamically generate the title
                         status = "primary", solidHeader = TRUE, width = 12,
                         id = "goal_minute_panel",  # ID for shinyjs
                         plotOutput("goal_minute_plot", height = "400px")
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         title = textOutput("combination_stats_plot_title"), 
                         status = "primary", solidHeader = TRUE, width = 12,
                         id = "combination_stats_panel",  # ID for shinyjs
                         plotOutput("combination_stats_plot", height = "400px")
                         )
                )
              )
      ),
      # Season Stats Tab
      tabItem(tabName = "season_stats",
              fluidRow(
                column(12, div(style = "text-align:left; font-size: 30px; font-weight:bold; color: white; 
                    background-color: rgba(0, 0, 0, 0); padding: 10px; border-radius: 10px;",
                               "All Season Offensive Performance Statistics")),                column(12,
                       # Add everything within a box for better visibility
                       box(title = "", status = "primary", solidHeader = TRUE, width = 12, 
                           # Select Season Input
                           selectInput("season", "Select Season:", 
                                       choices = c("", sort(unique(data$season), decreasing = TRUE)), 
                                       selected = sort(unique(data$season), decreasing = TRUE)[1]),
                           
                           # DataTable Output (Season Stats Table)
                           DTOutput("summary_table")
                       )
                )
              )
      )
    )
  )
      
)



# Define server logic
server <- function(input, output, session) {
  
  # Reactive expressions for stats
  output$num_seasons <- renderText({
    paste0(length(unique(data$season)))
  })
  
  output$num_players <- renderText({
    paste0(length(unique(data$Player)))
  })
  
  output$num_teams <- renderText({
    unique_teams <- unique(c(data$home_team, data$away_team))
    paste0(length(unique_teams))
  })
  
  output$season_range <- renderText({
    paste0("(", min(data$season), "-", max(data$season), ")")  # Get min and max season values
  })
  
  output$no_data_message <- renderText({
    # Check if there is data for the selected player and season
    if (nrow(player_specific_season_data()) == 0) {
      return("No data available for the selected player and season.")
    } else {
      return(NULL)  # No message if data is available
    }
  })
  
  observe({
    if (nrow(player_specific_season_data()) == 0) {
      # No data: show message and disable panels
      shinyjs::show("no_data_message")
      shinyjs::disable("player_stats_panel")
      shinyjs::disable("interactive_plot_panel")
      shinyjs::disable("goal_minute_panel")
      shinyjs::disable("combination_stats_panel")
    } else {
      # Data available: hide message and enable panels
      shinyjs::hide("no_data_message")
      shinyjs::enable("player_stats_panel")
      shinyjs::enable("interactive_plot_panel")
      shinyjs::enable("goal_minute_panel")
      shinyjs::disable("combination_stats_panel")
    }
  })
  
  player_choices <- reactive({ sort(unique(data()$Player)) })
  
  # Reactive data for season summary
  season_summary_data <- reactive({
    req(input$season)
    selected_season_data <- data %>%
      filter(season == input$season)
    
    assist_counts <- selected_season_data %>%
      filter(!is.na(player_assisted), result == "Goal") %>%
      group_by(player_assisted) %>%
      summarize(Total_Assists = n()) %>%
      ungroup()
    
    player_stats <- selected_season_data %>%
      group_by(Player) %>%
      summarize(
        Total_Games = n_distinct(match_id),  # Number of games played
        Total_Games = n_distinct(match_id),  # Number of games played
        Total_Goals = sum(result == "Goal", na.rm = TRUE),
        Total_xG = sum(xG, na.rm = TRUE),
        Average_xG = mean(xG, na.rm = TRUE),
        Total_Shots = n(),
        Shots_on_TargetPg = sum(result == "Goal" | result == "SavedShot") / n_distinct(match_id),
        Conversion_Rate_Per_Game = (Total_Goals / n_distinct(match_id)) / 
          (Total_Shots / n_distinct(match_id)) * 100,
        Average_Minute_per_Goal = mean(minute[result == "Goal"]),
        Home_Goals = sum(result == "Goal" & h_a == "h"),
        Away_Goals = sum(result == "Goal" & h_a == "a"),
        Penalty_Scored = sum(result == "Goal" & situation == "Penalty"),
        Penalty_Taken = sum(situation == "Penalty"),
        Headers = sum(result == "Goal" & shotType == "Header"),
        LeftFoot_Goals = sum(result == "Goal" & shotType == "Left Foot"),  # Scored by Left Foot
        RightFoot_Goals = sum(result == "Goal" & shotType == "Right Foot")  # Scored by Right Foot
      ) %>%
      ungroup() %>%
      mutate(
        Total_xG = round(Total_xG, 2),
        Average_xG = round(Average_xG, 2),
        Conversion_Rate_Per_Game = round(Conversion_Rate_Per_Game, 2),
        Shots_on_TargetPg = round(Shots_on_TargetPg, 2),
        Average_Minute_per_Goal = round(Average_Minute_per_Goal, 2),
        Penalties_Converted = paste(Penalty_Scored, Penalty_Taken, sep = "/")
      )
    
    player_stats %>%
      left_join(assist_counts, by = c("Player" = "player_assisted")) %>%
      select(Player, Total_Games, Total_Goals, Total_Assists, Total_xG, Average_xG, Total_Shots, 
             Shots_on_TargetPg, Conversion_Rate_Per_Game, Headers, Average_Minute_per_Goal,
             Home_Goals, Away_Goals, Penalties_Converted, LeftFoot_Goals, RightFoot_Goals) %>%
      arrange(desc(Total_Goals))
  })
  
  # Render season summary table
  output$summary_table <- renderDT({
    datatable(
      season_summary_data(), 
      options = list(
        pageLength = 20, 
        autoWidth = TRUE, 
        scrollX = TRUE  # Enable horizontal scrolling
      )
    )
  })
  
  
  # Reactive data for player-specific stats
  player_specific_season_data <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter data for the selected player and season
    selected_player_season_data <- data %>%
      filter(Player == input$player_search, season == input$season_player)
    
    # Calculate the number of assists made by the selected player
    assist_counts <- data %>%
      filter(result == "Goal", player_assisted == input$player_search, season == input$season_player) %>%
      summarize(Total_Assists = n())  # Count the number of assists
    
    # Calculate player statistics
    player_stats <- selected_player_season_data %>%
      group_by(Player) %>%
      summarize(
        Total_Games = n_distinct(match_id),  # Number of games played
        Total_Goals = sum(result == "Goal", na.rm = TRUE),
        Total_xG = sum(xG, na.rm = TRUE),
        Average_xG = mean(xG, na.rm = TRUE),
        Total_Shots = n(),
        Shots_on_TargetPg = sum(result == "Goal" | result == "SavedShot") / n_distinct(match_id),
        Conversion_Rate_Per_Game = (Total_Goals / n_distinct(match_id)) / 
          (Total_Shots / n_distinct(match_id)) * 100,
        Average_Minute_per_Goal = mean(minute[result == "Goal"]),
        Home_Goals = sum(result == "Goal" & h_a == "h"),
        Away_Goals = sum(result == "Goal" & h_a == "a"),
        # Penalty-related statistics
        Penalty_Scored = sum(result == "Goal" & situation == "Penalty", na.rm = TRUE),
        Penalty_Taken = sum(situation == "Penalty", na.rm = TRUE),  # Ensure `na.rm` is used
        Headers = sum(result == "Goal" & shotType == "Header"),
        LeftFoot_Goals = sum(result == "Goal" & shotType == "Left Foot"),  # Scored by Left Foot
        RightFoot_Goals = sum(result == "Goal" & shotType == "Right Foot")  # Scored by Right Foot
      ) %>%
      ungroup() %>%
      mutate(
        Total_xG = round(Total_xG, 2),
        Average_xG = round(Average_xG, 2),
        Conversion_Rate_Per_Game = round(Conversion_Rate_Per_Game, 2),
        Shots_on_TargetPg = round(Shots_on_TargetPg, 2),
        Average_Minute_per_Goal = round(Average_Minute_per_Goal, 2),
        Penalties_Converted = paste(Penalty_Scored, Penalty_Taken, sep = "/"),
        Total_Assists = assist_counts$Total_Assists  # Add assists to the stats
      )
    
    # Return the player stats
    player_stats
  })
  
  # Render player-specific stats
  output$total_goals <- renderText({ player_specific_season_data()$Total_Goals })
  output$total_assists <- renderText({ player_specific_season_data()$Total_Assists })
  output$total_xG <- renderText({ player_specific_season_data()$Total_xG })
  output$total_shots <- renderText({ player_specific_season_data()$Total_Shots })
  output$shots_on_target_pg <- renderText({ player_specific_season_data()$Shots_on_TargetPg })
  output$conversion_Rate_Per_Game <- renderText({
    conversion_rate <- player_specific_season_data()$Conversion_Rate_Per_Game
    paste0(conversion_rate, "%")
  })
  output$headers <- renderText({ player_specific_season_data()$Headers })
  output$avg_minute_per_goal <- renderText({ player_specific_season_data()$Average_Minute_per_Goal })
  output$home_goals <- renderText({ player_specific_season_data()$Home_Goals })
  output$away_goals <- renderText({ player_specific_season_data()$Away_Goals })
  output$penalties_converted <- renderText({ player_specific_season_data()$Penalties_Converted })
  output$total_games <- renderText({ player_specific_season_data()$Total_Games })
  output$left_foot_goals <- renderText({ player_specific_season_data()$LeftFoot_Goals })
  output$right_foot_goals <- renderText({ player_specific_season_data()$RightFoot_Goals })
  
  
  
  ############################################ Reactive plot for the shot map
  interactive_plot <- reactive({
    req(input$player_search, input$season_player, input$situation_filter, input$result_filter, input$shottype_filter)
    
    plot_data <- data %>%
      filter(Player == input$player_search, 
             season == input$season_player,
             (input$result_filter == "All" | result == input$result_filter),  # Use selected result
             (input$situation_filter == "All" | situation == input$situation_filter),
             (input$shottype_filter == "All" | shotType == input$shottype_filter))#,
             #xG >= (1 - input$xg_filter))  # Reverse the filter logic here
    
    # Create and return the plot if data is available
    if (!is.null(plot_data) && nrow(plot_data) > 0) {
      # Modify the plot to include player_assisted and lastAction in hover info
      plot <- ggplot(plot_data, aes(x = X * 100, y = Y * 100, 
                                    color = result, size = xG, 
                                    text = paste("Assisted by:", player_assisted, "<br>",
                                                 "Last Action:", lastAction, "<br>",
                                                 "xG:", round(xG, 2)))) +  # Rounded xG to 1 decimal place
        annotate_pitch(colour = "white", fill = "seagreen", limits = FALSE) +
        geom_point(alpha = 0.8) +  # Keep size variation in the plot
        theme_pitch() +
        theme(panel.background = element_rect(fill = "seagreen"),
              legend.text = element_text(size = 12),
              legend.title = element_text(face = "bold", size = 14)) +  # Bold title for the legend
        coord_flip(xlim = c(49, 101)) +
        scale_y_reverse() +
        scale_color_manual(values = c("Goal" = "chartreuse", 
                                      "Missed Shots" = "orangered1", 
                                      "Saved Shot" = "blue", 
                                      "Blocked Shot" = "yellow",
                                      "Own Goal" = "pink",
                                      "Shot On Post" = "peru")) +
        guides(color = guide_legend(override.aes = list(size = 6), title = "Result"), 
               size = guide_legend(title = NULL))  # Hide xG title in the legend
      
      # Convert ggplot to plotly for tooltips
      ggplotly(plot, tooltip = "text") %>%
        config(displayModeBar = FALSE,  # Hide the plotly modebar
               scrollZoom = FALSE,      # Disable zooming
               doubleClick = FALSE,     # Disable double-clicking
               displaylogo = FALSE,     # Hide the Plotly logo
               editable = FALSE) %>%   # Prevent legend from being interactive
        htmlwidgets::onRender("
      function(el) {
        el.on('plotly_hover', function() {
          el.style.cursor = 'default';  // Keep default cursor for hover
        });
      }
    ")
    } else {
      # Return a blank plot with the pitch layout but no points
      empty_plot <- ggplot() +
        annotate_pitch(colour = "white", fill = "seagreen", limits = FALSE) +
        theme_pitch() +
        theme(panel.background = element_rect(fill = "seagreen"),
              plot.title = element_text(hjust = 0.5, size = 14),
              legend.position = "none") +
        coord_flip(xlim = c(49, 101)) +
        scale_y_reverse() +
        labs(title = "No Data Available", x = "Pitch Y Coordinates", y = "Pitch X Coordinates")
      
      ggplotly(empty_plot)
    }
  })
  
  # Output the interactive plot
  output$interactive_plot <- renderPlotly({ interactive_plot() })
  
  player_goal_min_plot <- reactive({
    req(input$player_search, input$season_player)
    plot_data <- data %>%
      filter(Player == input$player_search, season == input$season_player, result == "Goal")
    
    # Add an explicit point at (0, 0) if it's missing
    if (nrow(plot_data) > 0 && min(plot_data$minute) > 0) {
      plot_data <- bind_rows(plot_data, data.frame(minute = 0, result = "Goal", ..count.. = 0))
    }
    
    if (!is.null(plot_data) && nrow(plot_data) > 0) {
      plot <- ggplot(plot_data, aes(x = minute)) +
        geom_freqpoly(binwidth = 5, aes(color = ..count..), size = 2) +  # Thicker line
        scale_color_viridis(option = "magma", begin = 0.3, end = 0.9, direction = 1) +
        scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 10)) +  # X-axis breaks at intervals of 10
        scale_y_continuous(limits = c(0, NA)) +  # Set y-axis lower limit to 0, upper limit will be auto
        theme_minimal(base_size = 16) +  # Increased base font size
        labs(x = "Minute of the Match", y = "Goal Count") +  # Updated axis labels
        theme(panel.background = element_rect(fill = "black", color = NA),  # Black background
              plot.background = element_rect(fill = "black", color = NA),    # Black plot background
              panel.grid.major = element_blank(),                           # Remove major grid lines
              panel.grid.minor = element_blank(),                           # Remove minor grid lines
              axis.text = element_text(size = 14, color = "white"),          # Increase axis text size
              axis.title = element_text(size = 16, face = "bold", color = "white"),  # Bold axis titles
              legend.position = "none")  # Remove the legend entirely
      
      plot  # Return the plot
    } else {
      ggplot() +
        theme_minimal(base_size = 16) +
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black")) +
        labs(title = "No Data Available", x = "Minute", y = "Count")
    }
  })
  
  
  # Output the Goal Minute Distribution Plot
  output$goal_minute_plot <- renderPlot({ player_goal_min_plot() })
  
  # Server logic to generate dynamic title for goal minute plot
  output$goal_minute_title <- renderText({
    player <- input$player_search
    season <- input$season_player
    if (player != "" && season != "") {
      paste("Distribution of Goals Scored Across Match Minutes by", player, "during the", season)
    } else {
      "Distribution of Goals Scored Across Match Minutes"
    }
  })
  
  
  
  # Reactive expression for Build-up Play Network
  linkup_play_plot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter the data for passes leading to shots or goals
    passes_data <- data %>%
      filter(result %in% c("Goal", "Shot"), season == input$season_player)
    
    # Prepare the edges for the network (passer to receiver)
    edges <- passes_data %>%
      filter(!is.na(player_assisted)) %>%
      group_by(Player, player_assisted) %>%
      summarize(pass_count = n()) %>%
      filter(pass_count > 3) %>%  # Filter to show only significant connections
      ungroup() %>%
      select(from = Player, to = player_assisted, weight = pass_count)
    
    # Build the network graph
    g <- graph_from_data_frame(edges, directed = TRUE)
    
    # Set vertex size based on degree (number of connections)
    V(g)$size <- degree(g) * 3  # Increase size based on the number of connections
    
    # Plot the network using Fruchterman-Reingold layout
    plot(g, 
         vertex.size = V(g)$size, 
         vertex.label.cex = 0.8, 
         edge.arrow.size = 0.8, 
         edge.width = E(g)$weight / 2,  # Edge width based on pass count
         layout = layout_with_fr(g),  # Better node spacing
         vertex.label.dist = 0.5,  # Reduce distance between labels and nodes
         vertex.label.color = "blue",  # Label color
         edge.color = "gray")  # Edge color
  })
  
  # Output the Build-up Play Network Plot
  output$linkup_play_plot <- renderPlot({
    linkup_play_plot()  # Render the reactive plot
  })
  
  
  # Reactive expression for Statistical Analysis of Combinations
  combination_stats_plot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter the data for passes leading to shots or goals, and involve the specific player
    passes_data <- data %>%
      filter(season == input$season_player, 
             (Player == input$player_search | player_assisted == input$player_search)) %>%
      filter(!is.na(Player), !is.na(player_assisted))  # Exclude NA values in Player and player_assisted
    
    # Create a new column to indicate whether the player was the passer or the receiver
    passes_data <- passes_data %>%
      mutate(player_role = ifelse(Player == input$player_search, "Passer", "Receiver"))
    
    # Group by the passer, receiver, and player role, then calculate stats like pass count and xG contribution
    combination_stats <- passes_data %>%
      group_by(Player, player_assisted, player_role) %>%
      summarise(total_xG = sum(xG), pass_count = n()) %>%
      arrange(desc(total_xG))
    
    # Filter to include only combinations with pass_count > 3
    combination_stats <- combination_stats %>%
      filter(pass_count > 3)
    
    # Visualize with a bar chart (pass count by player combination)
    ggplot(combination_stats, aes(x = reorder(paste(Player, "→", player_assisted), -pass_count), y = pass_count, fill = total_xG)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis_c(option = "viridis") +  # Use viridis color scale
      labs(title = "", x = "", y = "Number of Passes", fill = "Combination xG") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 12),  # Increase size and make the y-axis labels bold
            axis.text.x = element_text(size = 12),
            legend.text = element_blank(),               # Remove numbers from legend
            legend.title = element_text(size = 12),      # Keep the legend title
            panel.grid = element_blank(),                # Remove all grid lines
            panel.background = element_rect(fill = "white"),  # Set background to white
            axis.line = element_line(colour = "black"),  # Add axis lines for a clean look
            panel.border = element_blank())              # Remove the plot box/border
  })
  
  # Output the Statistical Analysis Plot
  output$combination_stats_plot <- renderPlot({
    combination_stats_plot()  # Render the reactive plot
  })
  
  # Server logic to generate dynamic title for goal minute plot
  output$combination_stats_plot_title <- renderText({
    player <- input$player_search
    season <- input$season_player
    if (player != "" && season != "") {
      paste("Effective Combinations of", player, " during the", season)
    } else {
      "Effective Combinations of a Player in Season"
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
