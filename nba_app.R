library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(knitr)
library(bslib)

# import data and functions from project.qmd file
source(purl("project.qmd", output = tempfile(), quiet = TRUE))

# helper function that creates an action link for the team schedules and game
make_link_column = function(df) {
  function(i) {
    layout_columns(actionLink(
      inputId = paste0("game", df$game_id[i]),
      label = if (df$season_type[i] != 'Playoffs') {
        p(df$game_name[i])
      } else {
        p(paste0("Game ", i, df$game_name[i]))
      }
    ))
  }
}


display_schedule_gamestats = function(input, output, session) {
  if (input$season != "" && input$team != "") {
    req(input$season, input$team)
    
    games = reactiveValues(observers = list())
    
    purrr::walk(games$observers, ~ .x$destroy())
    output$links = NULL
    
    df = get_games_for_seasonteam(nba_data, input$season, input$team)
    
    pre = df$by.sub$`Pre Season`
    reg = df$by.sub$`Regular Season`
    plof = df$by.sub$Playoffs
    all_df = df$all
    
    pregame_links = {
      if (is.null(pre)) {
        p("No games found for the pre-season.")
      } else {
        purrr::map(seq_len(nrow(pre)), make_link_column(pre))
      }
    }
    
    regular_links = {
      if (is.null(reg)) {
        p("No games found for the regular season.")
      } else {
        purrr::map(seq_len(nrow(reg)), make_link_column(reg))
      }
    }
    
    playoffs_links = {
      if (is.null(plof) || length(names(plof)) == 0) {
        p("No games found for the playoffs.")
      } else {
        teams = names(plof)
        purrr::map(seq_len(length(teams)), \(team_i) {
          df.playoff = plof[[teams[team_i]]]
          list(h6(teams[team_i]),
               purrr::map(seq_len(nrow(
                 df.playoff
               )), make_link_column(df.playoff)))
        })
      }
    }
    
    yr = as.integer(input$season)
    output$regular = renderUI({list(h4("Regular Season"), regular_links)})
    output$pre = renderUI({list(h4("Preseason"), pregame_links)})
    output$playoffs = renderUI({list(h4("Playoffs"), playoffs_links)})
    
    games$observers = purrr::map(seq_len(nrow(all_df)), function(i) {
      label = paste0("game", all_df$game_id[i])
      
      observeEvent(input[[label]], {
        tables = summarize_game_stat(all_df, all_df$game_id[i])
        final.score = tables$final
        teams = colnames(tables$final)
        output$table = renderTable(tables$stats |> select(2, 1, 3))
        output$final = renderTable(tables$final)
        showModal(
          modalDialog(
            title = paste0("", all_df$game_name[i]),
            fluidRow(
              column(
                width = 4,
                tags$div(
                  style = "text-align: center; display: flex; align-items: center; margin-bottom: 20px; 
                  margin-left: auto; margin-right: auto;",
                  renderImage({
                    return(list(
                      src = file.path("finalized_logos", paste0(all_df$team_abbreviation_home[i], ".gif")),
                      contentType = "image/gif",
                      alt = paste(input$team, "logo")
                    ))
                  }, deleteFile = FALSE)
                )
              ),
              column(
                width = 4,
                div(
                  style = "text-align: center; display: flex; justify-content: center; align-items: center; text-align: center;",
                  tableOutput("final")
                )
              ),
              column(
                width = 4,
                tags$div(
                  style = "text-align: center; display: flex; align-items: center; margin-bottom: 20px; 
                  margin-left: auto; margin-right: auto;",
                  renderImage({
                    return(list(
                      src = file.path("finalized_logos", paste0(all_df$team_abbreviation_away[i], ".gif")),
                      contentType = "image/gif",
                      alt = paste(input$team, "logo")
                    ))
                  }, deleteFile = FALSE)
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                tags$div(
                  style = "text-align: center; display: flex; align-items: center; margin-bottom: 20px; margin-top: -300px;
                  margin-left: auto; margin-right: auto; th {
                    text-align: center !important;
                  }",
                  # Control the container's height,
                  tableOutput("table")
                )
              )
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
      })
    })
  }
}
  
  # define UI for the app
  ui = fluidPage(
    
    # Add custom CSS to style the sidebar panel
    tags$head(
      tags$style(HTML("
      .sidebar { 
        background-color: white !important; 
      }
    "))
    ),
    
    titlePanel("NBA Season Summary"),
    sidebarLayout(
      sidebarPanel(
        # dropdown menu to select NBA season
        selectInput(
          inputId = "season",
          label = "Select an NBA season:",
          choices = c("Select an NBA season", as.character(2000:2022)),
          # seasons from 1946 to 2023
          # default selection
        ),
        
        # select input to choose a team based on the selected season
        selectInput(
          inputId = "team",
          label = "Select an NBA team:",
          choices = c("Select a season first"),
          # default when no season selected
          selected = NULL # no team selected initially
        ),
        
        # action button to show stats for the selected season
        actionButton("show_stats", "Show Season Stats"),
        uiOutput("error"),
        
        # Add spacing
        br(),br(),br(),
        
        # UI output for displaying logo
        tags$div(
          style = "text-align: center;",
          # Control the container's height,
          imageOutput("team_logo", height = "50px")
        ),
        
        br(),br(),

        tags$div(
          uiOutput("full_team_name"),
          style = "text-align: center; margin-top: 20px;" # Center and add spacing
        ),
        class="sidebar"
      ),
      
      mainPanel(
        uiOutput("team_name"),
        
        tabsetPanel(
          # tab displaying team schedule
          tabPanel(
            title = "Team Schedule and Game Statistics",
            fluidRow(
              column(
                width = 12,
                uiOutput("pre")
              )
            ),
            fluidRow(
              column(
                width = 6, 
                uiOutput("regular")
              ),
              column(
                width = 6, 
                uiOutput("playoffs")
              )
            )
          ),
          # tab to display a plot of league-wide season stats
          tabPanel(
            title = "League-Wide Stats",
            uiOutput("options"),
            plotOutput("season_stats_plot") # league stats output
          ),
          
          # tab to display team information
          tabPanel(
            title = "Team Stats",
            fluidRow(
              column(width = 4, 
                     uiOutput("top5_title"), 
                     tableOutput("top_5_points")
              ),
              # table for PPG
              column(
                width = 4,
                uiOutput("top5_rebounds"),
                tableOutput("top_5_rebounds") # table for RPG
              ),
              column(
                width = 4, 
                uiOutput("top5_assists"), 
                tableOutput("top_5_assists") # table for APG
              ),
              plotOutput("team_3p_time_series")
            ),
            # plot for fg% time series
            # html to format top 5 table
            tags$style(
              HTML(
                "table {
                 width: 100%;
                 margin-top: 10px;
                 border-collapse: collapse;
               }
               th {
                 background-color: #f2f2f2;
                 font-weight: bold;
                 text-align: left;
                 padding: 8px;
               }
               td {
                 padding: 8px;
                 border: 1px solid #ddd;
               }
               tr:nth-child(even) {
                 background-color: #f9f9f9;
               }
               tr:hover {
                 background-color: #f1f1f1;
               }"
              )
            )
          )
        ) # End of tabsetPanel
      ) # End of mainPanel
    ) # End of sidebarLayout
  ) # End of fluidPage
  
  reset = function(output) {
    output$team_name = NULL
    output$team_logo = NULL
    output$season_stats_plot = NULL
    output$top_5_points = NULL
    output$top_5_assists = NULL
    output$top_5_rebounds = NULL
    output$team_3p_time_series = NULL
    output$pre = NULL 
    output$regular = NULL
    output$playoffs = NULL
    output$options = NULL
    output$top5_title = NULL
    output$top5_rebounds = NULL
    output$top5_assists = NULL
  }
  
  # define app server function
  server = function(input, output, session) {
    logo_path = reactiveVal(NULL)
    show_results = reactiveVal(FALSE)
    
    observeEvent(input$team, {
      #display_schedule_gamestats(input, output, session)
      reset(output)
      show_results(FALSE)
      
      if (input$team != "") {
        # Dynamically construct the file path for the selected team
        path = file.path("finalized_logos", paste0(input$team, ".gif"))
        logo_path(path)  # Update the reactive value
      } else {
        # Set a default or empty value if no team is selected
        logo_path(NULL)
      }
      
      output$error = renderUI({
        p(HTML('<br>Please select an NBA season and team, and press "Show Season Stats" 
          when you are done.'),
           style = "font-weight:bold;")
      })
    })
    
    # Update the logo path when the "Show Season Stats" button is clicked
    observeEvent(input$show_stats, {
      show_results(TRUE)
      if (input$team != "Select a team" && input$season != "Select an NBA season") {
        print("Showing stats...")
        print(logo_path())
        
        output$options = renderUI(
          {
            radioButtons(
              inputId = "league_graph",
              label = "Select a statistic to graph:",
              choices = c("Offense/Defense", "3-Point"),
              selected = "Offense/Defense",
              inline = TRUE
            )
          }
        )
        
        output$team_name = renderUI(
          {
            yr = as.integer(input$season)
            return(h1(paste0("The ", yr, "-", yr + 1, " NBA Season for ", input$team)))
          }
        )
        
        output$full_team_name = renderUI(
          {
            full_name = nba_data |>
              filter(grepl(paste0(input$season, "$"), as.character(season_id)),
                     team_abbreviation_home == input$team) |>
              select(team_name_home) |>
              slice(1) |>
              as.character()
            return(p(full_name))
          }
        )
        output$team_logo = renderImage({
          req(logo_path())  # Ensure the logo path is set before rendering
          return(list(
            src = logo_path(),
            contentType = "image/gif",
            alt = paste(input$team, "logo")
          ))
        }, deleteFile = FALSE)
        
        display_schedule_gamestats(input, output, session)
        
        if (show_results()) {
          output$season_stats_plot = renderPlot({
            averages = filter_and_average_fg3(nba_data, input$season)
            # get filtered and averaged data
            
            if (input$league_graph == "Offense/Defense") {
              graph_team_rating(team_rating(nba_data, as.numeric(input$season)))
            } else if (input$league_graph == "3-Point") {
              graph_three_rating(three_pointer_stat(nba_data, as.numeric(input$season)))
            }
          })
          
          # render table for points per game
          output$top_5_points = renderUI(
            list(
              h4("Top 5 Players by PPG"),
              renderTable({
                points = get_top5_ppg_for_team_and_season(player_data_transformed, 
                                                           input$team, input$season)
                if (!is.null(points)) {
                  colnames(points) = c("Player Name", "Points/g")
                  points
                }
              }, rownames = TRUE)
            )
          )
          
          # render table for rebounds per game
          output$top_5_rebounds = renderUI(
            
            list(
              h4("Top 5 Players by RPG"),
              renderTable({
                rebounds = get_top5_rpg_for_team_and_season(player_data_transformed, 
                                                             input$team, input$season)
                
                if (!is.null(rebounds)) {
                  colnames(rebounds) = c("Player Name", "Rebounds/g")
                  rebounds
                }
              }, rownames = TRUE)
            )
          )
          
          # render table for assists per game
          output$top_5_assists = renderUI(
            list(
              h4("Top 5 Players by APG"),
              renderTable({
                assists = get_top5_apg_for_team_and_season(player_data_transformed, 
                                                            input$team, input$season)
                
                if (!is.null(assists)) {
                  colnames(assists) = c("Player Name", "Assists/g")
                  assists
                }
              }, rownames = TRUE)
            )
          )
              
          # render time series plot for team field goal percentage
          output$team_3p_time_series = renderPlot({
            games_data = team_fgp_per_game(nba_data, input$season, input$team)
            
            if (!is.null(games_data) && nrow(games_data) > 0) {
              ggplot(games_data, aes(x = as.Date(game_date), y = fg_pct)) +
                geom_line(color = "blue", size = 1) +
                labs(
                  title = paste(
                    "Game-to-Game Field Goal Percentage for",
                    input$team,
                    "in",
                    input$season,
                    "Season"
                  ),
                  x = "Date",
                  y = "Field Goal Percentage"
                ) +
                theme_minimal()
            }
          })
          output$error = NULL
        } else {
          reset(output)
          output$error = renderUI({
            p(HTML('<br>Please select an NBA season and team, and press "Show Season Stats" 
          when you are done.'),
              style = "font-weight:bold;")
          })
        }
      }
    })
    
    # update team options based on selected season
    observeEvent(input$season, {
      reset(output)
  
      show_results(FALSE)
      # only proceed if a season has been entered in input
      if (input$season != "") {
        # filter data based on the selected season and extract unique team abbreviations
        filtered_teams = nba_data |>
          filter(grepl(paste0(input$season, "$"), as.character(season_id))) |>
          pull(team_abbreviation_home) |>
          unique() |>
          sort()
        
        # update selectInput with the filtered list of teams for the season
        updateSelectInput(session,
                          "team",
                          choices = c("Select a team", filtered_teams),
                          # set available teams
        ) # first team listed is default
      }
      
      output$error = renderUI({
        p(HTML('<br>Please select an NBA season and team, and press "Show Season Stats" 
          when you are done.'),
          style = "font-weight:bold;")
      })
    })
    
  }
  
  shinyApp(ui = ui, server = server)