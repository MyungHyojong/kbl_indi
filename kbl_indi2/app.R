library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)

# Function to calculate the next multiple of 5
next_multiple_of_5 <- function(x) {
  return(ceiling(x / 5) * 5)
}

# Function to create stat filter UI elements with dynamic max values
create_stat_filter_ui <- function(data, id, short_label, long_label) {
  # If the stat is a percentage, set max to 100
  if (grepl("%", id)) {
    max_value <- 100
  } else {
    max_value <- next_multiple_of_5(max(data[[id]], na.rm = TRUE))
  }
  
  # Clean ID for conditionalPanel
  clean_id <- gsub("[^[:alnum:]]", "_", id)
  
  list(
    checkboxInput(paste0("filter_", clean_id), label = paste0(short_label, " (", long_label, ")"), value = FALSE),
    conditionalPanel(
      condition = paste0("input.filter_", clean_id, " == true"),
      sliderInput(inputId = clean_id, label = long_label, min = 0, max = max_value, value = c(0, max_value))
    )
  )
}

# Function to apply stat filters
apply_stat_filter <- function(data, input, id) {
  clean_id <- gsub("[^[:alnum:]]", "_", id)
  if (input[[paste0("filter_", clean_id)]]) {
    # 슬라이더의 최소, 최대 값을 가져와서 필터링
    min_val <- input[[clean_id]][1]
    max_val <- input[[clean_id]][2]
    
    data <- data %>%
      filter(.data[[id]] >= min_val & .data[[id]] <= max_val)
  }
  return(data)
}

ui <- fluidPage(
  titlePanel("KBL Player 00~23 game Stats"),
  
  sidebarLayout(
    sidebarPanel(
      "Exploring all player stats from the KBL 00-01 to 23-24 seasons",
      
      h3("Filters"),
      
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(players$Date),
                     end = max(players$Date),
                     min = min(players$Date),
                     max = max(players$Date),
                     format = "yyyy-mm-dd"),
      
      selectInput("Team", "Team", c("All", unique(players$Team)), multiple = TRUE, selected = "All"),
      
      # All stat filters are shown from the start
      do.call(tagList, c(
        create_stat_filter_ui(players, "Pts", "Pts", "Points"),
        create_stat_filter_ui(players, "2PTM", "2PTM", "2PT Made"),
        create_stat_filter_ui(players, "2PTA", "2PTA", "2PT Attempted"),
        create_stat_filter_ui(players, "`2PT %`", "2PT%", "2PT Percentage"),
        create_stat_filter_ui(players, "3PTM", "3PTM", "3PT Made"),
        create_stat_filter_ui(players, "3PTA", "3PTA", "3PT Attempted"),
        create_stat_filter_ui(players, "`3PT %`", "3PT%", "3PT Percentage"),
        create_stat_filter_ui(players, "FGM", "FGM", "Field Goals Made"),
        create_stat_filter_ui(players, "FGA", "FGA", "Field Goals Attempted"),
        create_stat_filter_ui(players, "`FG %`", "FG%", "Field Goal Percentage"),
        create_stat_filter_ui(players, "FTM", "FTM", "Free Throws Made"),
        create_stat_filter_ui(players, "FTA", "FTA", "Free Throws Attempted"),
        create_stat_filter_ui(players, "`FT %`", "FT%", "Free Throw Percentage"),
        create_stat_filter_ui(players, "OR", "OR", "Offensive Rebounds"),
        create_stat_filter_ui(players, "DR", "DR", "Defensive Rebounds"),
        create_stat_filter_ui(players, "TOT", "TOT", "Total Rebounds"),
        create_stat_filter_ui(players, "DK", "DK", "Dunks"),
        create_stat_filter_ui(players, "AST", "AST", "Assists"),
        create_stat_filter_ui(players, "TO", "TO", "Turnovers"),
        create_stat_filter_ui(players, "Stl", "Stl", "Steals"),
        create_stat_filter_ui(players, "BS", "BS", "Blocks"),
        create_stat_filter_ui(players, "PF", "PF", "Personal Fouls"),
        create_stat_filter_ui(players, "FO", "FO", "Fouled by Opponent"),
        create_stat_filter_ui(players, "PP", "PP", "Paintzone Points")
      )),
      
      h3("Plot options"),
      
      # X variable selection for histogram
      conditionalPanel(
        condition = "input.plot_type == 'histogram'",
        selectInput("x_var_hist", "X Variable", choices = c("Pts", "2PTM", "2PTA", "`2PT %`", "3PTM", "3PTA", "`3PT %`", "FGM", "FGA", "`FG %`", "FTM", "FTA", "`FT %`", "OR", "DR", "TOT", "DK", "AST", "TO", "Stl", "BS", "PF", "FO", "PP"))
      ),
      
      # X and Y variable selection for scatter and heatmap
      conditionalPanel(
        condition = "input.plot_type == 'scatter' || input.plot_type == 'heatmap'",
        selectInput("x_var", "X Variable", choices = c("Pts", "2PTM", "2PTA", "`2PT %`", "3PTM", "3PTA", "`3PT %`", "FGM", "FGA", "`FG %`", "FTM", "FTA", "`FT %`", "OR", "DR", "TOT", "DK", "AST", "TO", "Stl", "BS", "PF", "FO", "PP")),
        selectInput("y_var", "Y Variable", choices = c("Pts", "2PTM", "2PTA", "`2PT %`", "3PTM", "3PTA", "`3PT %`", "FGM", "FGA", "`FG %`", "FTM", "FTA", "`FT %`", "OR", "DR", "TOT", "DK", "AST", "TO", "Stl", "BS", "PF", "FO", "PP"))
      ),
      
      radioButtons("plot_type", "Plot type", c("histogram", "scatter", "heatmap")),
      numericInput("size", "Font size", 16)
    ),
    mainPanel(
      strong(
        "There are",
        textOutput("num_players", inline = TRUE),
        "players in the dataset"
      ),
      plotOutput("nba_plot"),
      DTOutput("players_data")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    players_filtered <- players
    
    players_filtered <- players_filtered %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    
    if (input$Team != "All" && length(input$Team) > 0) {
      players_filtered <- players_filtered %>%
        filter(Team %in% input$Team)
    }
    
    stats <- c("Pts", "2PTM", "2PTA", "`2PT %`", "3PTM", "3PTA", "`3PT %`", "FGM", "FGA", "`FG %`", "FTM", "FTA", "`FT %`", "OR", "DR", "TOT", "DK", "AST", "TO", "Stl", "BS", "PF", "FO", "PP")
    for (stat in stats) {
      players_filtered <- apply_stat_filter(players_filtered, input, stat)
    }
    
    players_filtered
  })
  
  output$players_data <- renderDT({
    datatable(filtered_data() %>% select(names(players)[1:25]))  # select() is applied here before datatable()
  })
  
  output$num_players <- renderText({
    nrow(filtered_data())
  })
  
  output$nba_plot <- renderPlot({
    if (input$plot_type == "histogram") {
      x_var <- paste0("`", input$x_var_hist, "`")
      p <- ggplot(filtered_data(), aes_string(x = x_var)) + 
        geom_histogram(fill = "blue", colour = "black") + 
        theme_classic(base_size = input$size)
      
      print(p)
    } else if (input$plot_type == "scatter") {
      x_var <- paste0("`", input$x_var, "`")
      y_var <- paste0("`", input$y_var, "`")
      p <- ggplot(filtered_data(), aes_string(x = x_var, y = y_var, color = "Team")) + 
        geom_point() +          # Use geom_point for scatter plot
        theme_classic(base_size = input$size) +
        theme(legend.position = "none")  # Remove legend
      
      print(p)
    } else if (input$plot_type == "heatmap") {
      x_var <- paste0("`", input$x_var, "`")
      y_var <- paste0("`", input$y_var, "`")
      p <- ggplot(filtered_data(), aes_string(x = x_var, y = y_var)) + 
        geom_bin2d() + 
        scale_fill_gradient(low = "white", high = "red") + 
        theme_classic(base_size = input$size) +
        theme(legend.position = "none")  # Remove legend
      
      print(p)
    }
  })
  
}

shinyApp(ui, server)

