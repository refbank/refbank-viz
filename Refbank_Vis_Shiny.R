library(shiny)
library(tidyverse)
library(here)
theme_set(theme_classic())

# Load Data
chat_csv <- read.csv(here("chat.csv")) |> mutate(repNum = repNum + 1)

# Define UI
ui <- fluidPage(
  titlePanel("Refbank Visualizations"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "rep", 
        "Repetition(s):", 
        choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6),
        selected = c(1, 2, 3, 4, 5, 6)
      ),
      selectInput(
        "game", 
        "Game ID:", 
        choices = c("All games", unique(chat_csv$gameId)),  
        selected = "All games"
      ),
      selectInput(
        "grouping",
        "Color by:",
        choices = c("None" = "gameId", "Group size" = "numPlayers"),# "Tangram" = "tangram"),
        selected = "None"
      )
    ),
    mainPanel(
      plotOutput("word_plot"),
      plotOutput("accuracy_plot"),
      plotOutput("time_plot")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  groupings <- c("None" = "gameId", "Group size" = "numPlayers")
  
  output$word_plot <- renderPlot({
    req(input$rep)
    
    rep_df <- chat_csv |>
      group_by(gameId, repNum, numPlayers) |>
      filter(role == "speaker") |> 
      summarise(total_num_words = sum(total_num_words, na.rm = TRUE), .groups = "drop") |>
      filter(!is.na(total_num_words),
             repNum %in% as.numeric(input$rep))
    
    if (input$game != "All games") {
      rep_df <- rep_df |> filter(input$grouping == input$game)
    }
    
    if (nrow(rep_df) == 0) return(NULL)
    
    ggplot(rep_df, aes(x = repNum, y = total_num_words, color = as.factor(.data[[input$grouping]]), group = gameId)) +
      geom_line(alpha = if (input$game == "All games") 0.2 else 1) +
      geom_point(alpha = if (input$game == "All games") 0.2 else 1) +
      labs(title = "Total speaker utterance length across repetitions", x = "Repetition number", y = "Length (words)",
           col = names(groupings)[groupings == input$grouping]) +
      # theme_minimal() +
      theme(legend.position = if (input$grouping == "gameId") "none" else "inside",
            legend.position.inside = c(0.9, 0.85)) +
      geom_smooth(aes(col = if (input$grouping == "gameId") "black" else as.factor(.data[[input$grouping]]),
                      group = if (input$grouping == "gameId") 1 else as.factor(.data[[input$grouping]])), 
                  method = "lm", formula = y ~ log(x), se = (input$grouping == "gameId"), linetype = "dashed")
  })
  
  output$accuracy_plot <- renderPlot({
    req(input$rep)
    
    correct_data <- chat_csv |>
      filter(!is.na(submitted) & !is.na(correct) & submitted == TRUE & correct == TRUE) |>
      group_by(gameId, repNum, numPlayers) |>
      summarise(submitted_correct = n(), .groups = "drop")
    
    total_data <- chat_csv |>
      filter(!is.na(submitted) & submitted == TRUE) |>
      group_by(gameId, repNum, numPlayers) |>
      summarise(submitted_total = n(), .groups = "drop")
    
    accuracy_data <- full_join(correct_data, total_data, by = join_by(gameId, repNum, numPlayers)) |>
      replace_na(list(submitted_correct = 0, submitted_total = 0)) |>
      mutate(accuracy = ifelse(submitted_total > 0, submitted_correct / submitted_total, 0)) |>
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All games") {
      accuracy_data <- accuracy_data |> filter(input$grouping == input$game)
    }
    
    if (nrow(accuracy_data) == 0) return(NULL)
    
    ggplot(accuracy_data, aes(x = repNum, y = accuracy, color = as.factor(.data[[input$grouping]]), group = gameId)) +
      geom_line(alpha = if (input$game == "All games") 0.2 else 1) +
      geom_point(alpha = if (input$game == "All games") 0.2 else 1) +
      labs(title = "Accuracy across repetitions", x = "Repetition", y = "Accuracy",
           col = names(groupings)[groupings == input$grouping]) +
      # theme_minimal() +
      theme(legend.position = if (input$grouping == "gameId") "none" else "inside",
            legend.position.inside = c(0.9, 0.2)) +
      geom_smooth(aes(col = if (input$grouping == "gameId") "black" else as.factor(.data[[input$grouping]]),
                      group = if (input$grouping == "gameId") 1 else as.factor(.data[[input$grouping]])), 
                  method = "lm", formula = y ~ log(x), se = (input$grouping == "gameId"), linetype = "dashed")
  })
  
  output$time_plot <- renderPlot({
    req(input$rep)
    
    reaction_time <- chat_csv |>
      filter(!is.na(time)) |>
      group_by(gameId, repNum, numPlayers) |>
      summarise(avg_time = mean(time, na.rm = TRUE), .groups = "drop") |>
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All games") {
      reaction_time <- reaction_time |> filter(input$grouping == input$game)
    }
    
    if (nrow(reaction_time) == 0) return(NULL)
    
    ggplot(reaction_time, aes(x = repNum, y = avg_time, color = as.factor(.data[[input$grouping]]), group = gameId)) +
      geom_line(alpha = if (input$game == "All games") 0.2 else 1) +
      geom_point(alpha = if (input$game == "All games") 0.2 else 1) +
      labs(title = "Response time per repetition", x = "Repetition", y = "Response time (s)",
           col = names(groupings)[groupings == input$grouping]) +
      # theme_minimal() +
      theme(legend.position = if (input$grouping == "gameId") "none" else "inside",
            legend.position.inside = c(0.9, 0.85)) +
      geom_smooth(aes(col = if (input$grouping == "gameId") "black" else as.factor(.data[[input$grouping]]),
                      group = if (input$grouping == "gameId") 1 else as.factor(.data[[input$grouping]])), 
                  method = "lm", formula = y ~ log(x), se = (input$grouping == "gameId"), linetype = "dashed")
  })
}

# Run App
shinyApp(ui = ui, server = server)

