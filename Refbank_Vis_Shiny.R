library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load Data
chat_csv <- read.csv("C:\\Users\\zihan\\Downloads\\multiparty-tangrams-main\\data\\study1\\chat.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Repetition Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "rep", 
        "Select Repetition(s):", 
        choices = c("1" = 0, "2" = 1, "3" = 2, "4" = 3, "5" = 4, "6" = 5),
        selected = 0  # Default selection
      ),
      selectInput(
        "game", 
        "Select Game ID:", 
        choices = c("All Games", unique(chat_csv$gameId)),  
        selected = "All Games"
      )
    ),
    mainPanel(
      plotOutput("word_plot"),
      plotOutput("accuracy_plot"),
      plotOutput("time_plot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  output$word_plot <- renderPlot({
    req(input$rep)
    
    rep_df <- chat_csv %>%
      group_by(gameId, repNum) %>%
      summarise(total_num_words = sum(total_num_words, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(total_num_words))
    
    filtered_data <- rep_df %>% filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All Games") {
      filtered_data <- filtered_data %>% filter(gameId == input$game)
    }
    
    if (nrow(filtered_data) == 0) return(NULL)
    
    enough_points <- length(unique(filtered_data$repNum)) > 1
    
    p <- ggplot(filtered_data, aes(x = repNum, y = total_num_words, color = as.factor(gameId), group = gameId)) +
      geom_line() +
      geom_point() +
      labs(title = "Total Number of Words Per Repetition", x = "Repetition Number", y = "Total Words") +
      theme_minimal() +
      theme(legend.position = "none")
    
    if (enough_points) {
      p <- p + geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linetype = "dashed")
    }
    
    p
  })
  
  output$accuracy_plot <- renderPlot({
    req(input$rep)
    
    correct_data <- chat_csv %>%
      filter(!is.na(submitted) & !is.na(correct) & submitted == TRUE & correct == TRUE) %>%
      group_by(gameId, repNum) %>%
      summarise(submitted_correct = n(), .groups = "drop")
    
    total_data <- chat_csv %>%
      filter(!is.na(submitted) & submitted == TRUE) %>%
      group_by(gameId, repNum) %>%
      summarise(submitted_total = n(), .groups = "drop")
    
    accuracy_data <- full_join(correct_data, total_data, by = c("gameId", "repNum")) %>%
      replace_na(list(submitted_correct = 0, submitted_total = 0)) %>%
      mutate(accuracy = ifelse(submitted_total > 0, submitted_correct / submitted_total, 0))
    
    avg_accuracy <- accuracy_data %>%
      group_by(repNum) %>%
      summarise(accuracy = mean(accuracy, na.rm = TRUE), .groups = "drop") %>%
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All Games") {
      avg_accuracy <- accuracy_data %>%
        filter(gameId == input$game) %>%
        group_by(repNum) %>%
        summarise(accuracy = mean(accuracy, na.rm = TRUE), .groups = "drop")
    }
    
    if (nrow(avg_accuracy) == 0) return(NULL)
    
    ggplot(avg_accuracy, aes(x = factor(repNum), y = accuracy, fill = factor(repNum))) +
      geom_bar(stat = "identity") +
      labs(title = "Average Accuracy by RepNum", x = "Repetition Number", y = "Average Accuracy", fill = "Rep Num") +
      theme_minimal()
  })
  
  output$time_plot <- renderPlot({
    req(input$rep)
    
    reaction_time <- chat_csv %>%
      filter(!is.na(time)) %>%
      group_by(gameId, repNum) %>%
      summarise(avg_time = mean(time), .groups = "drop")
    
    filtered_time <- reaction_time %>%
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All Games") {
      filtered_time <- filtered_time %>% filter(gameId == input$game)
    }
    
    avg_time <- filtered_time %>%
      group_by(repNum) %>%
      summarise(time = mean(avg_time, na.rm = TRUE), .groups = "drop")
    
    if (nrow(avg_time) == 0) return(NULL)
    
    ggplot(avg_time, aes(x = factor(repNum), y = time, fill = factor(repNum))) +
      geom_bar(stat = "identity") +
      labs(title = "Average Time by RepNum", x = "Repetition Number", y = "Average Time", fill = "Rep Num") +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui = ui, server = server)

