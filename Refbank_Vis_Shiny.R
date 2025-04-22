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
        selected = 0
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

# Define Server
server <- function(input, output) {
  
  output$word_plot <- renderPlot({
    req(input$rep)
    
    rep_df <- chat_csv %>%
      group_by(gameId, repNum) %>%
      summarise(total_num_words = sum(total_num_words, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(total_num_words)) %>%
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All Games") {
      rep_df <- rep_df %>% filter(gameId == input$game)
    }
    
    if (nrow(rep_df) == 0) return(NULL)
    
    ggplot(rep_df, aes(x = repNum, y = total_num_words, color = as.factor(gameId), group = gameId)) +
      geom_line() +
      geom_point() +
      labs(title = "Total Number of Words Per Repetition", x = "Repetition Number", y = "Total Words") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linetype = "dashed")
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
      mutate(accuracy = ifelse(submitted_total > 0, submitted_correct / submitted_total, 0)) %>%
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All Games") {
      accuracy_data <- accuracy_data %>% filter(gameId == input$game)
    }
    
    if (nrow(accuracy_data) == 0) return(NULL)
    
    ggplot(accuracy_data, aes(x = repNum, y = accuracy, color = as.factor(gameId), group = gameId)) +
      geom_line() +
      geom_point() +
      labs(title = "Accuracy Across Repetitions", x = "Repetition Number", y = "Accuracy") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linetype = "dashed")
  })
  
  output$time_plot <- renderPlot({
    req(input$rep)
    
    reaction_time <- chat_csv %>%
      filter(!is.na(time)) %>%
      group_by(gameId, repNum) %>%
      summarise(avg_time = mean(time, na.rm = TRUE), .groups = "drop") %>%
      filter(repNum %in% as.numeric(input$rep))
    
    if (input$game != "All Games") {
      reaction_time <- reaction_time %>% filter(gameId == input$game)
    }
    
    if (nrow(reaction_time) == 0) return(NULL)
    
    ggplot(reaction_time, aes(x = repNum, y = avg_time, color = as.factor(gameId), group = gameId)) +
      geom_line() +
      geom_point() +
      labs(title = "Average Time per Repetition", x = "Repetition Number", y = "Average Time") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linetype = "dashed")
  })
}

# Run App
shinyApp(ui = ui, server = server)

