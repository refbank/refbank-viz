library(shiny)
library(ggplot2)
library(dplyr)

# Load Data
chat_csv <- read.csv("C:\\Users\\zihan\\Downloads\\multiparty-tangrams-main\\data\\study1\\chat.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Total Number of Words Per Repetition"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "rep", 
        "Select Repetition(s):", 
        choices = c(
          "1" = 0,
          "2" = 1, 
          "3" = 2, 
          "4" = 3,
          "5" = 4,
          "6" = 5),
        selected = 0  # Default selection
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$plot <- renderPlot({
    req(input$rep)  # Ensure at least one checkbox is selected
    
    # Aggregate total words per game and repetition
    rep_df <- chat_csv %>%
      group_by(gameId, repNum) %>%
      summarise(total_num_words = sum(total_num_words, na.rm = TRUE), .groups = "drop")
    
    # Filter data based on selected repetitions
    filtered_data <- rep_df %>% filter(repNum %in% as.numeric(input$rep))
    
    # Check if filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    # Plot total number of words per repetition
    ggplot(filtered_data, aes(x = repNum, y = total_num_words, color = as.factor(gameId), group = gameId)) +
      geom_line() +
      geom_point() +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linetype = "dashed") +  # Regression line
      labs(
        title = "Total Number of Words Per Repetition",
        x = "Repetition Number",
        y = "Total Number of Words",
        color = "Game ID"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # Hide legend but keep colors distinct
  })
}

# Run App
shinyApp(ui = ui, server = server)
