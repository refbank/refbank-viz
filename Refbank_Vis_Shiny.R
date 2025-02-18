library(shiny)
library(ggplot2)

# Load Data
chat_csv <- read.csv("C:\\Users\\zihan\\Downloads\\multiparty-tangrams-main\\data\\study1\\chat.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "rep", 
        "Checkbox group", 
        choices = c(
          "1" = 0,
          "2" = 1, 
          "3" = 2, 
          "4" = 3,
          "5" = 4,
          "6" = 5))
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
    
    # Correct subsetting
    x <- chat_csv[chat_csv$repNum %in% as.numeric(input$rep), ]
    
    # Check if data is empty after filtering
    if (nrow(x) == 0) {
      return(NULL)  # Avoid plotting an empty dataset
    }
    
    ggplot(x, aes(x = factor(gameId), y = total_num_words, fill = factor(repNum))) +
      geom_bar(stat = "identity") +
      labs(title = "Sum of Words Per Game", x = "Game ID", y = "Total Number of Words") +
      theme_minimal()
  })
}

# Run App
shinyApp(ui = ui, server = server)

