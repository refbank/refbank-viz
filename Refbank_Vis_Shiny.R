library(shiny)
library(tidyverse)
library(here)
theme_set(theme_classic(base_size = 14))

###### Data loading ######
### NOTE: assumes that refbank-import is in the same dir as refbank-viz
DATA_LOC = here("../refbank-import/harmonized_data")
all_files <- list.files(DATA_LOC)
all_data <- map(all_files, \(f) {
  read_csv(here(DATA_LOC, f), show_col_types = FALSE) # |>
    # mutate(dataset = str_remove(f, "\\.csv"))
}) |> list_rbind()

groupings <- c("None" = "game_id", "Group size" = "group_size", "Structure" = "structure")

make_line_plot <- function(df, y, game, grouping, title, y_lab, legend_pos) {
  ggplot(df, 
         aes(x = rep_num, y = .data[[y]], 
             color = as.factor(.data[[grouping]]), 
             group = game_id)) +
    geom_line(alpha = if (game == "All games") 0.15 else 1) +
    geom_point(alpha = if (game == "All games") 0.15 else 1) +
    labs(title = title, x = "Repetition", y = y_lab,
         col = names(groupings)[groupings == grouping]) +
    theme(legend.position = if (grouping == "game_id") "none" else "inside",
          legend.position.inside = legend_pos) +
    geom_smooth(aes(col = if (grouping == "game_id") "black" else as.factor(.data[[grouping]]),
                    group = if (grouping == "game_id") 1 else as.factor(.data[[grouping]])), 
                method = "lm", formula = y ~ log(x), 
                se = (grouping == "game_id"), linetype = "dashed")
}

###### UI / selectors ######
ui <- fluidPage(
  titlePanel("Refbank Visualizations"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "rep", 
        "Repetition(s):", 
        choices = 1:12,
        selected = 1:6
      ),
      selectInput(
        "dataset",
        "Dataset:",
        choices = unique(all_data$paper_id),
        selected = unique(all_data$paper_id),
        multiple = TRUE
      ),
      selectInput(
        "game", 
        "Game ID:", 
        choices = c("All games", unique(all_data$game_id)),  
        selected = "All games"
      ),
      selectInput(
        "grouping",
        "Color by:",
        choices = groupings,
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

###### Server & plots ######
server <- function(input, output) {
  
  output$word_plot <- renderPlot({
    req(input$rep)
    
    rep_df <- all_data |>
      filter(paper_id %in% input$dataset) |> 
      group_by(game_id, rep_num, group_size) |>
      filter(role == "describer") |> 
      summarise(total_num_words = sum(length(str_split(message, " ")), na.rm = TRUE), .groups = "drop") |>
      filter(!is.na(total_num_words),
             rep_num %in% as.numeric(input$rep))
    
    if (input$game != "All games") {
      rep_df <- rep_df |> filter(input$grouping == input$game)
    }
    
    if (nrow(rep_df) == 0) return(NULL)
    
    make_line_plot(rep_df, "total_num_words", 
                   input$game, input$grouping, 
                   "Total speaker utterance length across repetitions", 
                   "Length (words)", c(0.9, 0.85))
    
  })
  
  output$accuracy_plot <- renderPlot({
    req(input$rep)
    
    correct_data <- all_data |>
      filter(paper_id %in% input$dataset) |> 
      filter(!is.na(choice_id)) |> 
      group_by(game_id, rep_num, group_size) |>
      summarise(submitted_correct = sum(target_id == choice_id, na.rm = TRUE),
                submitted_total = n(), .groups = "drop")
    
    accuracy_data <- correct_data |> 
      # replace_na(list(submitted_correct = 0, submitted_total = 0)) |>
      mutate(accuracy = ifelse(submitted_total > 0, submitted_correct / submitted_total, 0)) |>
      filter(rep_num %in% as.numeric(input$rep))
    
    if (input$game != "All games") {
      accuracy_data <- accuracy_data |> filter(input$grouping == input$game)
    }
    
    if (nrow(accuracy_data) == 0) return(NULL)
    
    make_line_plot(accuracy_data, "accuracy", 
                   input$game, input$grouping, 
                   "Accuracy across repetitions", 
                   "Accuracy", c(0.9, 0.2))
    
  })
  
  output$time_plot <- renderPlot({
    req(input$rep)
    
    reaction_time <- all_data |>
      filter(paper_id %in% input$dataset) |> 
      filter(!is.na(time_to_choice)) |>
      group_by(game_id, rep_num, group_size) |>
      summarise(avg_time = mean(time_to_choice, na.rm = TRUE), .groups = "drop") |>
      filter(rep_num %in% as.numeric(input$rep))
    
    if (input$game != "All games") {
      reaction_time <- reaction_time |> filter(input$grouping == input$game)
    }
    
    if (nrow(reaction_time) == 0) return(NULL)
    
    make_line_plot(reaction_time, "avg_time", 
                   input$game, input$grouping, 
                   "Response time across repetitions", 
                   "Response time (s)", c(0.9, 0.85))
    
  })
}

###### App ######
shinyApp(ui = ui, server = server)

