library(shiny)
library(tidyverse)
library(stringi)
library(here)

source(here("data_helpers.R"))
source(here("theme.R"))

###### Data loading ######
### NOTE: assumes that refbank-import is in the same dir as refbank-viz
DATA_LOC = here("../refbank-import/harmonized_data")
all_dirs <- list.dirs(DATA_LOC, full.names = FALSE) |> 
  stri_remove_empty()

all_messages <- map(all_dirs, \(d) get_messages_full(DATA_LOC, d)) |> 
  list_rbind() |> 
  mutate(option_size = option_set |> 
           str_split(";") |> 
           lengths()) |> 
  filter(option_size != 1)
all_choices <- map(all_dirs, \(d) get_choices_full(DATA_LOC, d)) |>
  list_rbind() |> 
  mutate(option_size = option_set |> 
           str_split(";") |> 
           lengths()) |> 
  filter(option_size != 1)
all_trials <- map(all_dirs, \(d) get_trials_full(DATA_LOC, d)) |>
  list_rbind() |> 
  mutate(option_size = option_set |> 
           str_split(";") |> 
           lengths()) |> 
  filter(option_size != 1)

groupings <- c("None" = "game_id", "Group size" = "group_size", 
               "Structure" = "structure", "Option set size" = "option_size")
option_sizes <- sort(unique(all_trials$option_size))

make_line_plot <- function(df, y, grouping, faceting, indiv_lines,
                           title, y_lab, legend_pos) {
  p <- ggplot(df, 
              aes(x = rep_num, y = .data[[y]], 
                  color = as.factor(.data[[grouping]]), 
                  group = game_id))
  
  if (indiv_lines) {
    p <- p +
      geom_line(alpha = 0.05)
  }
  
  p <- p +
    geom_point(alpha = 0.05)
  
  if (grouping == "game_id") {
    p <- p +
      geom_smooth(aes(group = 1), 
                  method = "lm", formula = y ~ log(x), 
                  se = TRUE, 
                  col = "black",
                  lty = "dashed", linewidth = 1.5)
  } else {
    p <- p +
      geom_smooth(aes(col = as.factor(.data[[grouping]]),
                      group = as.factor(.data[[grouping]])), 
                  method = "lm", formula = y ~ log(x), 
                  se = FALSE, 
                  lty = "dashed", linewidth = 1.5)
  }
  
  p <- p +
    scale_x_continuous(breaks = if (max(df$rep_num) > 6) seq(0, 12, 2) else 1:6) + 
    labs(title = title, x = "Repetition", y = y_lab,
         col = names(groupings)[groupings == grouping])
  
  if (grouping == "group_size") {
    p <- p + GRP_SIZE_COL_SCALE
  } else if (grouping == "structure") {
    p <- p + STRUCT_COL_SCALE
  } else if (grouping == "option_size") {
    p <- p + OPT_SIZE_COL_SCALE
  }
  
  if (faceting != "game_id") {
    p <- p +
      facet_grid( ~ as.factor(.data[[faceting]])) +
      theme(legend.position = if (grouping == "game_id") "none" else "bottom")
  } else {
    p <- p +
      theme(legend.position = if (grouping == "game_id") "none" else "inside",
            legend.position.inside = legend_pos)
  }
  
  p
}

###### UI / selectors ######
ui <- fluidPage(
  titlePanel("Refbank Visualizations"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "rep",
        "Repetition(s):",
        min = 1, max = 13,
        value = c(1, 6)
      ),
      selectInput(
        "dataset",
        "Dataset:",
        choices = unique(all_trials$paper_id),
        selected = setdiff(unique(all_trials$paper_id), "yoon2019_audience"),
        multiple = TRUE
      ),
      selectInput(
        "grouping",
        "Color by:",
        choices = groupings,
        selected = "None"
      ),
      selectInput(
        "faceting",
        "Facet by:",
        choices = groupings,
        selected = "None"
      ),
      checkboxInput(
        "indiv_lines",
        "Show lines per game",
        value = TRUE
      ),
      checkboxInput(
        "stage_one_only",
        "Only include first stage data",
        value = TRUE
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
    
    rep_df <- all_messages
    if (input$stage_one_only) {
      rep_df <- rep_df |> 
        filter(stage_num == 1)
    }
    
    rep_df <- rep_df |>
      filter(paper_id %in% input$dataset,
             role == "describer",
             !message_irrelevant) |> 
      group_by(game_id, rep_num, group_size, structure, option_size, trial_num) |> 
      summarise(total_num_words = sum(lengths(str_split(text, " ")), na.rm = TRUE), 
                .groups = "drop_last") |>
      summarise(mean_num_words = mean(total_num_words, na.rm = TRUE), 
                .groups = "drop") |>
      filter(!is.na(mean_num_words),
             input$rep[1] <= rep_num & rep_num <= input$rep[2])
    
    if (nrow(rep_df) == 0) return(NULL)
    
    make_line_plot(rep_df, "mean_num_words", 
                   input$grouping, input$faceting, input$indiv_lines,
                   "Total speaker utterance length across repetitions", 
                   "Length (words)", c(0.85, 0.75))
    
  })
  
  output$accuracy_plot <- renderPlot({
    req(input$rep)
    
    correct_data <- all_choices
    if (input$stage_one_only) {
      correct_data <- correct_data |> 
        filter(stage_num == 1)
    }
    
    correct_data <- correct_data |>
      filter(paper_id %in% input$dataset,
             !is.na(choice_id)) |> 
      group_by(game_id, rep_num, group_size, structure, option_size) |>
      summarise(submitted_correct = sum(target == choice_id, na.rm = TRUE),
                submitted_total = n(), .groups = "drop")
    
    accuracy_data <- correct_data |> 
      # replace_na(list(submitted_correct = 0, submitted_total = 0)) |>
      mutate(accuracy = ifelse(submitted_total > 0, submitted_correct / submitted_total, 0)) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2])
    
    if (nrow(accuracy_data) == 0) return(NULL)
    
    make_line_plot(accuracy_data, "accuracy", 
                   input$grouping, input$faceting, input$indiv_lines,
                   "Accuracy across repetitions", 
                   "Accuracy", c(0.85, 0.25))
    
  })
  
  output$time_plot <- renderPlot({
    req(input$rep)
    
    reaction_time <- all_choices
    if (input$stage_one_only) {
      reaction_time <- reaction_time |> 
        filter(stage_num == 1)
    }
    
    reaction_time <- reaction_time |>
      filter(paper_id %in% input$dataset,
             !is.na(time_stamp),
             choice_id != "timed_out") |>
      group_by(game_id, rep_num, group_size, structure, option_size) |>
      summarise(avg_time = mean(time_stamp, na.rm = TRUE), .groups = "drop") |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2])
    
    if (nrow(reaction_time) == 0) return(NULL)
    
    make_line_plot(reaction_time, "avg_time", 
                   input$grouping, input$faceting, input$indiv_lines,
                   "Response time across repetitions", 
                   "Response time (s)", c(0.85, 0.8))
    
  })
}

###### App ######
shinyApp(ui = ui, server = server)
