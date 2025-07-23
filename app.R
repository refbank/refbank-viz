library(shiny)
library(tidyverse)
library(stringi)
library(here)
library(refbankr)

source(here("theme.R"))
source(here("get_data.R"))

###### Data loading ######
source="cached" # "cached" or "redivis"

if (source=="redivis"){
all_datasets <- get_datasets()
all_messages <- get_messages(include_trial_data=T, include_condition_data=T) 
all_choices <- get_choices(include_trial_data = T, include_condition_data = T) 
all_trials <- get_trials(include_condition_data = T) 
all_tonext <- get_cosine_similarities(sim_type="to_next")
all_diverge <- get_cosine_similarities(sim_type="diverge") 
all_targetdiff <- get_cosine_similarities(sim_type="diff")
all_idiosyncrasy <- get_cosine_similarities(sim_type="idiosyncrasy")
}

if (source=="cached"){
  file_loc <- "cached_data"
    check_cache(file_loc)
    all_datasets <- read_csv(here(file_loc, "datasets.csv"))
    all_messages <- read_csv(here(file_loc, "messages.csv"))
    all_choices <- read_csv(here(file_loc, "choices.csv"))
    all_trials <- read_csv(here(file_loc, "trials.csv"))
    all_tonext <- read_csv(here(file_loc, "to_next.csv"))
    all_diverge <- read_csv(here(file_loc, "diverge.csv"))
    all_targetdiff <- read_csv(here(file_loc, "diff.csv"))
    all_idiosyncrasy <- read_csv(here(file_loc, "idiosyncrasy.csv"))
}


groupings <- c("None" = "game_id", "Group size" = "group_size", 
               "Structure" = "structure", "Option set size" = "option_size")

option_sizes <- sort(unique(all_trials$option_size))

make_line_plot <- function(df, y, grouping, faceting, indiv_lines, stage_one_only,
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
      geom_smooth(aes(group = if (stage_one_only) 1 else stage_num), 
                  method = "lm", formula = y ~ log(x), 
                  se = TRUE, 
                  col = "black",
                  lty = "dashed", linewidth = 1.5)
  } else {
    p <- p +
      geom_smooth(aes(col = as.factor(.data[[grouping]]),
                      group = if (stage_one_only) as.factor(.data[[grouping]])
                      else interaction(stage_num, as.factor(.data[[grouping]]))), 
                  method = "lm", formula = y ~ log(x), 
                  se = FALSE, 
                  lty = "dashed", linewidth = 1.5)
  }
  
  p <- p +
    scale_x_continuous(breaks = if (max(df$rep_num, na.rm = TRUE) > 6) seq(0, 12, 2) else 1:6) + 
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
        choices = unique(all_trials$dataset_id),
        selected = setdiff(unique(all_trials$dataset_id), "yoon2019_audience"),
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
      tabsetPanel(
        tabPanel(
          title = "Canonical analyses",
          style = "padding: 15px 0px",
          plotOutput("word_plot"),
          plotOutput("accuracy_plot"),
          plotOutput("time_plot"),
        ),
        tabPanel(
          title = "Embedding analyses",
          style = "padding: 15px 0px",
          plotOutput("toprev_plot"),
          plotOutput("diverge_plot"),
          plotOutput("tardiff_plot"),
          plotOutput("idio_plot")
        )
      )
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
      filter(dataset_id %in% input$dataset,
             role == "describer",
             is.na(message_irrelevant) | !message_irrelevant,
             !is.na(text)) |> 
      group_by(game_id, rep_num, group_size, structure, option_size, stage_num, trial_num) |> 
      summarise(total_num_words = sum(lengths(str_split(text, " ")), na.rm = TRUE), 
                .groups = "drop_last") |>
      summarise(mean_num_words = mean(total_num_words, na.rm = TRUE), 
                .groups = "drop") |>
      filter(!is.na(mean_num_words),
             input$rep[1] <= rep_num & rep_num <= input$rep[2])
    
    if (nrow(rep_df) == 0) return(NULL)
    
    make_line_plot(rep_df, "mean_num_words", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
                   "Reduction: Total speaker utterance length across repetitions", 
                   "Length (words)", c(0.85, 0.75)) +
      coord_cartesian(ylim = c(0, 50))
    
  })
  
  output$accuracy_plot <- renderPlot({
    req(input$rep)
    
    correct_data <- all_choices
    if (input$stage_one_only) {
      correct_data <- correct_data |> 
        filter(stage_num == 1)
    }
    
    correct_data <- correct_data |>
      filter(dataset_id %in% input$dataset,
             !is.na(choice_id)) |> 
      group_by(game_id, rep_num, group_size, structure, option_size, stage_num) |>
      summarise(submitted_correct = sum(target == choice_id, na.rm = TRUE),
                submitted_total = n(), .groups = "drop")
    
    accuracy_data <- correct_data |> 
      # replace_na(list(submitted_correct = 0, submitted_total = 0)) |>
      mutate(accuracy = ifelse(submitted_total > 0, submitted_correct / submitted_total, 0)) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2])
    
    if (nrow(accuracy_data) == 0) return(NULL)
    
    make_line_plot(accuracy_data, "accuracy", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
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
      filter(dataset_id %in% input$dataset,
             !is.na(time_stamp),
             choice_id != "timed_out") |>
      group_by(game_id, rep_num, group_size, structure, option_size, stage_num) |>
      summarise(avg_time = mean(time_stamp, na.rm = TRUE), .groups = "drop") |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2])
    
    if (nrow(reaction_time) == 0) return(NULL)
    
    make_line_plot(reaction_time, "avg_time", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
                   "Response time across repetitions", 
                   "Response time (s)", c(0.85, 0.8))
    
  })
  
  output$toprev_plot <- renderPlot({
    req(input$rep)
    
    toprev_data <- all_tonext
    if (input$stage_one_only) {
      toprev_data <- toprev_data |> 
        filter(stage_num == 1)
    }
    
    toprev_data <- toprev_data |>
      filter(dataset_id %in% input$dataset,
             !is.na(sim)) |>
      mutate(rep_num = later) |> 
      group_by(game_id, rep_num, group_size, structure, option_size, stage_num) |>
      summarise(sim = mean(sim, na.rm = TRUE), .groups = "drop")
    
    if (nrow(toprev_data) == 0) return(NULL)
    
    make_line_plot(toprev_data, "sim", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
                   "Convergence: Similarity to previous round utterance within game", 
                   "Cosine similarity", c(0.85, 0.25))
  })
  
  output$diverge_plot <- renderPlot({
    req(input$rep)
    
    diverge_data <- all_diverge
    if (input$stage_one_only) {
      diverge_data <- diverge_data |> 
        filter(stage_num == 1)
    }
    
    diverge_data <- diverge_data |>
      filter(dataset_id %in% input$dataset,
             !is.na(sim),
             !is.na(target)) |> 
      # mutate(game_id = paste(game_id_1, game_id_2, sep = "_")) |> 
      group_by(condition_id, rep_num, group_size, structure, option_size, stage_num, target) |>
      summarise(sim = mean(sim, na.rm = TRUE), .groups = "drop") |> 
      mutate(game_id = paste(condition_id, target, sep = "_"))
    
    if (nrow(diverge_data) == 0) return(NULL)
    
    make_line_plot(diverge_data, "sim", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
                   "Divergence: Similarity to other games within rounds", 
                   "Cosine similarity", c(0.85, 0.25))
  })
  
  output$tardiff_plot <- renderPlot({
    req(input$rep)
    
    tardiff_data <- all_targetdiff
    if (input$stage_one_only) {
      tardiff_data <- tardiff_data |> 
        filter(stage_num == 1)
    }
    
    tardiff_data <- tardiff_data |>
      filter(dataset_id %in% input$dataset,
             !is.na(sim)) |> 
      mutate(target = paste(target_1, target_2, sep = "_")) |> 
      group_by(game_id, rep_num, group_size, structure, option_size, stage_num) |>
      summarise(sim = mean(sim, na.rm = TRUE), .groups = "drop")
    
    if (nrow(tardiff_data) == 0) return(NULL)
    
    make_line_plot(tardiff_data, "sim", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
                   "Differentiation: Similarity to other same-round targets within game", 
                   "Cosine similarity", c(0.85, 0.25))
  })
  
  output$idio_plot <- renderPlot({
    req(input$rep)
    
    idio_data <- all_idiosyncrasy
    if (input$stage_one_only) {
      idio_data <- idio_data |> 
        filter(stage_num == 1)
    }
    
    idio_data <- idio_data |>
      filter(dataset_id %in% input$dataset,
             !is.na(sim),
             !is.na(structure)) |> 
      group_by(game_id, rep_num, group_size, structure, option_size, stage_num) |>
      summarise(sim = mean(sim, na.rm = TRUE), .groups = "drop")
    
    if (nrow(idio_data) == 0) return(NULL)
    
    make_line_plot(idio_data, "sim", 
                   input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
                   "Idiosyncrasy: Similarity to mean of round 1 utterances", 
                   "Cosine similarity", c(0.85, 0.25))
  })
}

###### App ######
shinyApp(ui = ui, server = server)
