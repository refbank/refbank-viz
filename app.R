library(shiny)
library(tidyverse)
library(stringi)
library(here)
library(redivis)

source(here("theme.R"))
source(here("get_data.R"))

source <- "cached"

if (source == "redivis") {
  con <- redivis$user("mcfrank")$dataset("refbank:2zy7")
  df <- con$table("per_game_summary:bsw0")$to_tibble()
}

if (source == "cached") {
  file_loc <- "cached_data"
  check_cache(file_loc)
  df <- read_csv(here(file_loc, "per_game_summary.csv"))
}



option_sizes <- sort(unique(df$option_size))

groupings <- c(
  "Dataset" = "dataset_id", "Group size" = "group_size",
  "Structure" = "structure", "Option set size" = "option_size"
)

facetings <- c(
  "None" = "dataset_id", "Group size" = "group_size",
  "Structure" = "structure", "Option set size" = "option_size"
)

make_line_plot <- function(df, y, grouping, faceting, indiv_lines, stage_one_only,
                           title, y_lab, legend_pos) {
  p <- ggplot(
    df,
    aes(
      x = rep_num, y = .data[[y]],
      color = as.factor(.data[[grouping]])
    )
  )

  if (indiv_lines) {
    p <- p +
      geom_line(aes(group = game_id), alpha = 0.05)
  }

  p <- p +
    geom_point(alpha = 0.05)

  if (grouping == "dataset_id") {
    p <- p +
      geom_smooth(aes(group = if (stage_one_only) 1 else stage_num),
        method = "lm", formula = y ~ log(x),
        se = TRUE,
        col = "black",
        lty = "dashed", linewidth = 1.5
      )
  } else {
    p <- p +
      geom_smooth(
        aes(
          col = as.factor(.data[[grouping]]),
          group = if (stage_one_only) {
            as.factor(.data[[grouping]])
          } else {
            interaction(stage_num, as.factor(.data[[grouping]]))
          },
          weight = trials,
        ),
        method = "lm", formula = y ~ log(x),
        se = FALSE,
        lty = "dashed", linewidth = 1.5
      )
  }

  p <- p +
    scale_x_continuous(breaks = if (max(df$rep_num, na.rm = TRUE) > 6) seq(0, 12, 2) else 1:6) +
    labs(
      title = title, x = "Repetition", y = y_lab,
      col = names(groupings)[groupings == grouping]
    )

  if (grouping == "group_size") {
    p <- p + GRP_SIZE_COL_SCALE
  } else if (grouping == "structure") {
    p <- p + STRUCT_COL_SCALE
  } else if (grouping == "option_size") {
    p <- p + OPT_SIZE_COL_SCALE
  }

  if (faceting != "dataset_id") {
    p <- p +
      facet_grid(~ as.factor(.data[[faceting]])) +
      theme(legend.position = if (grouping == "dataset_id") "none" else "bottom")
  } else {
    p <- p +
      theme(
        legend.position = if (grouping == "dataset_id") "none" else "inside",
        legend.position.inside = legend_pos
      )
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
      checkboxGroupInput(
        "group_size",
        "Group size",
        inline = T,
        choices = sort(unique(df$group_size)),
        selected = unique(df$group_size),
      ),
      checkboxGroupInput(
        "structure",
        "Structure",
        inline = T,
        choices = c("thin", "medium", "med_thick", "thick", "network-swap", "naive-swap"),
        selected = c("thin", "medium", "med_thick", "thick", "network-swap", "naive-swap"),
      ),
      checkboxGroupInput(
        "option_size",
        "Option set size",
        inline = T,
        choices = sort(unique(df$option_size)),
        selected = unique(df$option_size),
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
        choices = facetings,
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
      ),
      selectInput(
        "dataset",
        "Dataset:",
        choices = unique(df$dataset_id),
        selected = setdiff(unique(df$dataset_id), c("yoon2019_audience")),
        multiple = TRUE
      ),
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
server <- function(input, output, session) {
  source <- "cached"

  if (source == "redivis") {
    con <- redivis$user("mcfrank")$dataset("refbank:2zy7")
    df <- con$table("per_game_summary:bsw0")$to_tibble()
  }

  if (source == "cached") {
    file_loc <- "cached_data"
    check_cache(file_loc)
    df <- read_csv(here(file_loc, "per_game_summary.csv"))
  }

  option_sizes <- sort(unique(df$option_size))

  observe({
    updateSelectInput(session, "dataset", choices = unique(df$dataset_id), selected = setdiff(unique(df$dataset_id), c("yoon2019_audience")))
    updateCheckboxGroupInput(session, "group_size", inline = T, choices = sort(unique(df$group_size)), selected = unique(df$group_size))
    updateCheckboxGroupInput(session, "option_size", inline = T, choices = sort(unique(df$option_size)), selected = unique(df$option_size))
  })

  output$word_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(words))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "words",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Reduction: Total speaker utterance length across repetitions",
      "Length (words)", c(0.85, 0.75)
    ) +
      coord_cartesian(ylim = c(0, 50))
  })

  output$accuracy_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(accuracy))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "accuracy",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Accuracy across repetitions",
      "Accuracy", c(0.85, 0.25)
    )
  })

  output$time_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(rt))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "rt",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Response time across repetitions",
      "Response time (s)", c(0.85, 0.8)
    )
  })

  output$toprev_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(to_next))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "to_next",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Convergence: Similarity to previous round utterance within game",
      "Cosine similarity", c(0.85, 0.25)
    )
  })

  output$diverge_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(diverge))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "diverge",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Divergence: Similarity to other games within rounds",
      "Cosine similarity", c(0.85, 0.25)
    )
  })

  output$tardiff_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(diff))

    if (nrow(df) == 0) {
      return(NULL)
    }


    make_line_plot(
      df, "diff",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Differentiation: Similarity to other same-round targets within game",
      "Cosine similarity", c(0.85, 0.25)
    )
  })

  output$idio_plot <- renderPlot({
    req(input$rep)

    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }

    df <- df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(!is.na(idiosyncrasy))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "idiosyncrasy",
      input$grouping, input$faceting, input$indiv_lines, input$stage_one_only,
      "Idiosyncrasy: Similarity to mean of round 1 utterances",
      "Cosine similarity", c(0.85, 0.25)
    )
  })
}

###### App ######
shinyApp(ui = ui, server = server)
