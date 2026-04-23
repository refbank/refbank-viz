library(shiny)
library(tidyverse)
library(stringi)
library(here)
library(redivis)

source(here("theme.R"))
source(here("get_data.R"))

source <- "cached"
next_version <- TRUE

if (source == "redivis") {
  con <- redivis$user("mcfrank")$dataset("refbank:2zy7")
  df <- con$table("per_game_summary:bsw0")$to_tibble()
}

if (source == "cached") {
  file_loc <- "cached_data"
  check_cache(file_loc, next_version)
  df <- read_csv(here(file_loc, "per_game_summary.csv"))
}

df <- df |> filter(confederates == "no")

option_sizes <- sort(unique(df$option_size))

groupings <- c(
  "Dataset" = "dataset_id", "Group size" = "group_size",
  "Information availability" = "information_availability_composite", "Option set size" = "option_size",
  "Modality" = "modality", "Backchannel" = "backchannel", "Feedback" = "feedback", "Role constancy" = "role_constancy"
)

facetings <- c(
  "None" = "dataset_id", "Group size" = "group_size",
  "Information availability" = "information_availability_composite", "Option set size" = "option_size",
  "Modality" = "modality", "Backchannel" = "backchannel", "Feedback" = "feedback", "Role constancy" = "role_constancy"
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
      geom_smooth(aes(group = if (stage_one_only) 1 else stage_num, weight = trials),
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
  } else if (grouping == "information_availability_composite") {
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

make_stack_plot <- function(df, y, grouping, faceting, indiv_lines, stage_one_only,
                            title, y_lab, legend_pos) {
  if (faceting != "dataset_id") {
    grouped_df <- df |>
      group_by(rep_num, .data[[faceting]], .data[[grouping]]) |>
      summarize(y = mean(.data[[y]], na.rm = T))
  } else {
    grouped_df <- df |>
      group_by(rep_num, .data[[grouping]]) |>
      summarize(y = mean(.data[[y]], na.rm = T))
  }
  p <- ggplot(
    grouped_df,
    aes(
      x = rep_num, y = y, fill = .data[[grouping]]
    )
  )

  p <- p + geom_area(position = "stack")


  p <- p +
    scale_x_continuous(breaks = if (max(df$rep_num, na.rm = TRUE) > 6) seq(0, 12, 2) else 1:6) +
    labs(
      title = title, x = "Repetition", y = y_lab,
      fill = names(groupings)[groupings == grouping]
    )

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
        "option_size",
        "Option set size",
        inline = T,
        choices = sort(unique(df$option_size)),
        selected = unique(df$option_size),
      ),
      checkboxGroupInput(
        "population",
        "Age groups",
        inline = T,
        choices = sort(unique(df$population)),
        selected = unique(df$population),
      ),
      checkboxGroupInput(
        "modality",
        "Modality",
        inline = T,
        choices = sort(unique(df$modality)),
        selected = unique(df$modality),
      ),
      checkboxGroupInput(
        "feedback",
        "Feedback on selection accuracy",
        inline = T,
        choices = c("none", "limited", "full"),
        selected = c("none", "limited", "full"),
      ),
      checkboxGroupInput(
        "backchannel",
        "Matcher backchannel",
        inline = T,
        choices = c("none", "limited", "full"),
        selected = c("none", "limited", "full"),
      ),
      checkboxGroupInput(
        "partner_constancy",
        "Partner constancy",
        inline = T,
        choices = c("Swaps" = "no", "No swaps" = "yes"),
        selected = c("no", "yes"),
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
          title = "PoS analyses",
          style = "padding: 15px 0px",
          plotOutput("hedge_plot"),
          plotOutput("def_plot"),
          plotOutput("pos_plot"),
          plotOutput("pos_stack_plot", height = 350),
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
  next_version <- TRUE
  if (source == "redivis") {
    con <- redivis$user("mcfrank")$dataset("refbank:2zy7")
    df <- con$table("per_game_summary:bsw0")$to_tibble()
  }

  if (source == "cached") {
    file_loc <- "cached_data"
    check_cache(file_loc, next_version)
    df <- read_csv(here(file_loc, "per_game_summary.csv"))
  }

  option_sizes <- sort(unique(df$option_size))

  observe({
    updateSelectInput(session, "dataset", choices = unique(df$dataset_id), selected = setdiff(unique(df$dataset_id), c("yoon2019_audience")))
    updateCheckboxGroupInput(session, "group_size", inline = T, choices = sort(unique(df$group_size)), selected = unique(df$group_size))
    updateCheckboxGroupInput(session, "option_size", inline = T, choices = sort(unique(df$option_size)), selected = unique(df$option_size))
    updateCheckboxGroupInput(session, "population", inline = T, choices = sort(unique(df$population)), selected = unique(df$population))
    updateCheckboxGroupInput(session, "modality", inline = T, choices = sort(unique(df$modality)), selected = unique(df$modality))
  })

  metadata_cols <- c(
    "game_id", "rep_num", "stage_num", "option_size", "order_match",
    "condition_id", "dataset_id", "condition_label", "group_size",
    "prior_relationship", "partner_constancy", "role_constancy", "population",
    "confederates", "modality", "feedback", "backchannel", "language", "information_availability_composite"
  )
  filter_to_input <- function(df) {
    if (input$stage_one_only) {
      df <- df |>
        filter(stage_num == 1)
    }
    df |>
      filter(dataset_id %in% input$dataset) |>
      filter(input$rep[1] <= rep_num & rep_num <= input$rep[2]) |>
      filter(group_size %in% input$group_size) |>
      filter(option_size %in% input$option_size) |>
      filter(population %in% input$population) |>
      filter(modality %in% input$modality) |>
      filter(feedback %in% input$feedback) |>
      filter(backchannel %in% input$backchannel) |>
      filter(partner_constancy %in% input$partner_constancy)
  }

  make_line_output <- function(col, title, y_lab, legend_pos, ylim = NULL) {
    renderPlot({
      req(input$rep)
      plot_df <- df |>
        filter_to_input() |>
        filter(!is.na(.data[[col]]))
      if (nrow(plot_df) == 0) {
        return(NULL)
      }
      p <- make_line_plot(
        plot_df, col, input$grouping, input$faceting,
        input$indiv_lines, input$stage_one_only,
        title, y_lab, legend_pos
      )
      if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
      p
    })
  }

  output$word_plot <- make_line_output(
    "words", "Reduction: Total speaker utterance length across repetitions",
    "Length (words)", c(0.85, 0.75),
    ylim = c(0, 50)
  )

  output$accuracy_plot <-
    make_line_output(
      "accuracy", "Accuracy across repetitions",
      "Accuracy", c(0.85, 0.25)
    )

  output$time_plot <- make_line_output(
    "rt", "Response time across repetitions",
    "Response time (s)", c(0.85, 0.75)
  )


  output$toprev_plot <- make_line_output(
    "to_next",
    "Convergence: Similarity to previous round utterance within game",
    "Cosine similarity", c(0.85, 0.25)
  )

  output$diverge_plot <- make_line_output(
    "diverge",
    "Divergence: Similarity to other games within rounds",
    "Cosine similarity", c(0.85, 0.75)
  )

  output$tardiff_plot <- make_line_output(
    "diff",
    "Differentiation: Similarity to other same-round targets within game",
    "Cosine similarity", c(0.85, 0.75)
  )


  output$idio_plot <- make_line_output(
    "idiosyncrasy",
    "Idiosyncrasy: Similarity to mean of round 1 utterances",
    "Cosine similarity", c(0.85, 0.75)
  )


  output$hedge_plot <- make_line_output("n_hedges",
    "Hedges across repetitions",
    "Hedges per trial", c(0.85, 0.75),
    ylim = c(0, 2)
  )

  output$def_plot <- renderPlot({
    req(input$rep)


    df <- df |>
      filter_to_input() |>
      pivot_longer(c("bare", "definite", "indefinite", "quantifier", "proper_noun", "demonstrative", "possessive"), names_to = "np_type", values_to = "np_count") |>
      filter(!is.na(np_type), !is.na(np_count))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "np_count",
      "np_type", input$faceting, F, input$stage_one_only,
      "Fraction of different types of NPs",
      "Fraction of NPs", c(0.85, 0.75)
    ) +
      coord_cartesian(ylim = c(0, 1))
  })

  output$pos_plot <- renderPlot({
    req(input$rep)


    df <- df |>
      filter_to_input() |>
      pivot_longer(c(
        "NOUN", "VERB",
        "DET", "PRON", "MODIFIER", "FUNCTION"
      ), names_to = "pos_type", values_to = "pos_frac") |>
      filter(!is.na(pos_type), !is.na(pos_frac)) |>
      mutate(pos_type = factor(pos_type, levels = c("NOUN", "VERB", "MODIFIER", "DET", "PRON", "FUNCTION")))

    if (nrow(df) == 0) {
      return(NULL)
    }

    make_line_plot(
      df, "pos_frac",
      input$grouping, "pos_type", input$indiv_lines, input$stage_one_only,
      "Part of speech rate",
      "Part of speech fraction", c(0.85, 0.75)
    ) +
      coord_cartesian(ylim = c(0, 1))
  })

  output$pos_stack_plot <- renderPlot({
    req(input$rep)
    df <- df |>
      filter_to_input() |>
      group_by(across(all_of(metadata_cols))) |>
      pivot_longer(c(
        "NOUN", "VERB",
        "DET", "PRON", "MODIFIER", "FUNCTION"
      ), names_to = "pos_type", values_to = "pos_frac") |>
      filter(!is.na(pos_type), !is.na(pos_frac)) |>
      mutate(pos_type = factor(pos_type, levels = c("NOUN", "VERB", "MODIFIER", "DET", "PRON", "FUNCTION")))


    if (nrow(df) == 0) {
      return(NULL)
    }

    make_stack_plot(
      df, "pos_frac",
      "pos_type", input$faceting, input$indiv_lines, input$stage_one_only,
      "Part of speech fraction",
      "Part of speech fraction", c(0.85, 0.75)
    ) +
      coord_cartesian(ylim = c(0, 1))
  })
}

###### App ######
shinyApp(ui = ui, server = server)
