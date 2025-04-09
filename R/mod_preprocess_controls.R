# R/mod_preprocess_controls.R

#' preprocess_controls UI Function
#'
#' @description UI for preprocessing controls and visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
mod_preprocess_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    # shinyjs::useShinyjs(), # Removed this line
    fluidRow(
      column(4,
             selectInput(ns("aggregationLevel"), "Aggregation Level",
                         choices = c("Daily", "Weekly"), selected = "Daily")
      ),
      column(4,
             conditionalPanel(
               condition = paste0("input['", ns("aggregationLevel"), "'] == 'Weekly'"),
               selectInput(ns("aggregationFunc"), "Weekly Aggregation Function",
                           choices = c("mean", "sum"), selected = "mean")
             )
      ),
      column(4,
             sliderInput(ns("trainTestSplit"), "Train Set Percentage",
                         min = 1, max = 100, value = 80, step = 1, post = "%")
      )
    ),
    hr(),
    plotly::plotlyOutput(ns("tsPlot"))
  )
}

#' preprocess_controls Server Functions
#'
#' @description Server logic for preprocessing. Handles cleaning, aggregation,
#'   train/test split, and visualization based on user inputs and upstream data.
#'
#' @param id Internal parameter for {shiny}.
#' @param data_input_reactives A reactive list from mod_data_input. Expected:
#'   `reactive_df`, `reactive_date_col`, `reactive_value_col`.
#'
#' @return A reactive list containing: `reactive_train_df`, `reactive_test_df`, `reactive_agg_level`.
#'
#' @noRd
#'
#' @import shiny
#' @import dplyr
#' @import lubridate
#' @import plotly
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @importFrom shinyjs disable enable
#' @importFrom dplyr slice
mod_preprocess_controls_server <- function(id, data_input_reactives){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- Existing server logic (no changes needed inside the function) ---
    cleaned_data <- reactive({
      req(data_input_reactives$reactive_df())
      req(data_input_reactives$reactive_date_col())
      req(data_input_reactives$reactive_value_col())
      raw_df <- data_input_reactives$reactive_df()
      date_col <- data_input_reactives$reactive_date_col()
      value_col <- data_input_reactives$reactive_value_col()
      validate(
        need(date_col %in% names(raw_df), paste("Date column '", date_col, "' not found.")),
        need(value_col %in% names(raw_df), paste("Value column '", value_col, "' not found."))
      )
      df_processed <- tryCatch({
        raw_df %>%
          dplyr::select(ds = !!rlang::sym(date_col), y = !!rlang::sym(value_col)) %>%
          dplyr::mutate(ds = lubridate::as_date(ds)) %>%
          dplyr::mutate(y = as.numeric(y)) %>%
          dplyr::filter(!is.na(ds), !is.na(y)) %>%
          dplyr::arrange(ds) %>%
          dplyr::distinct(ds, .keep_all = TRUE)
      }, error = function(e) {
        shiny::showNotification(paste("Error during data cleaning:", e$message), type = "error", duration = 10)
        return(dplyr::tibble(ds = as.Date(character()), y = numeric()))
      })
      validate(
        need(nrow(df_processed) > 0, "No valid data rows remaining after cleaning (check date/value formats and NAs)."),
        need(inherits(df_processed$ds, "Date"), "Date column conversion failed."),
        need(is.numeric(df_processed$y), "Value column conversion to numeric failed.")
      )
      return(df_processed)
    })

    aggregated_data <- reactive({
      req(cleaned_data())
      clean_df <- cleaned_data()
      agg_level <- input$aggregationLevel

      validate(need(nrow(clean_df) > 0, "Cannot aggregate empty data."))

      if (agg_level == "Weekly") {
        req(input$aggregationFunc) # Require function selection if Weekly
        agg_func_selected <- input$aggregationFunc

        # Remove the line: agg_func_sym <- rlang::sym(agg_func_selected)

        validate(need(agg_func_selected %in% c("mean", "sum"), "Invalid aggregation function selected."))

        df_agg <- tryCatch({
          grouped_df <- clean_df %>%
            dplyr::group_by(ds = lubridate::floor_date(ds, "week", week_start = getOption("lubridate.week.start", 1)))

          # --- Use if/else based on selected function ---
          if (agg_func_selected == "mean") {
            summary_df <- grouped_df %>%
              dplyr::summarise(y = mean(y, na.rm = TRUE), .groups = 'drop')
          } else if (agg_func_selected == "sum") {
            summary_df <- grouped_df %>%
              dplyr::summarise(y = sum(y, na.rm = TRUE), .groups = 'drop')
          } else {
            # This case should ideally not happen due to the selectInput choices,
            # but it's good practice to handle unexpected values.
            stop("Unsupported aggregation function specified.") # This will be caught by tryCatch
          }
          # --- End if/else ---

          summary_df %>% dplyr::arrange(ds) # Arrange after summarising

        }, error = function(e){
          shiny::showNotification(
            # The error message 'e$message' will now be more direct if stop() is called above
            paste("Error during weekly aggregation:", e$message), type = "error", duration = 10
          )
          return(dplyr::tibble(ds = as.Date(character()), y = numeric()))
        })

        validate(need(nrow(df_agg) > 0, "Weekly aggregation resulted in empty data."))
        return(df_agg)

      } else { # Daily aggregation (no change)
        return(clean_df)
      }
    })

    split_index <- reactive({
      req(aggregated_data())
      df <- aggregated_data()
      validate(need(nrow(df) >= 2, "Need at least 2 data points after aggregation to perform train/test split."))
      split_perc <- input$trainTestSplit / 100
      idx <- max(1, floor(nrow(df) * split_perc))
      if(split_perc == 1) idx <- nrow(df)
      return(idx)
    })

    train_df <- reactive({
      req(aggregated_data(), split_index())
      aggregated_data() %>% dplyr::slice(1:split_index())
    })

    test_df <- reactive({
      req(aggregated_data(), split_index())
      df <- aggregated_data()
      idx <- split_index()
      if (idx < nrow(df)) {
        df %>% dplyr::slice((idx + 1):nrow(df))
      } else {
        df %>% dplyr::slice(0)
      }
    })

    output$tsPlot <- plotly::renderPlotly({
      req(train_df())
      train_data <- train_df()
      test_data <- test_df()
      validate(need(nrow(train_data) > 0, "No training data available to plot."))
      p <- plot_ly() %>%
        add_trace(data = train_data, x = ~ds, y = ~y, type = 'scatter', mode = 'lines+markers', name = 'Train', line = list(color = '#1f77b4'), marker = list(color = '#1f77b4', size=4))
      if (nrow(test_data) > 0) {
        p <- p %>% add_trace(data = test_data, x = ~ds, y = ~y, type = 'scatter', mode = 'lines+markers', name = 'Test', line = list(color = '#ff7f0e'), marker = list(color = '#ff7f0e', size=4))
      }
      if (nrow(test_data) > 0 && nrow(train_data) > 0) {
        split_date <- max(train_data$ds)
        p <- p %>% layout(shapes = list(
          list(type = "line", fillcolor = "grey", line = list(color = "grey", dash = "dash"), opacity = 0.8,
               x0 = split_date, x1 = split_date, xref = "x", y0 = 0, y1 = 1, yref = "paper")
        ))
      }
      p %>% layout(
        title = "Processed Time Series (Train/Test Split)",
        xaxis = list(title = "Date", rangeslider = list(visible=TRUE)),
        yaxis = list(title = "Value"),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
        hovermode = "x unified"
      )
    })

    return(
      list(
        reactive_train_df = train_df,
        reactive_test_df = test_df,
        reactive_agg_level = reactive({ input$aggregationLevel }),
        reactive_aggregated_df = aggregated_data
      )
    )
    # --- End server logic ---

  })
}
