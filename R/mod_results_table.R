# R/mod_results_table.R

#' results_table UI Function
#'
#' @description A shiny Module for displaying evaluation metrics in a table.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom DT DTOutput
mod_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Model Performance Metrics"), # Heading for the table section
    DT::DTOutput(ns("metricsTable"))
  )
}

#' results_table Server Functions
#'
#' @description Server logic for the results table module. Renders a DT datatable
#'   displaying calculated model evaluation metrics (MAE, RMSE, MAPE).
#'
#' @param id Internal parameter for {shiny}.
#' @param reactive_metrics_summary A reactive expression returning a tibble/dataframe
#'   containing the calculated metrics. Expected columns should ideally include
#'   identifiers for Model, Data Set (Train/Test), Metric Name (.metric), and
#'   Value (.estimate). Example format after potential processing in main server:
#'   Model | DataSet | .metric | .estimate
#'   ------|---------|---------|----------
#'   ARIMA | Train   | mae     | ...
#'   ARIMA | Test    | mae     | ...
#'   ...
#'
#' @noRd
#'
#' @import shiny
#' @import DT
#' @import dplyr
#' @import tidyr
mod_results_table_server <- function(id, reactive_metrics_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive expression for the formatted metrics data frame
    reactive_metrics_df <- reactive({
      metrics_data <- reactive_metrics_summary()

      # Require metrics_data to be non-NULL and have rows before proceeding
      req(metrics_data)
      validate(
        need(is.data.frame(metrics_data) && nrow(metrics_data) > 0,
             "Metrics are not available yet. Run a forecast.")
        # Can add more specific column checks if needed
      )

      # Prepare table for display: Pivot metrics into columns
      metrics_formatted <- tryCatch({
        metrics_data %>%
          # Ensure standard metric names if needed (yardstick uses .metric, .estimate)
          # Ensure DataSet and Model columns exist from upstream processing
          dplyr::select(Model, DataSet, Metric = .metric, Value = .estimate) %>%
          tidyr::pivot_wider(
            names_from = Metric,
            values_from = Value
          ) %>%
          # Arrange columns (optional, but good for consistency)
          dplyr::select(Model, DataSet, any_of(c("mae", "rmse", "mape"))) %>%
          # Rename for better display
          dplyr::rename(
            MAE = mae,
            RMSE = rmse,
            MAPE = mape
          )
      }, error = function(e){
        shiny::showNotification("Error formatting metrics table.", type = "warning")
        # Return an empty placeholder or the original data to show something
        return(tibble::tibble(Status = "Error formatting data"))
      })
      return(metrics_formatted)
    })

    output$metricsTable <- DT::renderDT({
      # Render the DataTable using the reactive data frame
      metrics_to_display <- reactive_metrics_df()
      req(metrics_to_display) # Ensure it's available

      DT::datatable(
        metrics_to_display,
        rownames = FALSE,
        filter = 'top',
        options = list(
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          ordering = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        )
      ) %>%
      {
        dt <- .
        # Use names(metrics_to_display) for checking column existence
        if ("MAE" %in% names(metrics_to_display)) dt <- DT::formatRound(dt, columns = "MAE", digits = 3)
        if ("RMSE" %in% names(metrics_to_display)) dt <- DT::formatRound(dt, columns = "RMSE", digits = 3)
        if ("MAPE" %in% names(metrics_to_display)) dt <- DT::formatRound(dt, columns = "MAPE", digits = 2)
        dt
      }
    }, server = FALSE)

    # Return the reactive metrics data frame from the module
    return(reactive_metrics_df)
  })
}
