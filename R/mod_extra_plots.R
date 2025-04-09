# R/mod_extra_plots.R

#' extra_plots UI Function
#' @description Module for displaying additional forecast visualizations.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny plotly
mod_extra_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Additional Plots"),
    tabsetPanel(
      tabPanel("Cumulative Forecast", plotly::plotlyOutput(ns("cumulativePlot"))),
      tabPanel("Forecast by Year", plotly::plotlyOutput(ns("yearlyPlot")))
    )
  )
}

#' extra_plots Server Function
#' @description Server logic for additional plots module.
#' @param id Internal parameter for {shiny}.
#' @param reactive_train_df Reactive training data (`ds`, `y`).
#' @param reactive_test_df Reactive test data (`ds`, `y`).
#' @param reactive_forecast_df Reactive forecast data (`ds`, `yhat`).
#' @noRd
#' @import shiny plotly dplyr lubridate tidyr
mod_extra_plots_server <- function(id, reactive_train_df, reactive_test_df, reactive_forecast_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive for combined historical + forecast data
    combined_data <- reactive({
      req(reactive_train_df())
      # Forecast is required for these plots
      req(reactive_forecast_df())

      train_df <- reactive_train_df() %>% select(ds, y) %>% mutate(Set = "Train")
      test_df <- reactive_test_df() %>% select(ds, y) %>% mutate(Set = "Test")
      forecast_df <- reactive_forecast_df() %>% select(ds, yhat) %>% mutate(Set = "Forecast")

      # Combine history
      history_df <- dplyr::bind_rows(train_df, test_df)

      # Full join ensures all dates are kept, coalesce merges actuals and forecast
      dplyr::full_join(history_df, forecast_df, by = "ds") %>%
        # Use actual 'y' if available, otherwise use forecast 'yhat'
        # Note: This assumes forecast_df *includes* historical fitted values if available
        # If forecast_df is only future, use bind_rows and filter forecast part
        dplyr::mutate(
          Value = dplyr::coalesce(y, yhat),
          Set = dplyr::coalesce(Set.x, Set.y) # Identify source
        ) %>%
        dplyr::select(ds, Value, Set) %>%
        dplyr::filter(!is.na(Value)) %>% # Remove rows where neither actual nor forecast exists
        dplyr::arrange(ds)
    })

    # --- Cumulative Plot ---
    output$cumulativePlot <- plotly::renderPlotly({
      plot_data <- combined_data()
      req(nrow(plot_data) > 0)

      plot_data <- plot_data %>%
        dplyr::mutate(Cumulative_Value = cumsum(Value))

      plot_ly(plot_data, x = ~ds, y = ~Cumulative_Value, color = ~Set, type = 'scatter', mode = 'lines') %>%
        layout(title = "Cumulative Sum (Actuals + Forecast)",
               yaxis = list(title = "Cumulative Value"),
               xaxis = list(title = "Date"),
               hovermode = "x unified")
    })

    # --- Yearly Plot ---
    output$yearlyPlot <- plotly::renderPlotly({
      # Use only forecast data for yearly summary of predictions
      forecast_data <- reactive_forecast_df()
      req(forecast_data, nrow(forecast_data) > 0)

      yearly_summary <- forecast_data %>%
        dplyr::filter(ds >= min(reactive_test_df()$ds)) %>% # Filter to test + future forecast
        dplyr::mutate(Year = factor(lubridate::year(ds))) %>% # Extract Year as factor
        dplyr::group_by(Year) %>%
        dplyr::summarise(Value = sum(yhat, na.rm = TRUE), .groups = 'drop') # Sum forecast by year

      req(nrow(yearly_summary) > 0)

      plot_ly(yearly_summary, x = ~Year, y = ~Value, type = 'bar',
              marker = list(color = '#1f77b4')) %>% # Use a consistent color
        layout(title = "Forecast Sum by Year (Test + Future Periods)",
               yaxis = list(title = "Sum of Forecasted Values"),
               xaxis = list(title = "Year", type = 'category')) # Ensure Year is treated as category
    })

  })
}
