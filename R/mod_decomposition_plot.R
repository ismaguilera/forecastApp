# R/mod_decomposition_plot.R

#' decomposition_plot UI Function
#' @description Module UI for displaying time series decomposition plot.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny plotly
mod_decomposition_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Maybe add options later (e.g., choose decomposition method?)
    plotly::plotlyOutput(ns("decompPlot"), height = "500px")
  )
}

#' decomposition_plot Server Function
#' @description Module Server for STL decomposition plot.
#' @param id Internal parameter for {shiny}.
#' @param reactive_aggregated_df Reactive containing the aggregated data before train/test split (needs 'ds', 'y').
#' @param reactive_aggregation_level Reactive returning aggregation level ('Daily', 'Weekly').
#' @noRd
#' @import shiny plotly dplyr tidyr lubridate stats
mod_decomposition_plot_server <- function(id, reactive_aggregated_df, reactive_aggregation_level){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$decompPlot <- plotly::renderPlotly({

      agg_data <- reactive_aggregated_df()
      agg_level <- reactive_aggregation_level()

      req(agg_data, agg_level) # Ensure inputs are available
      validate(
        need(nrow(agg_data) > 1, "Need at least 2 data points for decomposition plot."),
        need(all(c("ds", "y") %in% names(agg_data)), "Aggregated data needs 'ds' and 'y' columns.")
      )

      # Determine frequency
      freq <- if (agg_level == "Daily") 7 else if (agg_level == "Weekly") 52 else 1

      # Check if enough data for STL
      validate(
        need(freq > 1, "Decomposition requires seasonal data (frequency > 1)."),
        need(nrow(agg_data) >= 2 * freq, paste0("Need at least 2 full seasonal cycles (", 2*freq, " points) for STL decomposition."))
      )

      message("Attempting STL decomposition for plot...")
      stl_result <- NULL
      decomp_data <- NULL
      tryCatch({
        # Create ts object from aggregated data
        # Ensure data is sorted by date first
        agg_data_sorted <- agg_data %>% arrange(ds)
        start_date <- min(agg_data_sorted$ds)
        start_year <- lubridate::year(start_date)
        ts_start <- if (freq > 1) { # Logic copied from train_arima
          day_in_cycle <- switch(as.character(freq),
                                 "7" = lubridate::wday(start_date, week_start = 1),
                                 "52" = lubridate::week(start_date),
                                 floor(lubridate::yday(start_date) / (365.25 / freq)) + 1 )
          c(start_year, day_in_cycle)
        } else { start_year }
        y_ts <- stats::ts(agg_data_sorted$y, frequency = freq, start = ts_start)

        # Perform STL decomposition
        stl_result <- stats::stl(y_ts, s.window = "periodic", robust = TRUE)

        # Extract components
        decomp_data <- tibble::tibble(
          ds = agg_data_sorted$ds, # Use dates from sorted data
          Data = agg_data_sorted$y,
          Seasonal = as.numeric(stl_result$time.series[,"seasonal"]),
          Trend = as.numeric(stl_result$time.series[,"trend"]),
          Remainder = as.numeric(stl_result$time.series[,"remainder"])
        ) %>%
          tidyr::pivot_longer(cols = c("Data", "Seasonal", "Trend", "Remainder"),
                              names_to = "Component", values_to = "Value") %>%
          dplyr::mutate(Component = factor(Component, levels=c("Data", "Seasonal", "Trend", "Remainder")))

      }, error = function(e) {
        warning("STL decomposition failed for plot: ", conditionMessage(e))
        # Return NULL or show error plot? Let's return NULL, req() below will handle it.
        return(NULL)
      }) # End tryCatch

      # Require successful decomposition
      req(decomp_data)
      message("STL successful for plot. Creating decomposition subplots.")

      # Create individual plots for each component trace
      p_data <- plot_ly(data = decomp_data %>% dplyr::filter(Component=="Data"),
                        x = ~ds, y = ~Value, type='scatter', mode='lines', name="Data",
                        line=list(color='grey')) %>%
        layout(yaxis = list(title="Data"))

      p_seasonal <- plot_ly(data = decomp_data %>% dplyr::filter(Component=="Seasonal"),
                            x = ~ds, y = ~Value, type='scatter', mode='lines', name="Seasonal",
                            line=list(color='#ff7f0e')) %>% # Orange
        layout(yaxis = list(title="Seasonal"))

      p_trend <- plot_ly(data = decomp_data %>% dplyr::filter(Component=="Trend"),
                         x = ~ds, y = ~Value, type='scatter', mode='lines', name="Trend",
                         line=list(color='#2ca02c')) %>% # Green
        layout(yaxis = list(title="Trend"))

      p_remainder <- plot_ly(data = decomp_data %>% dplyr::filter(Component=="Remainder"),
                             x = ~ds, y = ~Value, type='scatter', mode='lines', name="Remainder",
                             line=list(color='#d62728')) %>% # Red
        layout(yaxis = list(title="Remainder"), xaxis = list(title="Date")) # Add x title only to bottom plot

      # Combine decomposition plots
      subplot(p_data, p_seasonal, p_trend, p_remainder,
              nrows = 4, shareX = TRUE, titleY = TRUE,
              heights = c(0.4, 0.2, 0.2, 0.2)) %>% # Adjust relative heights
        layout(title = "Time Series Decomposition (STL)",
               showlegend = FALSE) # Hide individual legends

    }) # End renderPlotly

  }) # End moduleServer
} # End server function
