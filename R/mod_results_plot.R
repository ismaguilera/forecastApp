# R/mod_results_plot.R

#' results_plot UI Function
#'
#' @description A shiny Module for displaying forecast results visually.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
mod_results_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Forecast Plot"), # Heading for the plot section
    plotly::plotlyOutput(ns("forecastPlot"), height = "500px") # Set a reasonable height
  )
}

#' results_plot Server Functions
#'
#' @description Server logic for the results plot module. Renders a plotly
#'   graph comparing actual data (train/test) with model forecasts.
#'
#' @param id Internal parameter for {shiny}.
#' @param reactive_forecast_list A reactive expression returning a list of
#'   forecast dataframes, named by model (e.g., list(ARIMA = df1, Prophet = df2)).
#' @param reactive_train_df A reactive expression returning the training dataframe (`ds`, `y`).
#' @param reactive_test_df A reactive expression returning the test dataframe (`ds`, `y`).
# @param reactive_forecast_df A reactive expression returning a forecast dataframe.
#   Expected columns: `ds`, `yhat`. Optional columns for confidence intervals:
#   `yhat_lower_95`, `yhat_upper_95` (or similar, e.g., `yhat_lower`, `yhat_upper`).
#'
#' @noRd
#'
#' @import shiny
#' @import plotly
#' @import dplyr
#' @import tibble
#' @importFrom rlang %||%
mod_results_plot_server <- function(id, reactive_train_df, reactive_test_df, reactive_forecast_list, reactive_global_holidays_data = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive expression to store the plot object
    reactive_plot_object <- reactive({
      # Require the essential data components
      # req(reactive_train_df())
      # req(reactive_test_df())
      # Forecast is only required if it's meant to be plotted (i.e., after model run)
      # We might display only historical data initially if forecast is NULL
      train_data <- reactive_train_df()
      test_data <- reactive_test_df()
      # forecast_data <- reactive_forecast_df() # This might be NULL initially
      forecast_list <- reactive_forecast_list() # Get the list
      holidays_data <- reactive_global_holidays_data() # Get the holidays data

      # Use req on train_data, allow empty test_data and forecast_list initially
      req(train_data)
      validate(need(nrow(train_data) > 0, "..."))

      # req(train_data, forecast_data) # Require forecast data now too for this test

      # Define a color palette (example)
      # Need enough colors for max number of models, or use a generator
      model_colors <- RColorBrewer::brewer.pal(max(3, length(forecast_list)), "Dark2") # Example palette
      names(model_colors) <- names(forecast_list)

      # Base plot
      p <- plot_ly() %>%
        layout(
          title = "Forecast vs Actuals",
          yaxis = list(title = "Value"),
          xaxis = list(title = "Date", rangeslider = list(visible=TRUE)),
          legend = #list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
          list(tracegroupgap = 10, #orientation = "h",
               title=list(text='<b> Models </b>'),
               xanchor = "center"#, x = 0.1, y = 0.9
               ),
          hovermode = "x unified"
        )

      # Add Training Data Trace
      p <- p %>% add_trace(data = train_data, x = ~ds, y = ~y,
                           type = 'scatter', mode = 'lines', line = list(color = 'black'), name = 'Actual (Train)')

      # Add Test Data Trace (if it exists)
      if (nrow(test_data) > 0) {
        p <- p %>% add_trace(data = test_data, x = ~ds, y = ~y, type = 'scatter', mode = 'lines',
                             line = list(color = 'grey', dash = 'dash'), name = 'Actual (Test)')
      }

      # --- Loop Through Forecast List ---
      if (length(forecast_list) > 0) {
        message(paste("Plotting forecasts for:", paste(names(forecast_list), collapse=", ")))
        for (model_name in names(forecast_list)) {
          forecast_data <- forecast_list[[model_name]]
          model_color <- model_colors[[model_name]] %||% "#808080" # Fallback color

          if (!is.null(forecast_data) && nrow(forecast_data) > 0 && "ds" %in% names(forecast_data)) {

            # Add Forecast Line - Use model_name for trace name
            if("yhat" %in% names(forecast_data) && is.numeric(forecast_data$yhat) && !all(is.na(forecast_data$yhat))){
              p <- p %>% add_trace(data = forecast_data, x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
                                   line = list(color = model_color), # Assign color
                                   name = model_name, # Use model name for legend
                                   legendgroup = model_name # Group line & CI
              )
            }

            # Add CI Lines (Workaround) - Use model_name for grouping
            has_ci_95 <- all(c("yhat_lower_95", "yhat_upper_95") %in% names(forecast_data))
            ci_data_valid <- FALSE
            if(has_ci_95) { # Check validity as before
              lower_ok <- is.numeric(forecast_data$yhat_lower_95) && !all(is.na(forecast_data$yhat_lower_95))
              upper_ok <- is.numeric(forecast_data$yhat_upper_95) && !all(is.na(forecast_data$yhat_upper_95))
              if(lower_ok && upper_ok) ci_data_valid <- TRUE
            }

            if(ci_data_valid) {
              p <- p %>% add_lines(data = forecast_data, x = ~ds, y = ~yhat_lower_95,
                                   line = list(color = model_color, width = 1, dash = 'dash'), # Match color, dashed
                                   legendgroup = model_name, # Group with forecast line
                                   showlegend = FALSE, name = paste(model_name, "Lower CI"), hoverinfo = 'skip' )
              p <- p %>% add_lines(data = forecast_data, x = ~ds, y = ~yhat_upper_95,
                                   line = list(color = model_color, width = 1, dash = 'dash'),
                                   legendgroup = model_name,
                                   showlegend = FALSE, name = paste(model_name, "Upper CI"), hoverinfo = 'skip' )
            }
          } else {
            warning(paste("Forecast data for model", model_name, "is invalid or empty."))
          }
        } # End for loop
      } # End if forecast_list has items
      # --- End Loop ---

      # Final layout adjustments (legend below)
      p <- p %>% layout(legend = list(tracegroupgap = 10, #orientation = "h",
                                      title=list(text='<b> Models </b>'),
                                      xanchor = "center"
                                      #, x = 0.1, y = 0.9
                                      )) # Ensure legend is positioned
      
      # --- Add Global Holidays Display ---
      if (!is.null(holidays_data) && nrow(holidays_data) > 0 &&
          all(c("ds", "holiday") %in% names(holidays_data))) {
        
        processed_holidays <- holidays_data %>%
          dplyr::mutate(ds = as.Date(ds)) %>%
          dplyr::distinct(ds, .keep_all = TRUE) # Ensure unique dates for lines/markers
        
        current_plot_data_dates <- c()
        if (!is.null(train_data) && nrow(train_data) > 0) current_plot_data_dates <- c(current_plot_data_dates, train_data$ds)
        if (!is.null(test_data) && nrow(test_data) > 0) current_plot_data_dates <- c(current_plot_data_dates, test_data$ds)
        if (length(forecast_list) > 0) {
          for(fcst_df_item in forecast_list) {
            if (!is.null(fcst_df_item) && nrow(fcst_df_item) > 0 && "ds" %in% names(fcst_df_item)) {
              current_plot_data_dates <- c(current_plot_data_dates, fcst_df_item$ds)
            }
          }
        }
        current_plot_data_dates <- unique(as.Date(current_plot_data_dates))
        
        holidays_in_range <- processed_holidays
        if (length(current_plot_data_dates) > 0) {
          min_date <- min(current_plot_data_dates, na.rm = TRUE)
          max_date <- max(current_plot_data_dates, na.rm = TRUE)
          if (!is.na(min_date) && !is.na(max_date)) {
            holidays_in_range <- processed_holidays %>%
              dplyr::filter(ds >= min_date & ds <= max_date)
          }
        }
        
        if (nrow(holidays_in_range) > 0) {
          shapes_list <- list()
          for (i in 1:nrow(holidays_in_range)) {
            h_date <- holidays_in_range$ds[i]
            shapes_list[[i]] <- list(
              type = "line",
              x0 = h_date, x1 = h_date,
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = "rgba(180, 180, 180, 0.6)", width = 1, dash = "dashdot")
            )
          }
          p <- p %>% layout(shapes = shapes_list)
          
          p <- p %>% add_trace(
            data = holidays_in_range,
            x = ~ds,
            y = 1, # Position at the top of the plot
            yref = "paper", # Relative to the plot paper
            type = 'scatter', 
            mode = 'markers',
            marker = list(opacity = 0.001, size = 10), # Effectively invisible but hoverable
            text = ~paste(holiday, "<br>", ds), # Hover text: holiday name and date
            hoverinfo = "text",
            name = "Holidays", # Name for the legend
            showlegend = TRUE
          )
        }
      }
      # --- End Global Holidays Display ---

      p # Return the plotly object
    })

    # Render the plot to the UI output
    output$forecastPlot <- plotly::renderPlotly({
      reactive_plot_object()
    })

    # # Return the reactive plot object for potential use elsewhere (e.g., download)
    # return(reactive_plot_object)
  })
}
