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
mod_results_plot_server <- function(id, reactive_train_df, reactive_test_df, reactive_forecast_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$forecastPlot <- plotly::renderPlotly({

      # Require the essential data components
      # req(reactive_train_df())
      # req(reactive_test_df())
      # Forecast is only required if it's meant to be plotted (i.e., after model run)
      # We might display only historical data initially if forecast is NULL
      train_data <- reactive_train_df()
      test_data <- reactive_test_df()
      # forecast_data <- reactive_forecast_df() # This might be NULL initially
      forecast_list <- reactive_forecast_list() # Get the list

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
      # p <- p %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1), ...)
      p # Return the plot object
      #
      #
      #
      #
      #
      # # validate(
      # #   need(is.data.frame(train_data) && nrow(train_data) > 0 && all(c("ds", "y") %in% names(train_data)),
      # #        "Valid training data is required."),
      # #   need(is.data.frame(test_data) && all(c("ds", "y") %in% names(test_data)), # Test data can be empty
      # #        "Test data input seems invalid.")
      # #   # We don't validate forecast_data here, as it might legitimately be NULL before forecasting
      # # )
      # # validate(need(!is.null(forecast_data) || nrow(test_data)>0, "No forecast or test data to plot after training data."))
      # # p <- plot_ly() # Start base plot
      # #
      # # # Base plot
      # # p <- plot_ly() %>%
      # #   layout(
      # #     title = "Forecast vs Actuals",
      # #     yaxis = list(title = "Value"),
      # #     xaxis = list(title = "Date", rangeslider = list(visible=TRUE)),
      # #     legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
      # #     hovermode = "x unified"
      # #   )
      # #
      # #
      # # # Add Training Data Trace
      # # p <- p %>% add_trace(data = train_data, x = ~ds, y = ~y,
      # #                      type = 'scatter', mode = 'lines',
      # #                      line = list(color = 'black'),
      # #                      name = 'Actual (Train)')
      # #
      # # # Add Test Data Trace (if it exists)
      # # if (nrow(test_data) > 0) {
      # #   p <- p %>% add_trace(data = test_data, x = ~ds, y = ~y,
      # #                        type = 'scatter', mode = 'lines',
      # #                        line = list(color = 'grey', dash = 'dash'),
      # #                        name = 'Actual (Test)')
      # # }
      # #
      # # # Process Forecast Data (Line + CI Lines)
      # # if (!is.null(forecast_data) && nrow(forecast_data) > 0 ) {
      # #
      # #   # Add Forecast Line (yhat)
      # #   if(is.numeric(forecast_data$yhat) && !all(is.na(forecast_data$yhat))){
      # #     p <- p %>% add_trace(data = forecast_data, x = ~ds, y = ~yhat,
      # #                          type = 'scatter', mode = 'lines',
      # #                          line = list(color = '#1f77b4'), # Plotly blue
      # #                          name = 'Forecast', # Main name for legend
      # #                          legendgroup = 'forecast_group' # Assign a legend group
      # #     )
      # #     message("Plotting: Added Forecast line trace.")
      # #   } else { warning("Forecast yhat invalid.") }
      # #
      # #
      # #   # --- WORKAROUND: Add CI using add_lines ---
      # #   has_ci_95 <- all(c("yhat_lower_95", "yhat_upper_95") %in% names(forecast_data))
      # #   ci_data_valid <- FALSE
      # #   if (has_ci_95) {
      # #     lower_ok <- is.numeric(forecast_data$yhat_lower_95) && !all(is.na(forecast_data$yhat_lower_95))
      # #     upper_ok <- is.numeric(forecast_data$yhat_upper_95) && !all(is.na(forecast_data$yhat_upper_95))
      # #     if (lower_ok && upper_ok) { ci_data_valid <- TRUE; message("Plotting: Valid CI data found for lines.") }
      # #     else { message("Plotting: CI columns non-numeric/all NAs. Skipping CI lines.") }
      # #   } else { message("Plotting: CI columns not found. Skipping CI lines.") }
      # #
      # #   if(ci_data_valid) {
      # #     message("Plotting: Attempting to add CI lines...")
      # #     # Add Lower Bound Line
      # #     p <- p %>% add_lines(data = forecast_data, x = ~ds, y = ~yhat_lower_95,
      # #                          line = list(color = '#1f77b4', width = 1, dash = 'dash'), # Dashed, same color family or grey
      # #                          legendgroup = 'forecast_group', # Same group as forecast line
      # #                          showlegend = FALSE, # Hide from legend
      # #                          name = 'Lower 95% CI' # Name appears on hover
      # #                          # hoverinfo = 'skip' # Optional: disable hover
      # #     )
      # #     # Add Upper Bound Line
      # #     p <- p %>% add_lines(data = forecast_data, x = ~ds, y = ~yhat_upper_95,
      # #                          line = list(color = '#1f77b4', width = 1, dash = 'dash'),
      # #                          legendgroup = 'forecast_group',
      # #                          showlegend = FALSE,
      # #                          name = 'Upper 95% CI'
      # #                          # hoverinfo = 'skip'
      # #     )
      # #     message("Plotting: add_lines for CI completed.")
      # #   }
      # #   # --- END WORKAROUND ---
      # #
      # # } # End if forecast_data exists
      #
      # # Add Forecast Trace (IF forecast_data exists and is valid)
      # # if (!is.null(forecast_data) && nrow(forecast_data) > 0 ) {
      # #   if(is.numeric(forecast_data$yhat) && !all(is.na(forecast_data$yhat))){
      # #     p <- p %>% add_trace(data = forecast_data, x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
      # #                          line = list(color = '#1f77b4'), name = 'Forecast')
      # #     message("Plotting: Added Forecast line trace.")
      # #   } else {
      # #     warning("Forecast yhat column is non-numeric or all NA.")
      # #   }
      #
      #   # --- TEMPORARILY COMMENT OUT RIBBON LOGIC ---
      #   # has_ci_95 <- all(c("yhat_lower_95", "yhat_upper_95") %in% names(forecast_data))
      #   # ci_data_valid <- FALSE
      #   # if (has_ci_95) { ... check NAs ... }
      #   # message(...) # Keep message check if you want
      #   # if(ci_data_valid) {
      #   #     # p <- p %>% add_ribbons(...) # COMMENTED OUT
      #   # }
      #
      #   # has_ci_95 <- all(c("yhat_lower_95", "yhat_upper_95") %in% names(forecast_data))
      #   # ci_data_valid <- FALSE
      #   # if (has_ci_95) {
      #   #   lower_ok <- is.numeric(forecast_data$yhat_lower_95) && !all(is.na(forecast_data$yhat_lower_95))
      #   #   upper_ok <- is.numeric(forecast_data$yhat_upper_95) && !all(is.na(forecast_data$yhat_upper_95))
      #   #   if (lower_ok && upper_ok) { ci_data_valid <- TRUE; message("Plotting: Valid CI data found for ribbons.") }
      #   #   else { message("Plotting: CI columns exist but non-numeric/all NAs. Skipping ribbons.") }
      #   # } else { message("Plotting: CI columns not found. Skipping ribbons.") }
      #   #
      #   #
      #   # # --- END COMMENT OUT ---
      #   # # Add Ribbons if valid
      #   # if(ci_data_valid) {
      #   #   message("Plotting: Attempting to add ribbons...")
      #   #   p <- p %>% add_ribbons(data = forecast_data, x = ~ds, name = '95% Interval', # name here links ribbon to legend
      #   #                          ymin = ~yhat_lower_95, ymax = ~yhat_upper_95,
      #   #                          line = list(color = 'transparent'),
      #   #                          # Match forecast line color but lighter alpha
      #   #                          fillcolor = 'rgba(31, 119, 180, 0.2)',
      #   #                          hoverinfo = 'skip',
      #   #                          legendgroup = 'forecast', # Group ribbon with line maybe?
      #   #                          showlegend = FALSE # Don't show separate legend item for ribbon fill
      #   #   )
      #   #   message("Plotting: add_ribbons call completed.")
      #   # }
      #
      # # Add Forecast Trace(s) and Confidence Intervals (if forecast_data exists)
      # # if (!is.null(forecast_data) && is.data.frame(forecast_data) && nrow(forecast_data) > 0) {
      # #
      # #   validate(
      # #     need(all(c("ds", "yhat") %in% names(forecast_data)),
      # #          "Forecast data must contain 'ds' and 'yhat' columns.")
      # #   )
      # #
      # #   # Check for standard 95% CI column names (adapt if needed for different models)
      # #   has_ci_95 <- all(c("yhat_lower_95", "yhat_upper_95") %in% names(forecast_data))
      # #   # --- ADD CHECK for valid numeric CI data ---
      # #   ci_data_valid <- FALSE
      # #   if (has_ci_95) {
      # #     # Check if BOTH lower and upper bounds are numeric and have at least one non-NA value
      # #     lower_ok <- is.numeric(forecast_data$yhat_lower_95) && !all(is.na(forecast_data$yhat_lower_95))
      # #     upper_ok <- is.numeric(forecast_data$yhat_upper_95) && !all(is.na(forecast_data$yhat_upper_95))
      # #     if (lower_ok && upper_ok) {
      # #       ci_data_valid <- TRUE
      # #       message("Plotting: Valid CI data found for ribbons.")
      # #     } else {
      # #       message("Plotting: CI columns exist but are non-numeric or contain all NAs. Skipping ribbons.")
      # #     }
      # #   } else {
      # #     message("Plotting: CI columns not found. Skipping ribbons.")
      # #   }
      # #   # --- END CHECK ---
      # #
      # #   # Add Ribbons only if columns exist AND data is valid numeric with non-NAs
      # #   if(ci_data_valid) {
      # #     p <- p %>% add_ribbons(data = forecast_data, x = ~ds, name = '95% Interval', # Ensure name set
      # #                            ymin = ~yhat_lower_95, ymax = ~yhat_upper_95,
      # #                            line = list(color = 'transparent'), fillcolor = 'rgba(31, 119, 180, 0.2)', # Use same family blue
      # #                            hoverinfo = 'skip' # Don't show ribbon hover usually
      # #     ) %>%
      # #       layout(showlegend = TRUE) # Ensure legend shows maybe? Or control manually.
      # #   } # We removed the else if for prophet CIs, restore if needed or integrate check
      # #
      # #   # Check for prophet default CI names
      # #   has_ci_prophet <- all(c("yhat_lower", "yhat_upper") %in% names(forecast_data))
      # #
      # #   # Add Ribbons first (if available) - Plotly adds layers sequentially
      # #   # if(has_ci_95) {
      # #   #   p <- p %>% add_ribbons(data = forecast_data, x = ~ds,
      # #   #                          ymin = ~yhat_lower_95, ymax = ~yhat_upper_95,
      # #   #                          line = list(color = 'transparent'), # No border line for ribbon
      # #   #                          fillcolor = 'rgba(0, 100, 255, 0.2)', # Light blue fill
      # #   #                          name = '95% Confidence Interval')
      # #   # } else if (has_ci_prophet) {
      # #   if (has_ci_prophet) {
      # #     # Handle Prophet's default names if 95% names aren't present
      # #     p <- p %>% add_ribbons(data = forecast_data, x = ~ds,
      # #                            ymin = ~yhat_lower, ymax = ~yhat_upper,
      # #                            line = list(color = 'transparent'),
      # #                            fillcolor = 'rgba(0, 100, 255, 0.2)',
      # #                            name = 'Confidence Interval') # Generic name if level unknown
      # #   }
      # #   # Add Forecast Line (ensure yhat is valid too)
      # #   if(is.numeric(forecast_data$yhat) && !all(is.na(forecast_data$yhat))){
      # #     p <- p %>% add_trace(data = forecast_data, x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
      # #                          line = list(color = '#1f77b4'), name = 'Forecast') # Match ribbon family blue
      # #   } else {
      # #     warning("Forecast yhat column is non-numeric or all NA. Cannot plot forecast line.")
      # #   }
      # #
      # #
      # #   # Add Forecast Line (yhat)
      # #   # p <- p %>% add_trace(data = forecast_data, x = ~ds, y = ~yhat,
      # #   #                      type = 'scatter', mode = 'lines',
      # #   #                      line = list(color = '#1f77b4'), # Plotly blue
      # #   #                      name = 'Forecast')
      # #
      # #
      # # } else {
      # #   # Maybe add a note if forecast isn't ready? Or just show historical.
      # #   # p <- p %>% layout(annotations = list(list(text = "Run forecast to see predictions", showarrow=FALSE)))
      # #
      # # }
      # # Final layout adjustments
      # # p <- p %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) # Ensure legend is positioned
      # # Final layout adjustments
      # # } # End if forecast_data exists
      #
      # p <- p %>% layout(
      #   title = "Forecast vs Actuals",
      #   xaxis = list(title="Date", rangeslider = list(visible=TRUE)),
      #   yaxis = list(title = "Value"),
      #   legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
      #   hovermode = "x unified"
      # )
      #
      # p # Return the plot object
    })

  })
}
