# R/mod_extra_plots.R

#' extra_plots UI Function
#' @description Module for displaying additional forecast visualizations.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny plotly ggplot2 forecast bslib
mod_extra_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Additional Plots"),
    # Remove selector from here
    # uiOutput(ns("diagnosticModelSelectorUI")),
    # hr(style="margin-top: 5px; margin-bottom: 10px;"),
    bslib::accordion(
      # Accordion item for Summary Plots with Tabs inside
      bslib::accordion_panel(
        title = "Summary Plots",
        icon = shiny::icon("chart-line"),
        tabsetPanel(
          tabPanel("Cumulative Forecast", plotly::plotlyOutput(ns("cumulativePlot"))),
          tabPanel("Forecast by Year", plotly::plotlyOutput(ns("yearlyPlot")))
        )
      ),
      # Accordion item for Diagnostic Plots with Selector and Tabs inside
      bslib::accordion_panel(
        title = "Diagnostic Plots",
        icon = shiny::icon("stethoscope"),
        # Move selector inside this panel
        uiOutput(ns("diagnosticModelSelectorUI")),
        hr(style="margin-top: 5px; margin-bottom: 10px;"),
        tabsetPanel(
          tabPanel("Residuals vs Fitted", plotly::plotlyOutput(ns("residualsVsFittedPlot"))),
          tabPanel("Residual ACF", plotly::plotlyOutput(ns("residualAcfPlot"))),
          tabPanel("Residual PACF", plotly::plotlyOutput(ns("residualPacfPlot")))
        )
      )
    )
  )
}

#' extra_plots Server Function
#' @description Server logic for additional plots module.
#' @param id Internal parameter for {shiny}.
#' @param reactive_train_df Reactive training data (`ds`, `y`).
#' @param reactive_test_df Reactive test data (`ds`, `y`).
#' @param reactive_forecast_list A reactive returning a named list of forecast dataframes.
#' @param reactive_fitted_list A reactive returning a named list of fitted value vectors.
#' @noRd
#' @import shiny plotly dplyr lubridate tidyr purrr RColorBrewer ggplot2 forecast
#' @importFrom rlang %||%
#' @importFrom stats residuals
mod_extra_plots_server <- function(id, reactive_train_df, reactive_test_df, reactive_forecast_list, reactive_fitted_list, reactive_selected_summary_model){ # Added reactive_selected_summary_model
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- Model Selector for Diagnostic Plots ---
    output$diagnosticModelSelectorUI <- renderUI({
      fitted_list <- reactive_fitted_list()
      req(fitted_list)
      model_choices <- names(fitted_list)
      validate(need(length(model_choices) > 0, "No fitted values available for diagnostic plots."))

      selectInput(ns("selected_diagnostic_model"),
                  label = "Select Model for Diagnostic Plots:",
                  choices = model_choices,
                  selected = model_choices[1])
    })

    # --- Observer to link Summary selection to Diagnostic selection ---
    observeEvent(reactive_selected_summary_model(), {
      selected_summary <- reactive_selected_summary_model()
      # Check if the selected model from summary exists in the choices for diagnostics
      # (It should, as both are based on successful runs, but good practice to check)
      fitted_list <- reactive_fitted_list()
      req(fitted_list)
      if (!is.null(selected_summary) && selected_summary %in% names(fitted_list)) {
        updateSelectInput(session, "selected_diagnostic_model", selected = selected_summary)
      }
    })

    # --- Reactive for Residual Calculation ---
    reactive_residuals_data <- reactive({
      selected_model <- input$selected_diagnostic_model
      train_df <- reactive_train_df()
      fitted_list <- reactive_fitted_list()

      req(selected_model, train_df, fitted_list)
      validate(need(selected_model %in% names(fitted_list), "Selected model not found in fitted values list."))

      fitted_values <- fitted_list[[selected_model]]
      actual_values <- train_df$y

      # Ensure lengths match
      req(length(fitted_values) == length(actual_values))

      # Calculate residuals
      residuals_vec <- actual_values - fitted_values

      # Return a tibble for plotting
      tibble::tibble(
        Fitted = fitted_values,
        Residuals = residuals_vec,
        Time = seq_along(residuals_vec) # Simple index for ACF/PACF plots if needed
        # Could also add train_df$ds if needed, but ensure alignment
      )
    })

    # --- Residuals vs Fitted Plot ---
    output$residualsVsFittedPlot <- plotly::renderPlotly({
      res_data <- reactive_residuals_data()
      req(res_data)
      selected_model <- input$selected_diagnostic_model # Get selected model name for title

      p <- ggplot(res_data, aes(x = Fitted, y = Residuals)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = paste("Residuals vs Fitted Values -", selected_model),
             x = "Fitted Values",
             y = "Residuals") +
        theme_minimal()

      plotly::ggplotly(p)
    })

    # --- Residual ACF Plot ---
    output$residualAcfPlot <- plotly::renderPlotly({
      res_data <- reactive_residuals_data()
      req(res_data)
      selected_model <- input$selected_diagnostic_model

      # Use forecast::ggAcf for easy plotting
      # Requires residuals as a numeric vector or ts object
      p_acf <- forecast::ggAcf(res_data$Residuals, lag.max = 40) + # Adjust lag.max if needed
        labs(title = paste("ACF of Residuals -", selected_model)) +
        theme_minimal()

      plotly::ggplotly(p_acf)
    })

    # --- Residual PACF Plot ---
    output$residualPacfPlot <- plotly::renderPlotly({
      res_data <- reactive_residuals_data()
      req(res_data)
      selected_model <- input$selected_diagnostic_model

      p_pacf <- forecast::ggPacf(res_data$Residuals, lag.max = 40) +
        labs(title = paste("PACF of Residuals -", selected_model)) +
        theme_minimal()

      plotly::ggplotly(p_pacf)
    })


    # --- Existing Plots (Cumulative and Yearly) ---
    # (Keep the existing logic for these plots below)
    # combined_data <- reactive({
    #   req(reactive_train_df())
    #   # Forecast is required for these plots
    #   req(reactive_forecast_df())
    #
    #   train_df <- reactive_train_df() %>% select(ds, y) %>% mutate(Set = "Train")
    #   test_df <- reactive_test_df() %>% select(ds, y) %>% mutate(Set = "Test")
    #   forecast_df <- reactive_forecast_df() %>% select(ds, yhat) %>% mutate(Set = "Forecast")
    #
    #   # Combine history
    #   history_df <- dplyr::bind_rows(train_df, test_df)
    #
    #   # Full join ensures all dates are kept, coalesce merges actuals and forecast
    #   dplyr::full_join(history_df, forecast_df, by = "ds") %>%
    #     # Use actual 'y' if available, otherwise use forecast 'yhat'
    #     # Note: This assumes forecast_df *includes* historical fitted values if available
    #     # If forecast_df is only future, use bind_rows and filter forecast part
    #     dplyr::mutate(
    #       Value = dplyr::coalesce(y, yhat),
    #       Set = dplyr::coalesce(Set.x, Set.y) # Identify source
    #     ) %>%
    #     dplyr::select(ds, Value, Set) %>%
    #     dplyr::filter(!is.na(Value)) %>% # Remove rows where neither actual nor forecast exists
    #     dplyr::arrange(ds)
    # })
    #
    # # --- Cumulative Plot ---
    # output$cumulativePlot <- plotly::renderPlotly({
    #   plot_data <- combined_data()
    #   req(nrow(plot_data) > 0)
    #
    #   plot_data <- plot_data %>%
    #     dplyr::mutate(Cumulative_Value = cumsum(Value))
    #
    #   plot_ly(plot_data, x = ~ds, y = ~Cumulative_Value, color = ~Set, type = 'scatter', mode = 'lines') %>%
    #     layout(title = "Cumulative Sum (Actuals + Forecast)",
    #            yaxis = list(title = "Cumulative Value"),
    #            xaxis = list(title = "Date"),
    #            hovermode = "x unified")
    # })
    #
    # # --- Yearly Plot ---
    # output$yearlyPlot <- plotly::renderPlotly({
    #   # Use only forecast data for yearly summary of predictions
    #   forecast_data <- reactive_forecast_df()
    #   req(forecast_data, nrow(forecast_data) > 0)
    #
    #   yearly_summary <- forecast_data %>%
    #     dplyr::filter(ds >= min(reactive_test_df()$ds)) %>% # Filter to test + future forecast
    #     dplyr::mutate(Year = factor(lubridate::year(ds))) %>% # Extract Year as factor
    #     dplyr::group_by(Year) %>%
    #     dplyr::summarise(Value = sum(yhat, na.rm = TRUE), .groups = 'drop') # Sum forecast by year
    #
    #   req(nrow(yearly_summary) > 0)
    #
    #   plot_ly(yearly_summary, x = ~Year, y = ~Value, type = 'bar',
    #           marker = list(color = '#1f77b4')) %>% # Use a consistent color
    #     layout(title = "Forecast Sum by Year (Test + Future Periods)",
    #            yaxis = list(title = "Sum of Forecasted Values"),
    #            xaxis = list(title = "Year", type = 'category')) # Ensure Year is treated as category
    # })

    # # --- Cumulative Plot (Using FIRST model in the list) ---
    # output$cumulativePlot <- plotly::renderPlotly({
    #   train_df <- reactive_train_df()
    #   test_df <- reactive_test_df() %||% data.frame(ds=as.Date(character()), y=numeric()) # Ensure test_df is not NULL
    #   forecast_list <- reactive_forecast_list()
    #
    #   # Require data and at least one forecast
    #   req(train_df, forecast_list)
    #   validate(need(length(forecast_list) > 0, "No forecast results available to plot."))
    #
    #   # Select the first forecast from the list
    #   first_model_name <- names(forecast_list)[1]
    #   forecast_data <- forecast_list[[first_model_name]]
    #   req(forecast_data) # Ensure the first forecast df is valid
    #
    #   message(paste("Cumulative Plot using forecast from:", first_model_name))
    #
    #   # Combine history + selected forecast
    #   history_df <- dplyr::bind_rows(
    #     train_df %>% select(ds, y) %>% mutate(Set = "Train"),
    #     test_df %>% select(ds, y) %>% mutate(Set = "Test")
    #   )
    #   combined_plot_data <- dplyr::full_join(history_df,
    #                                          forecast_data %>% select(ds, yhat) %>% mutate(Set = "Forecast"),
    #                                          by = "ds") %>%
    #     dplyr::mutate(
    #       Value = dplyr::coalesce(y, yhat),
    #       Set = dplyr::coalesce(Set.x, Set.y)
    #     ) %>%
    #     dplyr::select(ds, Value, Set) %>%
    #     dplyr::filter(!is.na(Value)) %>%
    #     dplyr::arrange(ds) %>%
    #     dplyr::mutate(Cumulative_Value = cumsum(Value)) # Calculate cumulative sum
    #
    #   validate(need(nrow(combined_plot_data) > 0, "No data available for cumulative plot."))
    #
    #   plot_ly(combined_plot_data, x = ~ds, y = ~Cumulative_Value, color = ~Set,
    #           colors = c("Train" = "black", "Test" = "grey", "Forecast" = "#1f77b4"), # Assign colors
    #           type = 'scatter', mode = 'lines') %>%
    #     layout(title = paste("Cumulative Sum (Actuals + Forecast:", first_model_name, ")"), # Dynamic Title
    #            yaxis = list(title = "Cumulative Value"),
    #            xaxis = list(title = "Date"),
    #            hovermode = "x unified")
    # }) # End cumulativePlot

    # --- Cumulative Plot (Overlaying ALL models) ---
    output$cumulativePlot <- plotly::renderPlotly({
      train_df <- reactive_train_df()
      test_df <- reactive_test_df() %||% tibble::tibble(ds=as.Date(character()), y=numeric()) # Default empty tibble
      forecast_list <- reactive_forecast_list()

      req(train_df, forecast_list)
      validate(
        need(nrow(train_df) > 0, "Training data needed for cumulative plot."),
        need(length(forecast_list) > 0, "No forecast results available for cumulative plot.")
      )
      message("Cumulative Plot: Preparing data for all models.")

      # 1. Prepare Historical Data & Cumsum
      history_df <- dplyr::bind_rows(
        train_df %>% dplyr::select(ds, y),
        test_df %>% dplyr::select(ds, y)
      ) %>%
        # Ensure no NAs in historical y for cumsum
        dplyr::filter(!is.na(y)) %>%
        dplyr::arrange(ds)

      if(nrow(history_df) == 0) validate("No valid historical data points.")

      history_cumulative <- history_df %>%
        dplyr::mutate(Cumulative_Value = cumsum(y),
                      Model = "Actuals", # Assign model name
                      Set = "Actuals") %>% # Assign set type
        dplyr::select(ds, Cumulative_Value, Set, Model)

      # Get last historical values needed for forecast branching
      last_hist_cumulative <- dplyr::last(history_cumulative$Cumulative_Value) %||% 0 # Default to 0 if no history
      last_hist_date <- dplyr::last(history_cumulative$ds)

      # 2. Process Forecasts Cumulatively
      all_forecasts_cumulative <- purrr::map_dfr(
        .x = forecast_list,
        .id = "Model", # Use list names as Model column
        .f = function(forecast_data, model_name) {
          # Basic validation of forecast data for this model
          if(is.null(forecast_data) || nrow(forecast_data) == 0 || !all(c("ds", "yhat") %in% names(forecast_data))) {
            warning("Invalid forecast data structure for model: ", model_name); return(NULL)
          }
          forecast_data %>%
            dplyr::select(ds, yhat) %>%
            dplyr::filter(ds > last_hist_date) %>% # Only dates AFTER history
            dplyr::arrange(ds) %>%
            dplyr::filter(!is.na(yhat)) %>% # Ensure no NAs in forecast values
            dplyr::mutate(
              # Calculate cumulative sum starting from the forecast period
              Incremental_Cumsum = cumsum(yhat),
              # Add the final historical cumulative value as an offset
              Cumulative_Value = last_hist_cumulative + Incremental_Cumsum,
              Set = "Forecast" # Assign set type
            ) %>%
            # Select only needed columns (ds, Cumulative_Value, Set already added)
            dplyr::select(ds, Cumulative_Value, Set)
          # Model column added by map_dfr using .id
        }
      )

      # 3. Combine History and Forecasts for Plotting
      plot_data <- dplyr::bind_rows(history_cumulative, all_forecasts_cumulative) %>%
        dplyr::arrange(Model, ds)

      validate(need(nrow(plot_data) > 0, "No cumulative data available to plot after processing."))

      # 4. Create Plot
      # Define colors - ensure 'Actuals' is distinct
      unique_models <- unique(plot_data$Model)
      forecast_models <- setdiff(unique_models, "Actuals")
      n_forecast_models <- length(forecast_models)

      palette_name <- "Dark2" # Choose a palette qualitative palette
      plot_colors <- RColorBrewer::brewer.pal(max(3, n_forecast_models), palette_name)
      if (n_forecast_models > length(plot_colors)) {
        plot_colors <- rep(plot_colors, length.out = n_forecast_models)
      }
      # Assign colors - specific color for Actuals
      model_color_map <- stats::setNames(plot_colors, forecast_models) # Use stats::setNames
      model_color_map[["Actuals"]] <- "#000000" # Set Actuals to black

      # Define linetypes
      linetype_map <- c("Actuals" = "solid", "Forecast" = "dash")

      plot_ly(plot_data) %>%
        # Add traces per model group
        # Grouping by Model ensures separate lines, then color and linetype applied
        dplyr::group_by(Model) %>%
        add_trace(x = ~ds, y = ~Cumulative_Value,
                  color = ~Model, colors = model_color_map, # Color by Model name
                  linetype = ~Set, linetypes = linetype_map, # Linetype by Set
                  type = 'scatter', mode = 'lines',
                  # legendgroup = ~Model, # Group legend items
                  name = ~Model # Name traces by Model
        ) %>%
        layout(title = "Cumulative Sum (Actuals + Forecasts)",
               yaxis = list(title = "Cumulative Value"),
               xaxis = list(title = "Date"),
               hovermode = "x unified",
               legend = list(tracegroupgap = 10, # Add gap between model groups in legend
                             title=list(text='<b> Models </b>'),
                             # orientation = "h",
                             xanchor = "center", x = 0.05, y = 0.95)
        )
    }) # End cumulativePlot

    # --- Yearly Plot (Comparing ALL models) ---
    output$yearlyPlot <- plotly::renderPlotly({
      forecast_list <- reactive_forecast_list()
      test_df <- reactive_test_df() %||% data.frame(ds=as.Date(character())) # Ensure test_df is not NULL

      req(forecast_list)
      validate(need(length(forecast_list) > 0, "No forecast results available for yearly plot."))
      validate(need(nrow(test_df)>0, "Test data needed to define start of yearly plot summary.")) # Need test start date

      min_plot_date <- min(test_df$ds) # Start summary from test period start

      # Process each forecast df in the list
      all_yearly_summaries <- purrr::map_dfr(
        .x = forecast_list,
        .id = "Model", # Creates a 'Model' column from list names
        .f = function(fcst_df) {
          if(is.null(fcst_df) || !("yhat" %in% names(fcst_df))) return(NULL) # Skip if invalid
          fcst_df %>%
            dplyr::filter(ds >= min_plot_date) %>% # Filter to test + future forecast
            dplyr::mutate(Year = factor(lubridate::year(ds))) %>%
            dplyr::group_by(Year) %>%
            dplyr::summarise(Value = sum(yhat, na.rm = TRUE), .groups = 'drop')
        }
      )

      validate(need(nrow(all_yearly_summaries) > 0, "Could not generate yearly summary data from forecasts."))

      # Define colors for models
      n_models_plot <- length(unique(all_yearly_summaries$Model))
      model_colors_yr <- RColorBrewer::brewer.pal(max(3, n_models_plot), "Set1") # Use a different palette maybe

      # Create grouped bar chart
      plot_ly(all_yearly_summaries, x = ~Year, y = ~Value, color = ~Model,
              colors = model_colors_yr, # Apply palette
              type = 'bar', barmode = 'group') %>%
        layout(title = "Forecast Sum by Year (Test + Future Periods)",
               yaxis = list(title = "Sum of Forecasted Values"),
               xaxis = list(title = "Year", type = 'category'),
               hovermode = "x unified")
    }) # End yearlyPlot

  })
}
