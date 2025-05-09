# R/mod_model_summary.R
#' model_summary UI Function
#'
# @description Module for displaying a summary of the run forecast model.
#' @description Module UI for displaying model summaries.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny
# @importFrom rlang %||%
mod_model_summary_ui <- function(id){
  ns <- NS(id)
  # tagList(
  #   h4("Model Summary"), # Section heading
  #   uiOutput(ns("summaryText")) # Dynamic content area
  # )
  tagList(
    h4("Model Summary"),
    # Placeholder for the dropdown selector
    uiOutput(ns("modelSelectorUI")),
    hr(style="margin-top: 5px; margin-bottom: 10px;"), # Add a separator
    # Placeholder for the summary text of the selected model
    uiOutput(ns("summaryText"))
  )
}

#' model_summary Server Function
#'
# @description Renders the model summary text based on the last run model.
#' @description Renders a dropdown to select a model and displays its summary.
#' @param id Internal parameter for {shiny}.
#' @param reactive_run_summary_list A reactive expression returning the list
#'   stored in r$run_models_summary. This list should be named by model
#'   and contain elements like 'config', 'success', 'error',
#'   'arima_order', 'frequency_used'.
#' @param reactive_model_config (Can likely be removed now - config is in the list)
#' @param reactive_model_name A reactive expression returning the name
#'   of the last model run (e.g., "ARIMA", "Prophet", "XGBoost") or NULL.
# @param reactive_model_config A reactive list containing the reactive
#   expressions for all model configuration inputs from mod_model_config.
# @param reactive_arima_selected_order Reactive containing named vector from
#   `forecast::arimaorder` if auto.arima was used, otherwise NULL.
# @param reactive_arima_used_frequency Reactive returning the frequency/period
#   actually used by the ARIMA model (e.g., 7, 52, or user input).
# @param reactive_aggregation_level Reactive returning the aggregation level
#   string ('Daily', 'Weekly').
#
#' @noRd
#' @import shiny dplyr
#' @importFrom rlang %||%

# mod_model_summary_server <- function(id, reactive_model_name, reactive_model_config,
#                                      reactive_arima_selected_order,reactive_aggregation_level,
#                                      reactive_arima_used_frequency){
#   moduleServer( id, function(input, output, session){
mod_model_summary_server <- function(id, reactive_run_summary_list){ # Simplified inputs
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive to get successfully run models and their info
    successful_runs <- reactive({
      summary_list <- reactive_run_summary_list()
      req(summary_list) # Require the list to exist
      # Filter for success and keep only needed info for dropdown/summary
      successful_models <- list()
      for (model_name in names(summary_list)) {
        if (isTRUE(summary_list[[model_name]]$success)) {
          successful_models[[model_name]] <- summary_list[[model_name]]
        }
      }
      # Return NULL if no models succeeded to prevent errors downstream
      if(length(successful_models) == 0) return(NULL) else return(successful_models)
    })

    # --- Render the Model Selection Dropdown ---
    output$modelSelectorUI <- renderUI({
      successful_model_info <- successful_runs()
      # Only show dropdown if there are successful models
      req(successful_model_info)

      model_choices <- names(successful_model_info)
      selectInput(ns("selected_summary_model"),
                  label = "Select Model to View Summary:",
                  choices = model_choices,
                  selected = model_choices[1]) # Select the first one by default
    })

    # --- Render the Summary Text based on Dropdown Selection ---
    output$summaryText <- renderUI({
      selected_model_name <- input$selected_summary_model
      successful_model_info <- successful_runs()

      # Require a model to be selected in the dropdown
      req(selected_model_name, successful_model_info)

      # Get the specific info for the selected model
      model_info <- successful_model_info[[selected_model_name]]
      req(model_info, model_info$config) # Should exist if selected_model_name is valid

      # --- ADD DEBUG: Print received model_info ---
      message("--- mod_model_summary: Received model_info ---")
      print(str(model_info))
      message("--- End received model_info ---")
      # --- END DEBUG ---


      # Extract details from model_info (config, status, etc.)
      config <- model_info$config
      # --- Make sure these were stored in app_server.R ---
      auto_order <- model_info$arima_order # NULL if not ARIMA or not auto
      freq_used <- model_info$frequency_used # NULL if not applicable model type
      agg_level <- model_info$aggregation_level # Need to store this too

      # Generate the summary using the switch statement (similar to before)
      summary_content <- switch(
        selected_model_name, # Switch on the dropdown value
        "ARIMA" = {
          is_auto <- config$auto
          # Add check: If is_auto is not logical TRUE/FALSE, default to FALSE (or throw error)
          if (!isTRUE(is_auto) && !identical(is_auto, FALSE)) {
            warning("Invalid value found for 'config$auto' in summary. Defaulting to FALSE.")
            print(paste("Value was:", is_auto)) # Log problematic value
            is_auto <- FALSE # Default to FALSE if invalid
          }
          message(print(paste("Esto es is_auto after check:", is_auto))) # Debug message
          # --- End Validate ---
          message(print(paste("Esto es is_auto",is_auto)))
          # is_seasonal <- config$arima_seasonal
          is_seasonal <- !is.null(freq_used) && !is.na(freq_used) && freq_used > 1
          # Get user-specified or auto-selected orders
          # Get orders (use auto if available, otherwise config)
          order_p <- if(is_auto && !is.null(auto_order)) auto_order["p"] else config$p
          order_d <- if(is_auto && !is.null(auto_order)) auto_order["d"] else config$d
          order_q <- if(is_auto && !is.null(auto_order)) auto_order["q"] else config$q
          # Only get P,D,Q if seasonal was determined to be active
          order_P <- if(is_seasonal && is_auto && !is.null(auto_order)) auto_order["P"] else if (is_seasonal && !is_auto) config$P else NA
          order_D <- if(is_seasonal && is_auto && !is.null(auto_order)) auto_order["D"] else if (is_seasonal && !is_auto) config$D else NA
          order_Q <- if(is_seasonal && is_auto && !is.null(auto_order)) auto_order["Q"] else if (is_seasonal && !is_auto) config$Q else NA

          # Period: Use auto if available, otherwise user input, default if needed
          order_period <- if(is_seasonal) freq_used else NA # Use the determined frequency
          # order_period <- if(is_auto && !is.null(auto_order) && "frequency" %in% names(auto_order)) {
          #   auto_order["frequency"]
          # } else if(is_seasonal) {
          #   config$period
          # } else { NA } # Period is irrelevant if non-seasonal

          # --- Contextual Interpretation Text ---


          d_interp <- ""
          if (!is.na(order_d) && order_d > 0) {
            d_context <- if (agg_level == "Daily") "day-to-day trends" else if (agg_level=="Weekly") "week-to-week trends" else "trends"
            d_interp <- paste("A non-seasonal differencing order (d) of", order_d,
                              "suggests the model is differencing the data", order_d,
                              "time(s) to make it stationary, likely removing", d_context,".")
          }
          D_interp <- ""
          if (is_seasonal && !is.na(order_D) && order_D > 0 && !is.na(order_period)) {
            D_context <- if (agg_level == "Daily" && order_period==7) "weekly patterns (comparing to same day last week)"
            else if (agg_level == "Weekly" && order_period %in% c(52, 53)) "yearly patterns (comparing to same week last year)"
            else paste0("patterns with a period of ", order_period)
            D_interp <- paste("A seasonal differencing order (D) of", order_D,
                              "suggests the model is using seasonal differencing to remove", D_context,".")
          }
          # --- End Interpretation Text ---

          tagList(
            tags$p(tags$strong("Model Type:"), " ARIMA (Autoregressive Integrated Moving Average)"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li(paste("Selection Method:", ifelse(is_auto, "Automatic (auto.arima)", "Manual"))),
              tags$li(paste0("Non-Seasonal Order (p, d, q): (", order_p, ", ", order_d, ", ", order_q, ")")),
              tags$li(paste("Seasonal Component:", ifelse(is_seasonal, paste0("Enabled (Period=", order_period, ")"), "Disabled"))),
              # Only show seasonal order if enabled
              if (is_seasonal) {
                # tags$li(paste0("Seasonal Order (P, D, Q): (", order_P, ", ", order_D, ", ", order_Q, ")"))
                tags$li(paste0("Seasonal Order (P, D, Q): (", order_P %||% "NA", ", ", order_D %||% "NA", ", ", order_Q %||% "NA", ")")) # Use %||% or similar for NA display
              }
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("ARIMA models capture linear time dependencies."),
              tags$li(tags$code("p/P"), ": Autoregressive components (dependency on past values)."),
              tags$li(tags$code("d/D"), ": Differencing components (to achieve stationarity)."),
              if(d_interp != "") tags$li(d_interp), # Show interpretation if applicable
              if(D_interp != "") tags$li(D_interp), # Show interpretation if applicable
              tags$li(tags$code("q/Q"), ": Moving Average components (dependency on past errors).")
            )
          )
        }, # End ARIMA case

        "ETS" = {
          is_manual <- config$manual
          ets_details <- if(is_manual) {
            # Display the user's selections
            paste0("Manual Spec: E=", config$ets_e, ", T=", config$ets_t, ", S=", config$ets_s,
                   ", Damped=", config$ets_damped_str)
          } else {
            "Automatic Selection (based on AICc)"
          }
          # Use the fitted_method stored in model_info
          fitted_model_desc <- model_info$fitted_method %||% "(Not available)"

          tagList(
            tags$p(tags$strong("Model Type:"), " ETS (Error, Trend, Seasonality)"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li(paste("Selection Method:", ets_details)),
              tags$li(paste("Fitted Model:", fitted_model_desc)) # Placeholder
            ),
            # ... (Interpretation text as before) ...
          )
        }, # End ETS case
        "TBATS" = {
          # Extract details if possible from model object (passed via r$forecast_obj -> reactive input?)
          # For now, just state automatic selection
          tagList(
            tags$p(tags$strong("Model Type:"), " TBATS"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li("Automatic selection via ", tags$code("forecast::tbats()"), "."),
              tags$li(paste("Fitted Model:", model_info$fitted_method %||% "(Not available)")) # Display fitted method
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Handles complex seasonality (multiple periods, non-integer) using trigonometric functions (Fourier series)."),
              tags$li("Also models trend, ARMA error correlation, and can apply Box-Cox transformation.")
            )
          )
        },

        "Prophet" = {
          growth_model <- config$growth
          tagList(
            tags$p(tags$strong("Model Type:"), " Prophet (Additive Regression Model)"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li(paste("Growth Model:", growth_model)),
              if(growth_model == "logistic") {
                tags$li(paste("Capacity (Cap):", config$prophet_capacity))
              },
              tags$li(paste("Yearly Seasonality:", ifelse(config$yearly, "Enabled", "Disabled"))),
              tags$li(paste("Weekly Seasonality:", ifelse(config$weekly, "Enabled", "Disabled"))),
              tags$li(paste("Daily Seasonality:", ifelse(config$daily, "Enabled", "Disabled"))),
              tags$li(paste("Changepoint Prior Scale:", config$changepoint_scale)),
              # Check if holidays were actually used (check if the reactive value is non-NULL)
              if(!is.null(config$used_holidays)) { # Access reactive value
                tags$li("Holidays: Custom holiday file provided.")
              } else {
                tags$li("Holidays: None provided.")
              },
              if(!is.null(config$used_regressors)) { # Access reactive value
                # Maybe list names if not too many?
                regressor_names <- setdiff(names(config$used_regressors), "ds")
                tags$li(paste("External Regressors:", paste(regressor_names, collapse=", ")))
              } else {
                tags$li("External Regressors: None provided.")
              }
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Prophet decomposes the time series into trend, seasonalities, and holiday effects."),
              tags$li(paste("Trend modeled as piecewise", config$growth,".")),
              tags$li(paste("Changepoint Prior Scale (", config$changepoint_scale ,") controls trend flexibility (larger = more flexible).")),
              # List enabled seasonalities
              { enabled_seasons <- c
              if(config$yearly) enabled_seasons <- c(enabled_seasons, "Yearly")
              if(config$weekly) enabled_seasons <- c(enabled_seasons, "Weekly")
              if(config$daily) enabled_seasons <- c(enabled_seasons, "Daily")
              tags$li(paste("Seasonalities Enabled:", paste(enabled_seasons, collapse=", ") ))
              },
              if(!is.null(config$used_holidays)) tags$li("Custom holidays included.")
            )
          )
        }, # End Prophet case

        "XGBoost" = {
          tagList(
            tags$p(tags$strong("Model Type:"), " XGBoost (eXtreme Gradient Boosting - Tree-based Ensemble)"),
            tags$p(tags$strong("Feature Engineering:")),
            tags$ul(
              tags$li("Time series features generated automatically via a recipe:"),
              tags$ul(
                tags$li("Date components (year, month, week, day of week, etc.)"),
                tags$li("Lagged values of the target variable."),
                tags$li("Rolling window statistics (mean, sd) on lagged values."),
                tags$li("Fourier terms for seasonality.")
              )
            ),
            tags$p(tags$strong("Hyperparameters:")),
            # Display Tuned Parameters if available
            if (!is.null(model_info$tuned_params)) {
              tuned <- model_info$tuned_params
              tags$ul(
                tags$li(paste("Tuning Method: Time Series CV + tune_grid")),
                tags$li(tags$strong("Best Parameters Found:")),
                tags$ul(
                  # Iterate through the tuned parameters tibble
                  # Use names() and tuned[[name]] to display dynamically
                  lapply(names(tuned)[!names(tuned) %in% ".config"], function(param_name) {
                     tags$li(paste0(param_name, ": ", round(tuned[[param_name]], 4))) # Round numeric values
                  })
                )
              )
            } else {
              # Fallback if tuning wasn't run or failed (shouldn't happen with current setup)
              tags$ul(
                tags$li(paste("Number of Rounds (Trees):", config$xgb_nrounds)),
                tags$li(paste("Learning Rate (eta):", config$xgb_eta)),
                tags$li(paste("Max Tree Depth:", config$xgb_max_depth)),
                tags$li(paste("Subsample Ratio:", config$xgb_subsample)),
                tags$li(paste("Column Sample Ratio:", config$xgb_colsample)),
                tags$li(paste("Min Split Loss (gamma):", config$xgb_gamma))
              )
            },
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("XGBoost captures potentially complex non-linear patterns and feature interactions."),
              tags$li("Relies heavily on engineered features (lags, date parts, rolling stats, Fourier terms) to understand time dynamics."),
              tags$li("Does not inherently extrapolate trends like ARIMA/Prophet.")
            )
          )
        }, # End XGBoost case

        "GAM" = {
          gam_trend_type <- config$smooth_trend
          gam_use_season_y <- config$use_season_y
          gam_use_season_w <- config$use_season_w
          tagList(
            tags$p(tags$strong("Model Type:"), " GAM (Generalized Additive Model)"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li(paste("Trend Type:", gam_trend_type)),
              tags$li(paste("Yearly Seasonality (Day of Year):", ifelse(gam_use_season_y, "Included (Smooth)", "Excluded"))),
              tags$li(paste("Weekly Seasonality (Day of Week):", ifelse(gam_use_season_w, "Included (Smooth)", "Excluded")))
              # Add regressors/holidays if implemented
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Models components using flexible smooth functions (splines)."),
              tags$li("Captures non-linear trends and complex seasonal patterns."),
              tags$li("Assumes errors are independent (autocorrelation might need addressing via residuals or model structure if significant).")
            )
          )
        }, # End GAM case
        "RF" = {
          num_trees <- config$rf_num_trees
          mtry_in <- config$rf_mtry
          mtry_disp <- if (!is.null(mtry_in) && mtry_in > 0) as.character(mtry_in) else "Auto (sqrt(p))"
          node_size <- config$rf_min_node_size
          tagList(
            tags$p(tags$strong("Model Type:"), " Random Forest (Tree Ensemble via 'ranger')"),
            tags$p(tags$strong("Feature Engineering:")),
            tags$ul(
              # Copied from XGBoost - assumes same recipe
              tags$li("Time series features generated automatically via a recipe:"),
              tags$ul(
                tags$li("Date components (year, month, week, etc.)"),
                tags$li("Lagged values of the target variable."),
                tags$li("Rolling window statistics (mean, sd) on lagged values."),
                tags$li("Fourier terms for seasonality.")
              )
            ),
            tags$p(tags$strong("Key Hyperparameters:")),
            tags$ul(
              tags$li(paste("Number of Trees:", num_trees %||% 500)),
              tags$li(paste("Variables per Split (mtry):", mtry_disp)),
              tags$li(paste("Min Node Size:", node_size %||% 5))
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Builds multiple independent decision trees on bootstrapped samples of data and features."),
              tags$li("Predictions are typically the average of individual tree predictions."),
              tags$li("Effective for non-linear patterns and interactions; often robust to overfitting."),
              tags$li("Like XGBoost, relies on engineered features for time dynamics.")
            )
          )
        }, # End RF case
        "RF" = {
          tagList(
            tags$p(tags$strong("Model Type:"), " Random Forest (Tree Ensemble via 'ranger')"),
            tags$p(tags$strong("Feature Engineering:")),
            tags$ul(
              # Copied from XGBoost - assumes same recipe
              tags$li("Time series features generated automatically via a recipe:"),
              tags$ul(
                tags$li("Date components (year, month, week, etc.)"),
                tags$li("Lagged values of the target variable."),
                tags$li("Rolling window statistics (mean, sd) on lagged values."),
                tags$li("Fourier terms for seasonality.")
              )
            ),
            tags$p(tags$strong("Hyperparameters:")),
            # Display Tuned Parameters if available
            if (!is.null(model_info$tuned_params)) {
              tuned <- model_info$tuned_params
              tags$ul(
                tags$li(paste("Tuning Method: Time Series CV + tune_grid")),
                tags$li(tags$strong("Best Parameters Found:")),
                tags$ul(
                  lapply(names(tuned)[!names(tuned) %in% ".config"], function(param_name) {
                     tags$li(paste0(param_name, ": ", round(tuned[[param_name]], 4)))
                  })
                )
              )
            } else {
              # Fallback to original config if tuning info not present
              num_trees <- config$rf_num_trees
              mtry_in <- config$rf_mtry
              mtry_disp <- if (!is.null(mtry_in) && mtry_in > 0) as.character(mtry_in) else "Auto (sqrt(p))"
              node_size <- config$rf_min_node_size
              tags$ul(
                tags$li(paste("Number of Trees:", num_trees %||% 500)),
                tags$li(paste("Variables per Split (mtry):", mtry_disp)),
                tags$li(paste("Min Node Size:", node_size %||% 5))
              )
            },
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Builds multiple independent decision trees on bootstrapped samples of data and features."),
              tags$li("Predictions are typically the average of individual tree predictions."),
              tags$li("Effective for non-linear patterns and interactions; often robust to overfitting."),
              tags$li("Like XGBoost, relies on engineered features for time dynamics.")
            )
          )
        }, # End RF case

        # Default case if model_name is not recognized
        {
          tags$p("Summary not available for this model type.")
        }
      ) # End switch
      tags$div(class = "model-summary-box", summary_content)

    }) # End renderUI

    # Return the reactive input value so it can be observed by other modules
    return(
      list(
        selected_model = reactive({ input$selected_summary_model })
      )
    )

  }) # End moduleServer
} # End server function
