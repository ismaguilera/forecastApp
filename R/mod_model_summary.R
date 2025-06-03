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
  tagList(
    h4(textOutput(ns("ui_model_summary_title_h4"), inline = TRUE)),
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
                  label = i18n$t("Select Model to View Summary:"),
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
            d_context_key <- if (agg_level == "Daily") "day-to-day trends" else if (agg_level=="Weekly") "week-to-week trends" else "trends"
            d_interp <- sprintf(i18n$t("A non-seasonal differencing order (d) of %d suggests the model is differencing the data %d time(s) to make it stationary, likely removing %s."),
                                order_d, order_d, i18n$t(d_context_key))
          }
          D_interp <- ""
          if (is_seasonal && !is.na(order_D) && order_D > 0 && !is.na(order_period)) {
            D_context_key <- if (agg_level == "Daily" && order_period==7) "weekly patterns (comparing to same day last week)"
            else if (agg_level == "Weekly" && order_period %in% c(52, 53)) "yearly patterns (comparing to same week last year)"
            else "patterns with a period of %d" # This one needs sprintf later

            if (grepl("%d", D_context_key)) {
                D_interp <- sprintf(i18n$t("A seasonal differencing order (D) of %d suggests the model is using seasonal differencing to remove %s."),
                                    order_D, sprintf(i18n$t(D_context_key), order_period))
            } else {
                D_interp <- sprintf(i18n$t("A seasonal differencing order (D) of %d suggests the model is using seasonal differencing to remove %s."),
                                    order_D, i18n$t(D_context_key))
            }
          }
          # --- End Interpretation Text ---

          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("ARIMA (Autoregressive Integrated Moving Average)")),
            tags$p(tags$strong(i18n$t("Configuration:"))),
            tags$ul(
              tags$li(paste(i18n$t("Selection Method:"), ifelse(is_auto, i18n$t("Automatic (auto.arima)"), i18n$t("Manual")))),
              tags$li(paste0(i18n$t("Non-Seasonal Order (p, d, q):"), " (", order_p, ", ", order_d, ", ", order_q, ")")),
              tags$li(paste(i18n$t("Seasonal Component:"), ifelse(is_seasonal, sprintf(i18n$t("Enabled (Period="), order_period), i18n$t("Disabled")))),
              if (is_seasonal) {
                tags$li(paste0(i18n$t("Seasonal Order (P, D, Q):"), " (", order_P %||% "NA", ", ", order_D %||% "NA", ", ", order_Q %||% "NA", ")"))
              }
            ),
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("ARIMA models capture linear time dependencies.")),
              tags$li(tags$code("p/P"), i18n$t(": Autoregressive components (dependency on past values).")),
              tags$li(tags$code("d/D"), i18n$t(": Differencing components (to achieve stationarity).")),
              if(d_interp != "") tags$li(d_interp),
              if(D_interp != "") tags$li(D_interp),
              tags$li(tags$code("q/Q"), i18n$t(": Moving Average components (dependency on past errors)."))
            )
          )
        }, # End ARIMA case

        "ETS" = {
          is_manual <- config$manual
          ets_details <- if(is_manual) {
            sprintf(i18n$t("Manual Spec: E=%s, T=%s, S=%s, Damped=%s"), config$ets_e, config$ets_t, config$ets_s, config$ets_damped_str)
          } else {
            i18n$t("Automatic Selection (based on AICc)")
          }
          fitted_model_desc <- model_info$fitted_method %||% i18n$t("(Not available)")

          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("ETS (Error, Trend, Seasonality)")),
            tags$p(tags$strong(i18n$t("Configuration:"))),
            tags$ul(
              tags$li(paste(i18n$t("Selection Method:"), ets_details)),
              tags$li(paste(i18n$t("Fitted Model:"), fitted_model_desc))
            )
          )
        }, # End ETS case
        "TBATS" = {
          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("TBATS")),
            tags$p(tags$strong(i18n$t("Configuration:"))),
            tags$ul(
              tags$li(i18n$t("Automatic selection via forecast::tbats().")),
              tags$li(paste(i18n$t("Fitted Model:"), model_info$fitted_method %||% i18n$t("(Not available)")))
            ),
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("Handles complex seasonality (multiple periods, non-integer) using trigonometric functions (Fourier series).")),
              tags$li(i18n$t("Also models trend, ARMA error correlation, and can apply Box-Cox transformation."))
            )
          )
        },

        "Prophet" = {
          growth_model <- config$growth
          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("Prophet (Additive Regression Model)")),
            tags$p(tags$strong(i18n$t("Configuration:"))),
            tags$ul(
              tags$li(paste(i18n$t("Growth Model:"), growth_model)),
              if(growth_model == "logistic") {
                tags$li(paste(i18n$t("Capacity (Cap):"), config$prophet_capacity))
              },
              tags$li(paste(i18n$t("Yearly Seasonality"),":", ifelse(config$yearly, i18n$t("Enabled"), i18n$t("Disabled")))),
              tags$li(paste(i18n$t("Weekly Seasonality"),":", ifelse(config$weekly, i18n$t("Enabled"), i18n$t("Disabled")))),
              tags$li(paste(i18n$t("Daily Seasonality (for daily data)"),":", ifelse(config$daily, i18n$t("Enabled"), i18n$t("Disabled")))),
              tags$li(paste(i18n$t("Changepoint Prior Scale:"), config$changepoint_scale)),
              if(isTRUE(config$used_holidays)) {
                tags$li(i18n$t("Holidays: Custom holiday file provided."))
              } else {
                tags$li(i18n$t("Holidays: None provided."))
              },
              if(isTRUE(config$used_regressors)) {
                 regressor_names <- setdiff(names(config$used_regressors %||% list()), "ds") # Ensure config$used_regressors is not NULL
                 tags$li(paste(i18n$t("External Regressors:"), paste(regressor_names, collapse=", ")))
              } else {
                tags$li(i18n$t("External Regressors: None provided."))
              }
            ),
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("Prophet decomposes the time series into trend, seasonalities, and holiday effects.")),
              tags$li(sprintf(i18n$t("Trend modeled as piecewise %s."), config$growth)),
              tags$li(sprintf(i18n$t("Changepoint Prior Scale (%s) controls trend flexibility (larger = more flexible)."), config$changepoint_scale)),
              { enabled_seasons <- c()
              if(config$yearly) enabled_seasons <- c(enabled_seasons, i18n$t("Yearly"))
              if(config$weekly) enabled_seasons <- c(enabled_seasons, i18n$t("Weekly"))
              if(config$daily) enabled_seasons <- c(enabled_seasons, i18n$t("Daily"))
              tags$li(paste(i18n$t("Seasonalities Enabled:"), paste(enabled_seasons, collapse=", ") ))
              },
              if(isTRUE(config$used_holidays)) tags$li(i18n$t("Custom holidays included."))
            )
          )
        }, # End Prophet case

        "XGBoost" = {
          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("XGBoost (eXtreme Gradient Boosting - Tree-based Ensemble)")),
            tags$p(tags$strong(i18n$t("Feature Engineering:"))),
            tags$ul(
              tags$li(i18n$t("Time series features generated automatically via a recipe:")),
              tags$ul(
                tags$li(i18n$t("Date components (year, month, week, day of week, etc.)")),
                tags$li(i18n$t("Lagged values of the target variable.")),
                tags$li(i18n$t("Rolling window statistics (mean, sd) on lagged values.")),
                tags$li(i18n$t("Fourier terms for seasonality."))
              )
            ),
            tags$p(tags$strong(i18n$t("Hyperparameters:"))),
            if (!is.null(model_info$tuned_params)) {
              tuned <- model_info$tuned_params
              tags$ul(
                tags$li(i18n$t("Tuning Method: Time Series CV + tune_grid")),
                tags$li(tags$strong(i18n$t("Best Parameters Found:"))),
                tags$ul(
                  lapply(names(tuned)[!names(tuned) %in% ".config"], function(param_name) {
                     tags$li(paste0(param_name, ": ", round(tuned[[param_name]], 4)))
                  })
                )
              )
            } else {
              tags$ul(
                tags$li(sprintf(i18n$t("Number of Rounds (Trees): %s"), config$xgb_nrounds)),
                tags$li(sprintf(i18n$t("Learning Rate (eta): %s"), config$xgb_eta)),
                tags$li(sprintf(i18n$t("Max Tree Depth: %s"), config$xgb_max_depth)),
                tags$li(sprintf(i18n$t("Subsample Ratio: %s"), config$xgb_subsample)),
                tags$li(sprintf(i18n$t("Column Sample Ratio: %s"), config$xgb_colsample)),
                tags$li(sprintf(i18n$t("Min Split Loss (gamma): %s"), config$xgb_gamma))
              )
            },
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("XGBoost captures potentially complex non-linear patterns and feature interactions.")),
              tags$li(i18n$t("Relies heavily on engineered features (lags, date parts, rolling stats, Fourier terms) to understand time dynamics.")),
              tags$li(i18n$t("Does not inherently extrapolate trends like ARIMA/Prophet."))
            )
          )
        }, # End XGBoost case

        "GAM" = {
          gam_trend_type_display <- if (config$smooth_trend) i18n$t("Smooth (Spline)") else i18n$t("Linear")
          gam_use_season_y_display <- ifelse(config$use_season_y, i18n$t("Included (Smooth)"), i18n$t("Excluded"))
          gam_use_season_w_display <- ifelse(config$use_season_w, i18n$t("Included (Smooth)"), i18n$t("Excluded"))
          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("GAM (Generalized Additive Model)")),
            tags$p(tags$strong(i18n$t("Configuration:"))),
            tags$ul(
              tags$li(paste(i18n$t("Trend Type: %s"), gam_trend_type_display)),
              tags$li(paste(i18n$t("Yearly Seasonality (Day of Year): %s"), gam_use_season_y_display)),
              tags$li(paste(i18n$t("Weekly Seasonality (Day of Week): %s"), gam_use_season_w_display))
            ),
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("Models components using flexible smooth functions (splines).")),
              tags$li(i18n$t("Captures non-linear trends and complex seasonal patterns.")),
              tags$li(i18n$t("Assumes errors are independent (autocorrelation might need addressing via residuals or model structure if significant)."))
            )
          )
        }, # End GAM case
        "RF" = { # Combined RF logic, handles tuned and non-tuned
          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("Random Forest (Tree Ensemble via 'ranger')")),
            tags$p(tags$strong(i18n$t("Feature Engineering:"))),
            tags$ul(
              tags$li(i18n$t("Time series features generated automatically via a recipe:")),
              tags$ul(
                tags$li(i18n$t("Date components (year, month, week, day of week, etc.)")),
                tags$li(i18n$t("Lagged values of the target variable.")),
                tags$li(i18n$t("Rolling window statistics (mean, sd) on lagged values.")),
                tags$li(i18n$t("Fourier terms for seasonality."))
              )
            ),
            tags$p(tags$strong(i18n$t("Hyperparameters:"))),
            if (!is.null(model_info$tuned_params)) {
              tuned <- model_info$tuned_params
              tags$ul(
                tags$li(i18n$t("Tuning Method: Time Series CV + tune_grid")),
                tags$li(tags$strong(i18n$t("Best Parameters Found:"))),
                tags$ul(
                  lapply(names(tuned)[!names(tuned) %in% ".config"], function(param_name) {
                     tags$li(paste0(param_name, ": ", round(tuned[[param_name]], 4)))
                  })
                )
              )
            } else {
              num_trees <- config$rf_num_trees
              mtry_in <- config$rf_mtry
              mtry_disp <- if (!is.null(mtry_in) && mtry_in > 0) as.character(mtry_in) else i18n$t("Auto (sqrt(p))")
              node_size <- config$rf_min_node_size
              tags$ul(
                tags$li(sprintf(i18n$t("Number of Trees: %s"), num_trees %||% 500)),
                tags$li(sprintf(i18n$t("Variables per Split (mtry): %s"), mtry_disp)),
                tags$li(sprintf(i18n$t("Min Node Size: %s"), node_size %||% 5))
              )
            },
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("Builds multiple independent decision trees on bootstrapped samples of data and features.")),
              tags$li(i18n$t("Predictions are typically the average of individual tree predictions.")),
              tags$li(i18n$t("Effective for non-linear patterns and interactions; often robust to overfitting.")),
              tags$li(i18n$t("Like XGBoost, relies on engineered features for time dynamics."))
            )
          )
        }, # End RF case
        "NNETAR" = {
          cfg <- model_info$config
          lambda_display <- if (isTRUE(cfg$nnetar_lambda_auto)) i18n$t("Auto") else (cfg$nnetar_lambda_manual %||% "N/A")
          size_display <- if (cfg$nnetar_size_method == "auto") i18n$t("Auto") else as.character(cfg$nnetar_size_manual %||% "N/A")

          tagList(
            tags$p(tags$strong(i18n$t("Model Type:")), i18n$t("NNETAR (Neural Network Autoregression)")),
            tags$p(tags$strong(i18n$t("Configuration:"))),
            tags$ul(
              tags$li(sprintf(i18n$t("Non-seasonal Lags (p): %s"), cfg$nnetar_p %||% "N/A")),
              tags$li(sprintf(i18n$t("Seasonal Lags (P): %s"), cfg$nnetar_P %||% "N/A")),
              tags$li(paste(i18n$t("Hidden Layer Size Calculation:"), i18n$t(cfg$nnetar_size_method %||% "N/A"))), # Translate "auto" or "manual"
              if(cfg$nnetar_size_method == "manual") {
                tags$li(sprintf(i18n$t("Manual Hidden Layer Size (size): %s"), size_display))
              },
              tags$li(sprintf(i18n$t("Number of Networks to Average (repeats): %s"), cfg$nnetar_repeats %||% "N/A")),
              tags$li(sprintf(i18n$t("Box-Cox Transformation (lambda): %s"), lambda_display))
            ),
            tags$p(tags$strong(i18n$t("Fitted Model Details:"))),
            tags$ul(
              tags$li(paste(i18n$t("Fitted Model:"), model_info$fitted_method %||% i18n$t("(Not available)"))),
              tags$li(sprintf(i18n$t("Frequency Used: %s"), model_info$frequency_used %||% "N/A"))
            ),
            tags$p(tags$strong(i18n$t("Interpretation:"))),
            tags$ul(
              tags$li(i18n$t("NNETAR is a feed-forward neural network model that uses lagged values of the time series as inputs.")),
              tags$li(tags$code("p"), i18n$t("(Non-seasonal lags): Number of past non-seasonal observations used as predictors.")),
              tags$li(tags$code("P"), i18n$t("(Seasonal lags): Number of past seasonal observations (e.g., same period last year/season) used as predictors.")),
              tags$li(tags$code("Size"), i18n$t("(Hidden Layer Nodes): Number of nodes in the single hidden layer. 'auto' typically calculates as (p+P+1)/2. More nodes allow for more complex patterns but risk overfitting.")),
              tags$li(tags$code("Repeats"), sprintf(i18n$t(": The model is fitted multiple times (e.g., %s times) with different random starting weights, and the results are averaged to improve robustness and avoid poor local optima."), cfg$nnetar_repeats %||% "N/A")),
              tags$li(tags$code("Lambda"), i18n$t(": Parameter for Box-Cox transformation. 'auto' selects lambda automatically; a specific value applies that transformation to stabilize variance."))
            )
          )
        }, # End NNETAR case
        {
          tags$p(i18n$t("Summary not available for this model type."))
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
