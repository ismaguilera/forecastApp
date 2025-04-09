# R/mod_model_summary.R
# Helper for displaying NA nicely (put in R/utils.R or similar if needed often)
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

#' model_summary UI Function
#'
#' @description Module for displaying a summary of the run forecast model.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny
mod_model_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Model Summary"), # Section heading
    uiOutput(ns("summaryText")) # Dynamic content area
  )
}

#' model_summary Server Function
#'
#' @description Renders the model summary text based on the last run model.
#' @param id Internal parameter for {shiny}.
#' @param reactive_model_name A reactive expression returning the name
#'   of the last model run (e.g., "ARIMA", "Prophet", "XGBoost") or NULL.
#' @param reactive_model_config A reactive list containing the reactive
#'   expressions for all model configuration inputs from mod_model_config.
#' @param reactive_arima_selected_order Reactive containing named vector from
#'   `forecast::arimaorder` if auto.arima was used, otherwise NULL.
#' @param reactive_arima_used_frequency Reactive returning the frequency/period
#'   actually used by the ARIMA model (e.g., 7, 52, or user input).
#' @param reactive_aggregation_level Reactive returning the aggregation level
#'   string ('Daily', 'Weekly').
#'
#' @noRd
#' @import shiny forecast
mod_model_summary_server <- function(id, reactive_model_name, reactive_model_config,
                                     reactive_arima_selected_order,reactive_aggregation_level,
                                     reactive_arima_used_frequency){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$summaryText <- renderUI({
      model_name <- reactive_model_name()
      config <- reactive_model_config # Get the list of config reactives
      auto_order <- reactive_arima_selected_order() # Get the selected order (is NULL if not auto)
      agg_level <- reactive_aggregation_level() # Get 'Daily' or 'Weekly'
      freq_used <- reactive_arima_used_frequency()
      # Only display summary if a model has been run
      req(model_name)

      # --- Generate Summary based on Model Name ---

      summary_content <- switch(
        model_name,
        "ARIMA" = {
          is_auto <- config$arima_auto()
          # is_seasonal <- config$arima_seasonal()
          is_seasonal <- !is.null(freq_used) && !is.na(freq_used) && freq_used > 1
          # Get user-specified or auto-selected orders
          # Get orders (use auto if available, otherwise config)
          order_p <- if(is_auto && !is.null(auto_order)) auto_order["p"] else config$arima_p()
          order_d <- if(is_auto && !is.null(auto_order)) auto_order["d"] else config$arima_d()
          order_q <- if(is_auto && !is.null(auto_order)) auto_order["q"] else config$arima_q()
          # Only get P,D,Q if seasonal was determined to be active
          order_P <- if(is_seasonal && is_auto && !is.null(auto_order)) auto_order["P"] else if (is_seasonal && !is_auto) config$arima_P() else NA
          order_D <- if(is_seasonal && is_auto && !is.null(auto_order)) auto_order["D"] else if (is_seasonal && !is_auto) config$arima_D() else NA
          order_Q <- if(is_seasonal && is_auto && !is.null(auto_order)) auto_order["Q"] else if (is_seasonal && !is_auto) config$arima_Q() else NA

          # Period: Use auto if available, otherwise user input, default if needed
          order_period <- if(is_seasonal) freq_used else NA # Use the determined frequency
          # order_period <- if(is_auto && !is.null(auto_order) && "frequency" %in% names(auto_order)) {
          #   auto_order["frequency"]
          # } else if(is_seasonal) {
          #   config$arima_period()
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
          is_manual <- config$ets_manual()
          ets_details <- if(is_manual) {
            # Display the user's selections
            paste0("Manual Spec: E=", config$ets_e(), ", T=", config$ets_t(), ", S=", config$ets_s(),
                   ", Damped=", config$ets_damped_str())
          } else {
            "Automatic Selection (based on AICc)"
          }
          # Placeholder for actual fitted model - requires passing model$method back
          fitted_model_desc <- "(Check console logs for final fitted model type)"

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
              tags$li("Automatic selection of Box-Cox, Trend, Damping, ARMA errors, and Seasonal components via ", tags$code("forecast::tbats()"), ".")
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Handles complex seasonality (multiple periods, non-integer) using trigonometric functions (Fourier series)."),
              tags$li("Also models trend, ARMA error correlation, and can apply Box-Cox transformation.")
            )
          )
        },

        "Prophet" = {
          growth_model <- config$prophet_growth()
          tagList(
            tags$p(tags$strong("Model Type:"), " Prophet (Additive Regression Model)"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li(paste("Growth Model:", growth_model)),
              if(growth_model == "logistic") {
                tags$li(paste("Capacity (Cap):", config$prophet_capacity()))
              },
              tags$li(paste("Yearly Seasonality:", ifelse(config$prophet_yearly(), "Enabled", "Disabled"))),
              tags$li(paste("Weekly Seasonality:", ifelse(config$prophet_weekly(), "Enabled", "Disabled"))),
              tags$li(paste("Daily Seasonality:", ifelse(config$prophet_daily(), "Enabled", "Disabled"))),
              tags$li(paste("Changepoint Prior Scale:", config$prophet_changepoint_scale())),
              # Check if holidays were actually used (check if the reactive value is non-NULL)
              if(!is.null(config$prophet_holidays_df())) { # Access reactive value
                tags$li("Holidays: Custom holiday file provided.")
              } else {
                tags$li("Holidays: None provided.")
              },
              if(!is.null(config$prophet_regressors_df())) { # Access reactive value
                # Maybe list names if not too many?
                regressor_names <- setdiff(names(config$prophet_regressors_df()), "ds")
                tags$li(paste("External Regressors:", paste(regressor_names, collapse=", ")))
              } else {
                tags$li("External Regressors: None provided.")
              }
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("Prophet decomposes the time series into trend, seasonalities, and holiday effects."),
              tags$li(paste("Trend modeled as piecewise", config$prophet_growth(),".")),
              tags$li(paste("Changepoint Prior Scale (", config$prophet_changepoint_scale() ,") controls trend flexibility (larger = more flexible).")),
              # List enabled seasonalities
              { enabled_seasons <- c()
                if(config$prophet_yearly()) enabled_seasons <- c(enabled_seasons, "Yearly")
                if(config$prophet_weekly()) enabled_seasons <- c(enabled_seasons, "Weekly")
                if(config$prophet_daily()) enabled_seasons <- c(enabled_seasons, "Daily")
                tags$li(paste("Seasonalities Enabled:", paste(enabled_seasons, collapse=", ") ))
              },
               if(!is.null(config$prophet_holidays_df())) tags$li("Custom holidays included.")
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
            tags$p(tags$strong("Key Hyperparameters:")),
            tags$ul(
              tags$li(paste("Number of Rounds (Trees):", config$xgb_nrounds())),
              tags$li(paste("Learning Rate (eta):", config$xgb_eta())),
              tags$li(paste("Max Tree Depth:", config$xgb_max_depth())),
              tags$li(paste("Subsample Ratio:", config$xgb_subsample())),
              tags$li(paste("Column Sample Ratio:", config$xgb_colsample())),
              tags$li(paste("Min Split Loss (gamma):", config$xgb_gamma()))
            ),
            tags$p(tags$strong("Interpretation:")),
            tags$ul(
              tags$li("XGBoost captures potentially complex non-linear patterns and feature interactions."),
              tags$li("Relies heavily on engineered features (lags, date parts, rolling stats, Fourier terms) to understand time dynamics."),
              tags$li("Does not inherently extrapolate trends like ARIMA/Prophet.")
            )
          )
        }, # End XGBoost case

        "GAM" = {
          trend_type <- config$gam_trend_type()
          season_y <- config$gam_use_season_y()
          season_w <- config$gam_use_season_w()
          tagList(
            tags$p(tags$strong("Model Type:"), " GAM (Generalized Additive Model)"),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li(paste("Trend Type:", trend_type)),
              tags$li(paste("Yearly Seasonality (Day of Year):", ifelse(season_y, "Included (Smooth)", "Excluded"))),
              tags$li(paste("Weekly Seasonality (Day of Week):", ifelse(season_w, "Included (Smooth)", "Excluded")))
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

        # Default case if model_name is not recognized
        {
          tags$p("Summary not available for this model type.")
        }
      ) # End switch

      # Wrap content in a div for styling if needed
      tags$div(class = "model-summary-box", summary_content)

    }) # End renderUI

  }) # End moduleServer
} # End server function


