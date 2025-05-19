# R/mod_model_config_ui.R

#' model_config UI Function
#' @description Module for configuring model parameters using sidebar layout.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny
#' @import shinyjs
mod_model_config_ui <- function(id){
  ns <- NS(id)
  default_horizon <- get_golem_config("forecast_horizon_default") # Returns 150 by default
  tagList(
    useShinyjs(), # Initialize shinyjs
    h4("Model Configuration"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Select Models to Run:"),
        checkboxInput(ns("use_arima"), "ARIMA", value = TRUE),
        checkboxInput(ns("use_ets"), "ETS", value = TRUE),
        checkboxInput(ns("use_tbats"), "TBATS", value = TRUE),
        checkboxInput(ns("use_prophet"), "Prophet", value = TRUE),
        checkboxInput(ns("use_xgboost"), "XGBoost", value = TRUE),
        checkboxInput(ns("use_gam"), "GAM", value = TRUE),
        checkboxInput(ns("use_rf"), "Random Forest", value = TRUE),
        hr(),
        numericInput(ns("forecastHorizon"), "Forecast Horizon (Periods):", value = default_horizon, min = 1, step = 1, max = 365),
        actionButton(ns("runForecast"), "Run Forecast", icon = icon("play-circle"), class = "btn-primary btn-block")
      ),
      mainPanel(
        width = 9,
        h5("Configure Model Parameters:"),
        bslib::accordion( # Using accordion for model parameters
          id = ns("modelParamsAccordion"), # Add an ID if needed for control
          multiple = FALSE, # Allow only one panel open at a time
          # ARIMA Panel
          bslib::accordion_panel(
            title = "ARIMA Parameters",
            value = "ARIMA", # Value for conditional logic if needed
            checkboxInput(ns("arima_auto"), "Auto ARIMA (auto.arima)", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("arima_auto"), "'] == false"),
              numericInput(ns("arima_p"), "Order p (AR):", value = 1, min = 0, max = 5),
              numericInput(ns("arima_d"), "Order d (Diff):", value = 1, min = 0, max = 3),
              numericInput(ns("arima_q"), "Order q (MA):", value = 1, min = 0, max = 5),
              checkboxInput(ns("arima_seasonal"), "Seasonal ARIMA", value = TRUE),
              conditionalPanel(
                condition = paste0("input['", ns("arima_seasonal"), "'] == true && input['", ns("arima_auto"), "'] == false"),
                numericInput(ns("arima_P"), "Seasonal P (SAR):", value = 1, min = 0, max = 3),
                numericInput(ns("arima_D"), "Seasonal D (SDiff):", value = 1, min = 0, max = 2),
                numericInput(ns("arima_Q"), "Seasonal Q (SMA):", value = 1, min = 0, max = 3),
                numericInput(ns("arima_period"), "Seasonal Period (e.g., 7 for daily/weekly, 52 for weekly/yearly):", value = 7, min = 1, max = 366)
              )
            )
          ),
          # ETS Panel
          bslib::accordion_panel(
            title = "ETS Parameters",
            value = "ETS",
            checkboxInput(ns("ets_manual"), "Manual ETS Configuration", value = FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("ets_manual"), "'] == true"),
              selectInput(ns("ets_e"), "Error Component (E):", choices = c("Auto (Z)"="Z", "Additive (A)"="A", "Multiplicative (M)"="M"), selected = "Z"),
              selectInput(ns("ets_t"), "Trend Component (T):", choices = c("Auto (Z)"="Z", "None (N)"="N", "Additive (A)"="A", "Multiplicative (M)"="M"), selected = "Z"),
              selectInput(ns("ets_s"), "Seasonal Component (S):", choices = c("Auto (Z)"="Z", "None (N)"="N", "Additive (A)"="A", "Multiplicative (M)"="M"), selected = "Z"),
              selectInput(ns("ets_damped_str"), "Damped Trend:", choices = c("Auto (NULL)"="NULL", "Yes (TRUE)"="TRUE", "No (FALSE)"="FALSE"), selected = "NULL")
            )
          ),
          # TBATS Panel
          bslib::accordion_panel(
            title = "TBATS Parameters",
            value = "TBATS",
            tags$p("TBATS model is run with automatic parameter selection.")
          ),
          # Prophet Panel
          bslib::accordion_panel(
            title = "Prophet Parameters",
            value = "Prophet",
            checkboxInput(ns("prophet_yearly"), "Yearly Seasonality", value = TRUE),
            checkboxInput(ns("prophet_weekly"), "Weekly Seasonality", value = TRUE),
            checkboxInput(ns("prophet_daily"), "Daily Seasonality (for daily data)", value = FALSE),
            selectInput(ns("prophet_growth"), "Growth Model:", choices = c("linear", "logistic"), selected = "linear"),
            conditionalPanel(
              condition = paste0("input['", ns("prophet_growth"), "'] == 'logistic'"),
              numericInput(ns("prophet_capacity"), "Capacity (Cap for Logistic Growth):", value = 100000)
            ),
            numericInput(ns("prophet_changepoint_scale"), "Changepoint Prior Scale:", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
            # actionButton(ns("load_chile_holidays"), "Load Default Holidays (Chile)", icon = icon("calendar-check")),
            # fileInput(ns("prophet_holidays_file"), "Upload Custom Holidays CSV (optional, cols: holiday, ds, [lower_window, upper_window])", accept = ".csv"),
            fileInput(ns("prophet_regressors_file"), "Upload External Regressors CSV (optional, cols: ds, regressor1, ...)", accept = ".csv")
          ),
          # XGBoost Panel
          bslib::accordion_panel(
            title = "XGBoost Parameters",
            value = "XGBoost",
            checkboxInput(ns("xgb_enable_tuning"), "Enable Hyperparameter Tuning", value = TRUE),
            numericInput(ns("xgb_nrounds"), "Number of Rounds (Trees):", value = 100, min = 10, max = 2000, step = 10),
            numericInput(ns("xgb_eta"), "Learning Rate (eta):", value = 0.1, min = 0.001, max = 0.5, step = 0.01),
            numericInput(ns("xgb_max_depth"), "Max Tree Depth:", value = 6, min = 1, max = 20, step = 1),
            numericInput(ns("xgb_subsample"), "Subsample Ratio of Training Instances:", value = 0.8, min = 0.1, max = 1, step = 0.1),
            numericInput(ns("xgb_colsample"), "Column Sample Ratio per Tree:", value = 0.8, min = 0.1, max = 1, step = 0.1),
            numericInput(ns("xgb_gamma"), "Min Split Loss (gamma):", value = 0, min = 0, max = 10, step = 0.1)
          ),
          # Random Forest Panel
          bslib::accordion_panel(
            title = "Random Forest Parameters",
            value = "RF",
            checkboxInput(ns("rf_enable_tuning"), "Enable Hyperparameter Tuning", value = TRUE),
            numericInput(ns("rf_num_trees"), "Number of Trees:", value = 500, min = 50, max = 2000, step = 50),
            numericInput(ns("rf_mtry"), "Variables per Split (mtry, 0 for auto if tuning off):", value = 0, min = 0, max = 100, step = 1),
            numericInput(ns("rf_min_node_size"), "Min Node Size:", value = 5, min = 1, max = 50, step = 1)
          ),
          # GAM Panel
          bslib::accordion_panel(
            title = "GAM Parameters",
            value = "GAM",
            selectInput(ns("gam_trend_type"), "Trend Type:", choices = c("Linear" = "linear", "Smooth (Spline)" = "smooth"), selected = "linear"),
            checkboxInput(ns("gam_use_season_y"), "Include Yearly Seasonality (Day of Year)", value = TRUE),
            checkboxInput(ns("gam_use_season_w"), "Include Weekly Seasonality (Day of Week)", value = TRUE)
          )
        ) # End bslib::accordion
      ) # End mainPanel
    ) # End sidebarLayout
  ) # End tagList
}
