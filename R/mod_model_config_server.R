# R/mod_model_config_server.R

#' model_config Server Function
#' @description Server logic for model configuration module.
#' @param id Internal parameter for {shiny}.
#' @param i18n The shiny.i18n translator object.
#' @noRd
#' @import shiny
#' @importFrom shinyjs toggleState
mod_model_config_server <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Render translated static text ---
    output$title_model_config <- renderText({ i18n$t("Model Configuration") })
    output$title_select_models <- renderText({ i18n$t("Select Models to Run:") })
    output$title_config_params <- renderText({ i18n$t("Configure Model Parameters:") })
    output$title_arima_params <- renderText({ i18n$t("ARIMA Parameters") })
    output$title_ets_params <- renderText({ i18n$t("ETS Parameters") })
    output$title_tbats_params <- renderText({ i18n$t("TBATS Parameters") })
    output$text_tbats_auto <- renderText({ i18n$t("TBATS model is run with automatic parameter selection.") })
    output$title_prophet_params <- renderText({ i18n$t("Prophet Parameters") })
    output$title_xgboost_params <- renderText({ i18n$t("XGBoost Parameters") })
    output$title_rf_params <- renderText({ i18n$t("Random Forest Parameters") })
    output$title_gam_params <- renderText({ i18n$t("GAM Parameters") })
    output$title_nnetar_params <- renderText({ i18n$t("NNETAR Parameters") })
    output$text_nnetar_desc <- renderText({ i18n$t("Neural Network Autoregressive Model. Predicts based on lagged values of the time series.") })
    output$title_nnetar_structure <- renderText({ i18n$t("Model Structure:") })
    output$help_nnetar_lags <- renderText({ i18n$t("Set p/P to 0 to let nnetar choose automatically. If both >0, specific lags are used. Seasonal P is only effective if data frequency > 1 (e.g., daily/weekly).") })
    output$help_nnetar_size <- renderText({ i18n$t("If 'Auto': For seasonal models (P>0), size is approx. (p+P+1)/2. For non-seasonal (P=0, p>0), size is approx. (p+1)/2. If p=0 and P=0, nnetar attempts to choose p, P, and size.") })
    output$title_nnetar_preprocessing <- renderText({ i18n$t("Data Preprocessing:") })

    # --- Observer to update input labels based on language change ---
    observe({
      req(i18n)
      i18n$get_key_translation() # Dependency on language change

      # Sidebar inputs
      updateCheckboxInput(session, "use_arima", label = i18n$t("ARIMA"))
      updateCheckboxInput(session, "use_ets", label = i18n$t("ETS"))
      updateCheckboxInput(session, "use_tbats", label = i18n$t("TBATS"))
      updateCheckboxInput(session, "use_prophet", label = i18n$t("Prophet"))
      updateCheckboxInput(session, "use_xgboost", label = i18n$t("XGBoost"))
      updateCheckboxInput(session, "use_gam", label = i18n$t("GAM"))
      updateCheckboxInput(session, "use_rf", label = i18n$t("Random Forest"))
      updateCheckboxInput(session, "use_nnetar", label = i18n$t("NNETAR"))
      updateNumericInput(session, "forecastHorizon", label = i18n$t("Forecast Horizon (Periods):"))
      updateActionButton(session, "runForecast", label = i18n$t("Run Forecast"))

      # ARIMA panel
      updateCheckboxInput(session, "arima_auto", label = i18n$t("Auto ARIMA (auto.arima)"))
      updateNumericInput(session, "arima_p", label = i18n$t("Order p (AR):"))
      updateNumericInput(session, "arima_d", label = i18n$t("Order d (Diff):"))
      updateNumericInput(session, "arima_q", label = i18n$t("Order q (MA):"))
      updateCheckboxInput(session, "arima_seasonal", label = i18n$t("Seasonal ARIMA"))
      updateNumericInput(session, "arima_P", label = i18n$t("Seasonal P (SAR):"))
      updateNumericInput(session, "arima_D", label = i18n$t("Seasonal D (SDiff):"))
      updateNumericInput(session, "arima_Q", label = i18n$t("Seasonal Q (SMA):"))
      updateNumericInput(session, "arima_period", label = i18n$t("Seasonal Period (e.g., 7 for daily/weekly, 52 for weekly/yearly):"))

      # ETS panel
      updateCheckboxInput(session, "ets_manual", label = i18n$t("Manual ETS Configuration"))
      updateSelectInput(session, "ets_e", label = i18n$t("Error Component (E):"))
      updateSelectInput(session, "ets_t", label = i18n$t("Trend Component (T):"))
      updateSelectInput(session, "ets_s", label = i18n$t("Seasonal Component (S):"))
      updateSelectInput(session, "ets_damped_str", label = i18n$t("Damped Trend:"))

      # Prophet panel
      updateCheckboxInput(session, "prophet_yearly", label = i18n$t("Yearly Seasonality"))
      updateCheckboxInput(session, "prophet_weekly", label = i18n$t("Weekly Seasonality"))
      updateCheckboxInput(session, "prophet_daily", label = i18n$t("Daily Seasonality (for daily data)"))
      updateSelectInput(session, "prophet_growth", label = i18n$t("Growth Model:"))
      updateNumericInput(session, "prophet_capacity", label = i18n$t("Capacity (Cap for Logistic Growth):"))
      updateNumericInput(session, "prophet_changepoint_scale", label = i18n$t("Changepoint Prior Scale:"))
      updateFileInput(session, "prophet_regressors_file", label = i18n$t("Upload External Regressors CSV (optional, cols: ds, regressor1, ...)"))

      # XGBoost panel
      updateCheckboxInput(session, "xgb_enable_tuning", label = i18n$t("Enable Hyperparameter Tuning"))
      updateNumericInput(session, "xgb_nrounds", label = i18n$t("Number of Rounds (Trees):"))
      updateNumericInput(session, "xgb_eta", label = i18n$t("Learning Rate (eta):"))
      updateNumericInput(session, "xgb_max_depth", label = i18n$t("Max Tree Depth:"))
      updateNumericInput(session, "xgb_subsample", label = i18n$t("Subsample Ratio of Training Instances:"))
      updateNumericInput(session, "xgb_colsample", label = i18n$t("Column Sample Ratio per Tree:"))
      updateNumericInput(session, "xgb_gamma", label = i18n$t("Min Split Loss (gamma):"))

      # Random Forest panel
      updateCheckboxInput(session, "rf_enable_tuning", label = i18n$t("Enable Hyperparameter Tuning"))
      updateNumericInput(session, "rf_num_trees", label = i18n$t("Number of Trees:"))
      updateNumericInput(session, "rf_mtry", label = i18n$t("Variables per Split (mtry, 0 for auto if tuning off):"))
      updateNumericInput(session, "rf_min_node_size", label = i18n$t("Min Node Size:"))

      # GAM panel
      updateSelectInput(session, "gam_trend_type", label = i18n$t("Trend Type:"))
      updateCheckboxInput(session, "gam_use_season_y", label = i18n$t("Include Yearly Seasonality (Day of Year)"))
      updateCheckboxInput(session, "gam_use_season_w", label = i18n$t("Include Weekly Seasonality (Day of Week)"))

      # NNETAR panel
      updateNumericInput(session, "nnetar_p", label = i18n$t("Non-seasonal lags (p) (0 for auto if P=0, or specify e.g., 1, 2)"))
      updateNumericInput(session, "nnetar_P", label = i18n$t("Seasonal lags (P) (0 for non-seasonal, or specify e.g., 1, 2 for seasonal)"))
      updateSelectInput(session, "nnetar_size_method", label = i18n$t("Hidden Layer Neurons (size) - Method"))
      updateNumericInput(session, "nnetar_size_manual", label = i18n$t("Number of Hidden Neurons"))
      updateNumericInput(session, "nnetar_repeats", label = i18n$t("Repeats (for stability)"))
      updateCheckboxInput(session, "nnetar_lambda_auto", label = i18n$t("Box-Cox Lambda (Auto select)"))
      updateNumericInput(session, "nnetar_lambda_manual", label = i18n$t("Manual Lambda (0-1 for Box-Cox, leave NA/blank for no transform)"))

    })

    # --- Observers to toggle numeric input states based on auto/manual selections ---
    # ARIMA Auto vs Manual
    observeEvent(input$arima_auto, {
      # Disable p,d,q,P,D,Q,period if auto is TRUE
      toggleState("arima_p", condition = !input$arima_auto)
      toggleState("arima_d", condition = !input$arima_auto)
      toggleState("arima_q", condition = !input$arima_auto)
      # Seasonal params depend on both arima_auto and arima_seasonal
      is_manual_seasonal_active <- !input$arima_auto && input$arima_seasonal
      toggleState("arima_P", condition = is_manual_seasonal_active)
      toggleState("arima_D", condition = is_manual_seasonal_active)
      toggleState("arima_Q", condition = is_manual_seasonal_active)
      toggleState("arima_period", condition = is_manual_seasonal_active)
    })
    observeEvent(input$arima_seasonal, {
      # This observer is needed if arima_auto is FALSE, to toggle seasonal params
      if (!is.null(input$arima_auto) && !input$arima_auto) {
        is_manual_seasonal_active <- input$arima_seasonal
        toggleState("arima_P", condition = is_manual_seasonal_active)
        toggleState("arima_D", condition = is_manual_seasonal_active)
        toggleState("arima_Q", condition = is_manual_seasonal_active)
        toggleState("arima_period", condition = is_manual_seasonal_active)
      }
    })

    # ETS Manual vs Auto
    observeEvent(input$ets_manual, {
      toggleState("ets_e", condition = input$ets_manual)
      toggleState("ets_t", condition = input$ets_manual)
      toggleState("ets_s", condition = input$ets_manual)
      toggleState("ets_damped_str", condition = input$ets_manual)
    })

    # --- Return a list of reactive expressions for all inputs ---
    return(
      list(
        # Model Selection
        use_arima = reactive(input$use_arima),
        use_ets = reactive(input$use_ets),
        use_tbats = reactive(input$use_tbats),
        use_prophet = reactive(input$use_prophet),
        use_xgboost = reactive(input$use_xgboost),
        use_gam = reactive(input$use_gam),
        use_rf = reactive(input$use_rf),
        use_nnetar = reactive({ input$use_nnetar }),

        # General
        forecast_horizon = reactive(input$forecastHorizon),
        active_tab = reactive(input$modelTabs), # To know which model's params are active
        run_forecast_button = reactive(input$runForecast),

        # ARIMA
        arima_auto = reactive(input$arima_auto),
        arima_p = reactive(input$arima_p),
        arima_d = reactive(input$arima_d),
        arima_q = reactive(input$arima_q),
        arima_seasonal = reactive(input$arima_seasonal),
        arima_P = reactive(input$arima_P),
        arima_D = reactive(input$arima_D),
        arima_Q = reactive(input$arima_Q),
        arima_period = reactive(input$arima_period),

        # ETS
        ets_manual = reactive(input$ets_manual),
        ets_e = reactive(input$ets_e),
        ets_t = reactive(input$ets_t),
        ets_s = reactive(input$ets_s),
        ets_damped_str = reactive(input$ets_damped_str),

        # Prophet
        prophet_yearly = reactive(input$prophet_yearly),
        prophet_weekly = reactive(input$prophet_weekly),
        prophet_daily = reactive(input$prophet_daily),
        prophet_growth = reactive(input$prophet_growth),
        prophet_capacity = reactive(input$prophet_capacity),
        prophet_changepoint_scale = reactive(input$prophet_changepoint_scale),
        prophet_holidays_df = reactive({ NULL }), # Simplified as per instructions
        prophet_regressors_df = reactive({
          req(input$prophet_regressors_file)
          tryCatch({ read.csv(input$prophet_regressors_file$datapath) }, error = function(e) NULL)
        }),

        # XGBoost
        xgb_enable_tuning = reactive(input$xgb_enable_tuning), # New
        xgb_nrounds = reactive(input$xgb_nrounds),
        xgb_eta = reactive(input$xgb_eta),
        xgb_max_depth = reactive(input$xgb_max_depth),
        xgb_subsample = reactive(input$xgb_subsample),
        xgb_colsample = reactive(input$xgb_colsample),
        xgb_gamma = reactive(input$xgb_gamma),

        # Random Forest
        rf_enable_tuning = reactive(input$rf_enable_tuning), # New
        rf_num_trees = reactive(input$rf_num_trees),
        rf_mtry = reactive(input$rf_mtry),
        rf_min_node_size = reactive(input$rf_min_node_size),

        # GAM
        gam_trend_type = reactive(input$gam_trend_type),
        gam_use_season_y = reactive(input$gam_use_season_y),
        gam_use_season_w = reactive(input$gam_use_season_w),

        # NNETAR
        nnetar_p = reactive({ input$nnetar_p }),
        nnetar_P = reactive({ input$nnetar_P }),
        nnetar_size_method = reactive({ input$nnetar_size_method }),
        nnetar_size_manual = reactive({ input$nnetar_size_manual }),
        nnetar_repeats = reactive({ input$nnetar_repeats }),
        nnetar_lambda_auto = reactive({ input$nnetar_lambda_auto }),
        nnetar_lambda_manual = reactive({ input$nnetar_lambda_manual })
      )
    )
  })
}
