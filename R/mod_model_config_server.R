# R/mod_model_config_server.R

#' model_config Server Function
#' @description Server logic for model configuration module.
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @import shiny
#' @importFrom shinyjs toggleState
mod_model_config_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
