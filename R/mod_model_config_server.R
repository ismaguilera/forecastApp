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

    # --- ReactiveVal for default holidays ---
    default_holidays_data <- reactiveVal(NULL)

    observeEvent(input$load_chile_holidays, {
      message("Load Default Holidays (Chile) button clicked.")
      tryCatch({
        # Construct the path relative to the app's root directory
        # Assumes feriados.csv is in inst/extdata/
        # Correct path for golem app structure when running from dev/run_dev.R or deployed
        holidays_path <- app_sys("extdata", "feriados.csv")

        if (file.exists(holidays_path)) {
          df <- read.csv(holidays_path, fileEncoding="UTF-8-BOM") # Added encoding just in case
          # Rename columns to 'ds' and 'holiday'
          if ("Fecha" %in% names(df) && "Feriados_chilenos" %in% names(df)) {
            df <- df %>%
              dplyr::rename(ds = Fecha, holiday = Feriados_chilenos) %>%
              dplyr::mutate(ds = as.Date(ds)) %>%
              dplyr::select(ds, holiday) # Keep only these two
          } else {
            stop("Default holidays file must contain 'Fecha' and 'Feriados_chilenos' columns.")
          }

          # Ensure 'ds' column is Date type after renaming
          if (!inherits(df$ds, "Date")) {
             stop("'ds' column in default holidays could not be coerced to Date.")
          }
          default_holidays_data(df)
          showNotification("Default Chilean holidays loaded and processed.", type = "message", duration = 5)
          # Optionally, disable fileInput or change its label
          # shinyjs::disable("prophet_holidays_file")
        } else {
          warning(paste("Default holidays file not found at:", holidays_path))
          showNotification("Default holidays file not found.", type = "error", duration = 5)
          default_holidays_data(NULL)
        }
      }, error = function(e) {
        warning(paste("Error loading default holidays:", conditionMessage(e)))
        showNotification(paste("Error loading default holidays:", conditionMessage(e)), type = "error", duration = 10)
        default_holidays_data(NULL)
      })
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
        prophet_holidays_df = reactive(input$holidays_file),
        # prophet_holidays_df = reactive({
        #   # Prioritize uploaded file
        #   if (!is.null(input$prophet_holidays_file) && !is.null(input$prophet_holidays_file$datapath)) {
        #     message("Using uploaded custom holidays file.")
        #     default_holidays_data(NULL) # Clear default if custom is uploaded
        #     tryCatch({
        #       df <- read.csv(input$prophet_holidays_file$datapath, sep = ",", fileEncoding = "UTF-8")
        #       # Rename columns to 'ds' and 'holiday'
        #       if ("Fecha" %in% names(df) && "Feriados_chilenos" %in% names(df)) {
        #         df <- df %>%
        #           dplyr::rename(ds = Fecha, holiday = Feriados_chilenos) %>%
        #           dplyr::mutate(ds = as.Date(ds)) %>%
        #           dplyr::select(ds, holiday) # Keep only these two
        #       } else {
        #         stop("El archivo de feriados debe contener las columnas 'Fecha' y 'Feriados_chilenos'.")
        #       }
        #       return(df)
        #     }, error = function(e) {
        #       showNotification(paste("Error al leer el archivo de feriados:", conditionMessage(e)), type = "error", duration = 10)
        #       return(NULL)
        #     })
        #   } else if (!is.null(default_holidays_data())) {
        #     message("Using default holidays data.")
        #     return(default_holidays_data())
        #   }
        #   return(NULL)
        # }),
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
        gam_use_season_w = reactive(input$gam_use_season_w)
      )
    )
  })
}
