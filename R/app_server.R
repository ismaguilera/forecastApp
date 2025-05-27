# R/app_server.R

#' The application server-side
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny dplyr tibble forecast parsnip workflows tune dials rsample yardstick timetk recipes slider
#' @importFrom shinyjs reset
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats predict 
#' @importFrom utils head capture.output str packageVersion
#' @importFrom purrr reduce 
#' @importFrom rlang `%||%`
# Needed for forecast() call inside observeEvent
# Add other necessary imports if functions are called directly here
#' @noRd
app_server <- function(input, output, session) {

  # --- Reactive Values Store ---
  r <- reactiveValues(
    run_id = 0, # Trigger for plot update
    # forecast_obj = NULL, # Can store raw forecast output if needed
    # forecast_df = NULL, # Tibble for plotting
    metrics_summary = NULL, # Tibble for table
    # model_name = NULL, # Store name of model run
    # arima_selected_order = NULL,
    # arima_used_frequency = NULL,
    forecast_list = list(), # Store list of forecast tibbles
    fitted_list = list(), # Store list of fitted value vectors
    metrics_list = list(), # Store list of metric tibbles (for later)
    # model_summary_list = list() # Store list of model summary info (for later)
    run_models_summary = list(),
    global_holidays_data = reactiveVal(NULL)
  )

  # --- Module Calls ---
  data_input_reactives <- mod_data_input_server("data_input_1")
  preprocess_reactives <- mod_preprocess_controls_server(
    "preprocess_controls_1",
    data_input_reactives = data_input_reactives
  )

  model_config_reactives <- mod_model_config_server("model_config_1")

  mod_decomposition_plot_server(
    "decomposition_plot_1",
    reactive_aggregated_df = preprocess_reactives$reactive_aggregated_df,
    reactive_aggregation_level = preprocess_reactives$reactive_agg_level
  )

  mod_results_plot_server(
    "results_plot_1",
    reactive_train_df = preprocess_reactives$reactive_train_df,
    reactive_test_df = preprocess_reactives$reactive_test_df,
    # reactive_forecast_df = reactive({ r$forecast_df }) # Use forecast_df for plot
    reactive_forecast_list = eventReactive(r$run_id, { r$forecast_list }) # Pass the list of forecast tibbles, triggered by run_id
  )
  # mod_model_summary_server(
  #   "model_summary_1",
  #   reactive_model_name = reactive({ r$model_name }), # Pass reactive model name
  #   reactive_model_config = model_config_reactives,  # Pass the whole list of config reactives
  #   reactive_arima_selected_order = reactive({ r$arima_selected_order }),
  #   reactive_aggregation_level = preprocess_reactives$reactive_agg_level,
  #   reactive_arima_used_frequency = reactive({ r$arima_used_frequency }) # Pass new reactive
  # )

  mod_model_summary_server(
    "model_summary_1",
    # Pass the reactive list containing summaries for all run models
    reactive_run_summary_list = eventReactive(r$run_id, {
      req(r$run_id > 0)
      r$run_models_summary # Pass the whole list
    }, ignoreNULL = FALSE)
    # Remove older individual reactive arguments
  ) -> summary_reactives # Assign module output to a variable

  mod_results_table_server(
    "results_table_1",
    reactive_metrics_summary = reactive({ r$metrics_summary })
  )

  # mod_extra_plots_server(
  #   "extra_plots_1",
  #   reactive_train_df = preprocess_reactives$reactive_train_df,
  #   reactive_test_df = preprocess_reactives$reactive_test_df,
  #   reactive_forecast_df = reactive({ r$forecast_df }))

  mod_extra_plots_server(
    "extra_plots_1",
    reactive_train_df = preprocess_reactives$reactive_train_df,
    reactive_test_df = preprocess_reactives$reactive_test_df,
    # Pass the list of forecasts, triggered by run_id
    reactive_forecast_list = eventReactive(r$run_id, {
      req(r$run_id > 0)
      r$forecast_list
    }, ignoreNULL = FALSE),
    # Pass the list of fitted values, triggered by run_id
    reactive_fitted_list = eventReactive(r$run_id, {
       req(r$run_id > 0)
       r$fitted_list
    }, ignoreNULL = FALSE),
    # Pass the selected model name from the summary module
    reactive_selected_summary_model = summary_reactives$selected_model # Assuming the summary module returns the input value
  )


  # --- Guided Tour ---
  # Define the steps for the introduction tour
  tour_steps <- reactive({
    data.frame(
      # Use CSS selectors targeting the namespaced IDs
      element = c(
        "#data_input_1-fileUpload-label",      # Use the actual fileInput ID
        "#data_input_1-dateCol-label",        # Target date column selector
        # Maybe target the div containing preprocess controls? Or specific control
        "#preprocess_controls_1-aggregationLevel-label",
        "#preprocess_controls_1-trainTestSplit",
        # Target the actual tabsetPanel container
        # Note: Might need to inspect element in browser to confirm the exact ID renderered
        # Usually it's the ID passed to tabsetPanel + "-pane-" + tab value,
        # but targeting the overall container '#model_config_1-modelTabs' might be safer
        "#model_config_1-modelTabs",
        "#model_config_1-forecastHorizon",
        "#model_config_1-runForecast",
        "#results_plot_1-forecastPlot",  # Plot output area
        "#results_table_1-metricsTable" # Metrics table output area
      ),
      intro = c(
        "Welcome! Start by uploading your time series data here (CSV or Excel).",
        "Once uploaded, select the columns containing your dates and values.",
        "Choose how to aggregate your data (Daily/Weekly) and how to aggregate if Weekly.",
        "Select the percentage of data to use for training the model.",
        "Configure the parameters for the desired forecast model (ARIMA, Prophet, or XGBoost) using these tabs.",
        "Set how many periods (days/weeks) you want to forecast into the future.",
        "Click here to train the selected model and generate the forecast based on your configuration.",
        "The forecast results will be plotted here against the historical data.",
        "Performance metrics (MAE, RMSE, MAPE) for the model fit will be displayed here."
      ),
      position = c( # Optional: Suggest positions for the tooltips
        "bottom",
        "bottom",
        "bottom",
        "bottom",
        "bottom",
        "right",
        "right",
        "top",
        "top"
      )
    )
  })

  # Observe the button click to start the tour
  observeEvent(input$startTour, {
    # Use session explicitly provided to app_server
    # --- DEBUG: Check if event fires ---
    # print("Start Tour button observed!")
    showNotification("Tour button clicked! Preparing tour...", type="message", duration = 3)
    # --- End DEBUG --
    # --- DEBUG: Define simple steps targeting only the main H1 title ---
    simple_steps <- data.frame(
      element = "h1", # CSS selector for the main title
      intro = "This is the main application title.",
      position = "bottom" # Position tooltip below the title
    )
    # --- End DEBUG ---
    rintrojs::introjs(session,
                      options = list(steps = tour_steps(),
                                     'showBullets' = FALSE, # Example option
                                     'showProgress' = TRUE)) # Example option
  })

  observeEvent(input$global_holidays_file, {
    req(input$global_holidays_file)
    inFile <- input$global_holidays_file
    df_holidays <- NULL
    tryCatch({
      df <- utils::read.csv(inFile$datapath, stringsAsFactors = FALSE, header = TRUE)
      # Validar y procesar df (debe tener columnas 'ds' y 'holiday')
      req("Fecha" %in% names(df), "Feriados_chilenos" %in% names(df)) # Original column names
      df_holidays <- df %>%
        dplyr::rename(ds = Fecha, holiday = Feriados_chilenos) %>%
        dplyr::mutate(ds = lubridate::as_date(ds)) %>%
        dplyr::select(ds, holiday) %>%
        dplyr::filter(!is.na(ds) & !is.na(holiday))
      req(nrow(df_holidays) > 0, "Processed holiday data is empty. Ensure correct format and non-empty data.")
      r$global_holidays_data(df_holidays)
      shiny::showNotification("Global holidays file uploaded and processed successfully.", type = "message")
    }, error = function(e) {
      r$global_holidays_data(NULL) # Reset on error
      error_message <- paste("Error processing global holidays file. Please check format (CSV with 'Fecha', 'Feriados_chilenos' columns) and content. Original error:", e$message)
      shiny::showNotification(error_message, type = "error", duration = 10)
    })
  })

  observeEvent(input$load_default_holidays, {
    req(input$load_default_holidays) # Triggered by button press
    df_holidays <- NULL
    tryCatch({
      default_h_file_name <- get_golem_config("default_holiday_file")
      req(default_h_file_name, "Default holiday file name not configured.")
      default_h_file_path <- app_sys("extdata", default_h_file_name)
      req(file.exists(default_h_file_path), paste("Default holiday file not found at:", default_h_file_path))
      
      df <- utils::read.csv(default_h_file_path, stringsAsFactors = FALSE, header = TRUE, fileEncoding="UTF-8-BOM")
      req("Fecha" %in% names(df), "Feriados_chilenos" %in% names(df)) # Original column names
      df_holidays <- df %>%
        dplyr::rename(ds = Fecha, holiday = Feriados_chilenos) %>%
        dplyr::mutate(ds = lubridate::as_date(ds)) %>%
        dplyr::select(ds, holiday) %>%
        dplyr::filter(!is.na(ds) & !is.na(holiday))
      req(nrow(df_holidays) > 0, "Processed default holiday data is empty.")
      r$global_holidays_data(df_holidays)
      shiny::showNotification("Default global holidays loaded successfully.", type = "message")
    }, error = function(e) {
      r$global_holidays_data(NULL) # Reset on error
      error_message <- paste("Error loading default global holidays. Please check the file and application configuration. Original error:", e$message)
      shiny::showNotification(error_message, type = "error", duration = 10)
    })
  })

  # --- Model Execution Logic ---
  observeEvent(model_config_reactives$run_forecast_button(), {
    message("Run Forecast button clicked.")
    # Get required inputs reactively
    train_df <- preprocess_reactives$reactive_train_df()
    test_df <- preprocess_reactives$reactive_test_df()
    full_aggregated_df <- preprocess_reactives$reactive_aggregated_df()
    agg_level <- preprocess_reactives$reactive_agg_level()
    horizon <- model_config_reactives$forecast_horizon()
    current_global_holidays <- r$global_holidays_data()


    # --- Determine Selected Models from Individual Checkboxes ---
    # selected_models_now <- c() # Start with empty vector
    # if (isTRUE(model_config_reactives$use_arima())) { # Check if TRUE
    #   selected_models_now <- c(selected_models_now, "ARIMA")
    # }
    # if (isTRUE(model_config_reactives$use_prophet())) {
    #   selected_models_now <- c(selected_models_now, "Prophet")
    # }
    # if (isTRUE(model_config_reactives$use_xgboost())) {
    #   selected_models_now <- c(selected_models_now, "XGBoost")
    # }
    # if (isTRUE(model_config_reactives$use_ets())) {
    #   selected_models_now <- c(selected_models_now, "ETS")
    # }
    # if (isTRUE(model_config_reactives$use_tbats())) {
    #   selected_models_now <- c(selected_models_now, "TBATS")
    # }
    # if (isTRUE(model_config_reactives$use_rf())) {
    #   selected_models_now <- c(selected_models_now, "RF")
    # }
    # if (isTRUE(model_config_reactives$use_gam())) {
    #   selected_models_now <- c(selected_models_now, "GAM")
    # }
    # --- End Determine Selected Models ---
    model_checks <- list(
      ARIMA = model_config_reactives$use_arima(),
      ETS = model_config_reactives$use_ets(),
      TBATS = model_config_reactives$use_tbats(),
      Prophet = model_config_reactives$use_prophet(),
      XGBoost = model_config_reactives$use_xgboost(),
      GAM = model_config_reactives$use_gam(),
      RF = model_config_reactives$use_rf(),
      NNETAR = model_config_reactives$use_nnetar() # Add this line
    )

    selected_models_now <- names(model_checks)[sapply(model_checks, isTRUE)]
    # selected_models_now <- model_config_reactives$selected_models() # Get selected models
    validate(need(length(selected_models_now) > 0, "Please select at least one model to run."))
    message(paste("Models selected:", paste(selected_models_now, collapse=", ")))

    # Validation
    req(train_df, test_df, full_aggregated_df, agg_level, horizon)
    validate(need(length(selected_models_now) > 0, "Please select at least one model to run."))
    message(paste("Models selected:", paste(selected_models_now, collapse=", ")))

    # --- Reset results lists ---
    r$forecast_list <- list()
    r$fitted_list <- list()
    r$metrics_list <- list()
    r$model_summary_list <- list()
    # --- End Reset ---

    n_models <- length(selected_models_now)
    progress_inc <- 1 / n_models # Progress increment per model




    # --- 1. Input Validation & Data Prep ---
    # Use req() to ensure data and configs are available
    # req(
    #   preprocess_reactives$reactive_train_df(),
    #   preprocess_reactives$reactive_test_df(),
    #   preprocess_reactives$reactive_aggregated_df(), # Needed for XGBoost features
    #   preprocess_reactives$reactive_agg_level(),
    #   model_config_reactives$active_tab(),
    #   model_config_reactives$forecast_horizon()
    # )


    # train_df <- preprocess_reactives$reactive_train_df()
    # test_df <- preprocess_reactives$reactive_test_df()
    # full_aggregated_df <- preprocess_reactives$reactive_aggregated_df()
    # active_model_tab <- model_config_reactives$active_tab()
    # horizon <- model_config_reactives$forecast_horizon()
    # agg_level <- preprocess_reactives$reactive_agg_level()
    freq_str <- if (agg_level == "Daily") "day" else "week"

    n_test_periods <- nrow(test_df)
    n_future_periods <- horizon # User requested future horizon
    total_periods_needed <- n_test_periods + n_future_periods

    last_train_date <- max(train_df$ds)
    by_period_forecast <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    

    future_dates_for_fcst <- seq.Date(
      from = last_train_date + by_period_forecast, # Comienza después del último dato de entrenamiento
      by = freq_str,
      length.out = total_periods_needed
    )
    # --- Nombres de columnas de feriados del entrenamiento (para consistencia en ARIMA xreg) ---
    # Esto se debe obtener DESPUÉS de entrenar el modelo ARIMA la primera vez,
    # o pasarlo como atributo del modelo. Por ahora, lo definiremos como NULL
    # y lo actualizaremos después de entrenar ARIMA.
    arima_xreg_colnames_from_training <- NULL
    # Basic check for enough training data
    validate(need(nrow(train_df) >= 5, "Need at least 5 training data points.")) # Adjust as needed

    # Reset previous results
    # r$forecast_obj <- NULL
    # r$forecast_df <- NULL
    r$metrics_summary <- NULL
    # r$model_name <- NULL
    # r$arima_selected_order <- NULL
    # r$arima_used_frequency <- NULL
    model_success <- FALSE # Flag


    shiny::withProgress(message = 'Running Forecast...', value = 0, {
      temp_summary_list <- list() # Temp list to build summaries
      tryCatch({ # Outer tryCatch for overall process
        successful_models <- c() # Keep track of models that ran ok
        for (i in seq_along(selected_models_now)) {
          model_name <- selected_models_now[i]
          message(paste("--- Starting Model:", model_name, "---"))
          current_progress <- (i-1) * progress_inc
          shiny::incProgress(amount = 0, # Update message first
                             detail = paste("Running", model_name,"(", i, "of", n_models,")"))

          forecast_tibble <- NULL # Initialize for this model
          fitted_values <- NULL # Initialize for this model
          model_run_success <- FALSE
          model_summary_entry <- list(config = list(), success = FALSE, error = NULL,
                                      aggregation_level = agg_level, # Store agg level
                                      frequency_used = NULL, # Store frequency
                                      arima_order = NULL, # Store ARIMA auto order
                                      fitted_method = NULL) # Store fitted method string (ETS/TBATS)

      # tryCatch(
      #   { # Wrap entire process in tryCatch
          # --- 2. Model Selection & Execution ---
          # model_name <- active_model_tab # Assuming tab name is model name
          # message(paste("Attempting to run model:", model_name))
          model_name <- selected_models_now[i]
          message(paste("--- Starting Model:", model_name, "---"))
          current_progress <- (i-1) * progress_inc
          shiny::incProgress(amount = 0, # Update message first
                             detail = paste("Running", model_name,"(", i, "of", n_models,")"))

          forecast_tibble <- NULL # Initialize for this model
          fitted_values <- NULL # Initialize for this model
          model_run_success <- FALSE

          tryCatch({ # Wrap each model run
            # --- Preparación de Feriados Específica para el Pronóstico ---
            # Para ARIMA (future_xreg) y GAM (future_holidays_df para generar features)
            future_holidays_df_for_model <- NULL
            if (!is.null(current_global_holidays) && nrow(current_global_holidays) > 0) {
                future_holidays_df_for_model <- current_global_holidays %>%
                    dplyr::mutate(ds = as.Date(ds)) %>%
                    dplyr::filter(ds %in% future_dates_for_fcst) # Solo feriados en el horizonte de pronóstico
            }
            

            # --- Model-Specific Logic ---
            if (model_name == "ARIMA") {
              config <- list(
                auto = model_config_reactives$arima_auto(),
                p = model_config_reactives$arima_p(),
                d = model_config_reactives$arima_d(),
                q = model_config_reactives$arima_q(),
                seasonal = model_config_reactives$arima_seasonal(),
                P = model_config_reactives$arima_P(),
                D = model_config_reactives$arima_D(),
                Q = model_config_reactives$arima_Q(),
                period = model_config_reactives$arima_period()
              ) # Extract ARIMA config from model_config_reactives
              message("ARIMA Config stored for summary:")
              freq_used <- 1 # Default
              if (config$seasonal) {
                if (config$auto) {
                  if (agg_level == "Daily") freq_used <- 7
                  else if (agg_level == "Weekly") freq_used <- 52
                  # else keep freq_used = 1 or handle other agg_levels
                } else { # Manual seasonal
                  manual_period <- as.integer(config$period)
                  if (!is.na(manual_period) && manual_period > 1) {
                    freq_used <- manual_period
                  } else {
                    # If manual period is invalid, treat as non-seasonal for summary
                    config$seasonal <- FALSE # Correct the config flag locally
                    freq_used <- 1
                    warning("Manual seasonal period invalid (<=1), treating as non-seasonal.")
                  }
                }
              } # else it stays 1 (non-seasonal)
              # r$arima_used_frequency <- freq_used # Store the calculated frequency
              message(paste("ARIMA frequency determined as:", freq_used))
              model_summary_entry$frequency_used <- freq_used # Store frequency
              model_summary_entry$config <- config


              model_or_fcst_obj <- train_arima(train_df, config, aggregation_level = agg_level, holidays_df = current_global_holidays)
              req(model_or_fcst_obj, "ARIMA model training failed (returned NULL).") # Check result
              
              future_xreg_arima <- NULL
              # Guardar los nombres de las columnas de los regresores de feriados usados en el entrenamiento
              # Esto es crucial. train_arima debería devolver esto o adjuntarlo al modelo.
              # Asumamos que model_arima_obj$xreg contiene la matriz usada en el entrenamiento si se usaron feriados.
              if (!is.null(model_or_fcst_obj$xreg)) {
                  message("ARIMA: Model was trained with xreg. Preparing future_xreg.")
                  arima_xreg_colnames_from_training <- colnames(model_or_fcst_obj$xreg)
                  
                  if (!is.null(future_holidays_df_for_model) && nrow(future_holidays_df_for_model) > 0 &&
                      !is.null(arima_xreg_colnames_from_training)) {
                      
                      # Crear dummies para las fechas futuras, asegurando las mismas columnas que en el entrenamiento
                      future_holiday_dummies <- future_holidays_df_for_model %>%
                          dplyr::mutate(holiday = make.names(holiday), value = 1) %>%
                          tidyr::pivot_wider(names_from = holiday, values_from = value, values_fill = 0)
                      
                      # Crear un dataframe base con todas las fechas futuras y todas las columnas de feriados del entrenamiento
                      future_xreg_df_base <- data.frame(ds = future_dates_for_fcst)
                      for (col_name in arima_xreg_colnames_from_training) {
                          future_xreg_df_base[[col_name]] <- 0 # Inicializar todas las dummies de feriados a 0
                      }
                      
                      # Unir las dummies de feriados futuros que realmente ocurren
                      # y actualizar las columnas correspondientes en future_xreg_df_base
                      if (nrow(future_holiday_dummies) > 0 && ncol(future_holiday_dummies) > 1) { # >1 para asegurar que hay más que solo 'ds'
                          common_cols_to_join <- intersect(names(future_xreg_df_base), names(future_holiday_dummies))
                          
                          # Asegurar que 'ds' sea la única columna común para el join
                          cols_from_future_dummies <- setdiff(names(future_holiday_dummies), "ds")
                          
                          temp_join_df <- future_xreg_df_base %>% dplyr::select(ds) %>%
                            dplyr::left_join(future_holiday_dummies %>% dplyr::select(ds, all_of(cols_from_future_dummies)), by = "ds")

                          # Actualizar las columnas en future_xreg_df_base
                          for(col_h in cols_from_future_dummies) {
                              if(col_h %in% names(future_xreg_df_base) && col_h %in% names(temp_join_df)) {
                                  future_xreg_df_base[[col_h]] <- dplyr::coalesce(temp_join_df[[col_h]], future_xreg_df_base[[col_h]])
                              }
                          }
                      }
                      
                      future_xreg_arima <- future_xreg_df_base %>%
                          dplyr::select(all_of(arima_xreg_colnames_from_training)) %>% # Asegurar el orden y las columnas
                          as.matrix()
                          
                      if(nrow(future_xreg_arima) != total_periods_needed) {
                          stop("Constructed future_xreg_arima rows do not match total_periods_needed.")
                      }
                      message(paste("ARIMA: future_xreg prepared with", ncol(future_xreg_arima), "columns."))
                  } else if (is.null(arima_xreg_colnames_from_training)) {
                      message("ARIMA: No xreg column names found from training model, cannot create future_xreg reliably.")
                  } else {
                      # Si no hay feriados en el futuro pero el modelo se entrenó con ellos,
                      # necesitamos una matriz de ceros con las columnas correctas.
                      future_xreg_arima <- matrix(0,
                                                  nrow = total_periods_needed,
                                                  ncol = length(arima_xreg_colnames_from_training),
                                                  dimnames = list(NULL, arima_xreg_colnames_from_training))
                      message("ARIMA: No future holidays, but model used xreg. Created zero matrix for future_xreg.")
                  }
              } else {
                  message("ARIMA: Model was not trained with xreg. future_xreg will be NULL.")
              }

              # --- ADD: Extract & Store Auto ARIMA Order ---
              if (config$auto) {
                message("Auto ARIMA selected. Extracting order...")
                sel_order <- tryCatch({
                  forecast::arimaorder(model_or_fcst_obj) # Get the selected order
                }, error = function(e_ord){
                  warning("Could not extract order from auto.arima model object.")
                  NULL
                })
                # r$arima_selected_order <- sel_order # Store it (can be NULL if failed)
                if(!is.null(sel_order)) {message("Stored auto order: ", paste(names(sel_order), sel_order, collapse=", "))}
                model_summary_entry$arima_order <- sel_order
              } else {
                # r$arima_selected_order <- NULL # Ensure it's NULL if not auto
                model_summary_entry$arima_order <- NULL
              }
              # Call updated forecast_arima
              forecast_output <- forecast_arima(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str, future_xreg = future_xreg_arima)
              forecast_tibble <- forecast_output$forecast # Tibble for plotting/test metrics
              fitted_values <- forecast_output$fitted    # Vector for train metrics

              req(forecast_output, forecast_tibble, fitted_values)
              # message("ARIMA forecast generated.")


              metrics_list <- list()
              # --- Train Metrics ---
              train_actual <- train_df$y
              if (!is.null(fitted_values) && length(fitted_values) == length(train_actual)) {
                train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
                if (!is.null(train_metrics_tbl)) {
                  metrics_list$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
                } else { message("ARIMA Training metrics calculation failed.") } # Use message for console debugging
              } else {
                message(paste("ARIMA Fitted values length mismatch:", length(fitted_values), "vs", length(train_actual), ". Skipping train metrics."))
              }
              # --- Test Metrics ---
              if (nrow(test_df) > 0) {
                test_actual <- test_df$y
                # Filter forecast_tibble for dates matching the test set
                test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)

                if (nrow(test_pred_df) == nrow(test_df)) {
                  # Ensure order matches test_df$ds
                  test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
                  test_pred <- test_pred_ordered_df$yhat
                  test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
                  if(!is.null(test_metrics_tbl)){
                    metrics_list$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)
                  } else { message("ARIMA Test metrics calculation failed.") }
                } else {
                  message("ARIMA: Could not align test predictions (count mismatch). Filtered preds: ", nrow(test_pred_df), ", Actuals: ", nrow(test_df), ". Skipping test metrics.")
                }
              }


            } else if (model_name == "ETS") {
              config <- list(
                manual = model_config_reactives$ets_manual(),
                ets_e = model_config_reactives$ets_e(),
                ets_t = model_config_reactives$ets_t(),
                ets_s = model_config_reactives$ets_s(),
                ets_damped_str = model_config_reactives$ets_damped_str()
              ) # Extract ETS config
              model_summary_entry$config <- config
              model_or_fcst_obj <- train_ets(train_df, config, agg_level, total_periods_needed)
              req(model_or_fcst_obj, "ETS/STLF training/forecasting failed (returned NULL).")
              freq_used <- ifelse(agg_level == "Daily",7,52) # Default
              # if (agg_level == "Daily") freq_used <- 7
              # else if (agg_level == "Weekly") freq_used <- 52
              model_summary_entry$frequency_used <- freq_used # Store frequency
              if(inherits(model_or_fcst_obj, "ets")) model_summary_entry$fitted_method <- model_or_fcst_obj$method
              if(inherits(model_or_fcst_obj, "forecast") &&
                 !is.null(model_or_fcst_obj$model)){
                model_summary_entry$fitted_method <- model_or_fcst_obj$model$method # From stlf underlying model
                }
              forecast_output <- forecast_ets(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
              forecast_tibble <- forecast_output$forecast
              fitted_values <- forecast_output$fitted
              req(forecast_output, forecast_tibble, fitted_values, "ETS forecast processing failed.")


            } else if (model_name == "TBATS") {
              config <- list() # No config yet
              model_summary_entry$config <- config
              model_or_fcst_obj <- train_tbats(train_df, config, agg_level)
              req(model_or_fcst_obj)
              forecast_output <- forecast_tbats(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
              forecast_tibble <- forecast_output$forecast
              fitted_values <- forecast_output$fitted
              model_summary_entry$fitted_method <- capture.output(print(model_or_fcst_obj))[1] # Example
              req(forecast_output, forecast_tibble, fitted_values, "TBATS forecasting failed.")
            } else if (model_name == "Prophet") {
              message("Prophet: Attempting to retrieve config values...")
              current_growth <- tryCatch({ model_config_reactives$prophet_growth() }, error = function(e) { message("Error getting prophet_growth"); NULL})
              req(current_growth, "Failed to get Prophet growth parameter.")
              
              holidays_input <- current_global_holidays
              # holidays_input can be NULL, so no req() here unless it's mandatory based on other settings
              
              regressors_input <- tryCatch({ model_config_reactives$prophet_regressors_df() }, error = function(e) { message("Error getting prophet_regressors_df"); NULL})
              # regressors_input can be NULL
              
              prophet_capacity_val <- if(current_growth == 'logistic') {
                tryCatch({ model_config_reactives$prophet_capacity() }, error = function(e) { message("Error getting prophet_capacity"); NULL})
              } else { NULL }
              if(current_growth == 'logistic') req(prophet_capacity_val, "Failed to get Prophet capacity for logistic growth.")

              message("Prophet: Config values retrieved (or are NULL).")
              config <- list(
                yearly = model_config_reactives$prophet_yearly(), # Assuming these are safe
                weekly = model_config_reactives$prophet_weekly(),
                daily = model_config_reactives$prophet_daily(),
                growth = current_growth,
                changepoint_scale = model_config_reactives$prophet_changepoint_scale(),
                capacity = prophet_capacity_val, # Use the retrieved value
                used_holidays = !is.null(holidays_input),
                used_regressors = !is.null(regressors_input) && length(setdiff(names(regressors_input %||% list()), "ds")) > 0
              )
              model_summary_entry$config <- config

              message("--- Prophet: Debugging Inputs (Post Config Creation) ---") # Moved debug block
              message("--- Prophet: Debugging Inputs ---")
              message("Prophet Config:")
              print(str(config))
              message("Holidays Input (holidays_input):")
              if(is.null(holidays_input)) message("  NULL") else print(str(head(holidays_input)))
              message("Regressors Input (regressors_input):")
              if(is.null(regressors_input)) message("  NULL") else print(str(head(regressors_input)))
              
              regressor_names_input <- NULL
              if(!is.null(regressors_input)){ 
                regressor_names_input <- setdiff(names(regressors_input), "ds")
                message(paste("  Regressor Names (regressor_names_input):", paste(regressor_names_input, collapse=", ")))
              }
              if(length(regressor_names_input) == 0) {
                regressors_input <- NULL # Treat as no regressors if only 'ds' or empty
                message("  No valid regressor columns found, setting regressors_input to NULL.")
              }

              prophet_train_df <- train_df
              if(current_growth == 'logistic'){ 
                prophet_train_df$cap <- model_config_reactives$prophet_capacity() 
                message(paste("  Capacity for logistic growth:", model_config_reactives$prophet_capacity()))
              }
              message("Prophet Train DF (prophet_train_df) head:")
              print(str(head(prophet_train_df)))
              message("--- End Prophet Debugging Inputs ---")
              
              message("Prophet: Calling train_prophet...")
              model_or_fcst_obj <- train_prophet(prophet_train_df, config, holidays_input, regressors_input, regressor_names_input)
              req(model_or_fcst_obj, "Prophet model training failed (train_prophet returned NULL).")
              message("Prophet: train_prophet successful.")

              message("Prophet: Calling forecast_prophet...")
              forecast_tibble <- forecast_prophet(model_or_fcst_obj, total_periods_needed, freq_str, config$capacity, regressors_input, regressor_names_input)
              req(forecast_tibble, "Prophet forecast generation failed (forecast_prophet returned NULL).") 
              message("Prophet: forecast_prophet successful.")
              
              req(is.data.frame(forecast_tibble) && all(c("ds", "yhat") %in% names(forecast_tibble)),
                  "Prophet: Forecast tibble structure is invalid after forecast_prophet.")
              message("Prophet: Forecast tibble structure valid.")
              
              fitted_values <- forecast_tibble %>% dplyr::filter(ds %in% train_df$ds) %>% pull(yhat)
              req(length(fitted_values) == nrow(train_df), "Prophet: Fitted values length mismatch with train_df.")
              message("Prophet: Fitted values extracted successfully.")

            } else if (model_name == "XGBoost") {
              config <- list(
                nrounds = model_config_reactives$xgb_nrounds(),
                eta = model_config_reactives$xgb_eta(),
                max_depth = model_config_reactives$xgb_max_depth(),
                subsample = model_config_reactives$xgb_subsample(),
                colsample_bytree = model_config_reactives$xgb_colsample(),
                gamma = model_config_reactives$xgb_gamma()
              ) # Extract XGBoost config
              
              # Get the enable_tuning reactive
              enable_xgb_tuning <- model_config_reactives$xgb_enable_tuning()
              model_summary_entry$config <- config # Store original config
              model_summary_entry$tuning_enabled <- enable_xgb_tuning # Store if tuning was run

              if (isTRUE(enable_xgb_tuning)) {
                message("XGBoost: Hyperparameter tuning ENABLED.")
                # --- XGBoost Tuning Workflow ---
                message("Setting up XGBoost tuning workflow...")
                # 1. Get UNPREPARED recipe
              unprepared_recipe <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
              req(unprepared_recipe, "XGBoost recipe creation failed")

              # 2. Define Parsnip Model Spec with Tunable Parameters
              # Using parameters similar to UI defaults but marking some for tuning
              xgb_spec <- parsnip::boost_tree(
                mode = "regression",
                engine = "xgboost",
                mtry = tune::tune(), # Tune mtry
                trees = 1000, # Keep trees high, let early stopping handle it (or tune)
                min_n = tune::tune(), # Tune min_n
                tree_depth = tune::tune(), # Tune tree_depth
                learn_rate = tune::tune(), # Tune learn_rate
                loss_reduction = tune::tune() # Tune gamma (loss_reduction)
                # subsample = config$subsample # Could tune this too
              ) %>%
                parsnip::set_engine("xgboost", objective = "reg:squarederror")

              # 3. Create Workflow
              xgb_wf <- workflows::workflow() %>%
                workflows::add_recipe(unprepared_recipe) %>%
                workflows::add_model(xgb_spec)

              # 4. Define Resampling Strategy (Time Series CV)
              # Using timetk version for convenience as it's already imported
              # Adjust initial, assess, skip based on data size/needs
              initial_periods <- max(floor(nrow(train_df) * 0.7), 20) # Start with 70% or 20 periods
              assess_periods <- max(floor(nrow(train_df) * 0.1), 5) # Assess on 10% or 5 periods
              skip_periods <- max(floor(assess_periods * 0.5), 1) # Skip half the assessment period
              
              if(initial_periods + assess_periods > nrow(train_df)) {
                 warning("Not enough data for default time series CV splits. Adjusting...")
                 initial_periods <- floor(nrow(train_df) * 0.6)
                 assess_periods <- floor(nrow(train_df) * 0.2)
                 skip_periods <- floor(assess_periods * 0.5)
                 req(initial_periods > 0, assess_periods > 0, skip_periods >= 0)
              }

              ts_cv_splits <- timetk::time_series_cv(
                data = train_df, # Use training data for CV
                date_var = ds,
                initial = paste(initial_periods, freq_str), # e.g., "90 day" or "12 week"
                assess = paste(assess_periods, freq_str),
                skip = paste(skip_periods, freq_str),
                cumulative = FALSE, # Sliding window usually preferred
                slice_limit = 5 # Limit number of CV slices for speed
              )
              message(paste("Created", nrow(ts_cv_splits), "time series CV splits."))

              # 5. Define Parameter Grid
              # Use dials to define ranges and create a grid
              xgb_params <- dials::parameters(xgb_spec) # Get tunable params from spec
              # Define ranges (adjust as needed) using update() on the parameter set
              # Example ranges, adjust based on features and expected values
              num_features <- tryCatch({ # Add error handling for baking recipe just for feature count
                 ncol(recipes::bake(recipes::prep(unprepared_recipe), new_data = NULL, has_role("predictor")))
              }, error = function(e) {
                 warning("Could not bake recipe to determine feature count for mtry range. Using default range.")
                 10 # Default fallback if baking fails
              })
              
              xgb_params <- update(
                xgb_params,
                mtry = dials::mtry(range = c(1L, max(1L, floor(num_features * 0.8)))), # Tune up to 80% of features
                min_n = dials::min_n(range = c(2L, 20L)),
                tree_depth = dials::tree_depth(range = c(3L, 10L)),
                learn_rate = dials::learn_rate(range = c(-2.5, -1.0)), # Log10 scale: ~0.003 to 0.1
                loss_reduction = dials::loss_reduction(range = c(-1.5, 1.5)) # Log10 scale: ~0.03 to ~30
              )

              # Create grid (e.g., 10 candidates)
              set.seed(123) # for reproducibility
              xgb_grid <- dials::grid_latin_hypercube(
                xgb_params,
                size = 10 # Number of parameter combinations to try
              )
              message(paste("Created tuning grid with", nrow(xgb_grid), "candidates."))

              # 6. Run Tuning
              shiny::incProgress(0.2, detail = "Tuning XGBoost Hyperparameters...")
              message("Starting hyperparameter tuning (tune_grid)...")
              tune_results <- tune::tune_grid(
                object = xgb_wf,
                resamples = ts_cv_splits,
                grid = xgb_grid,
                metrics = yardstick::metric_set(yardstick::rmse), # Optimize for RMSE
                control = tune::control_grid(save_pred = FALSE, # Don't save predictions
                                             verbose = TRUE, # Show progress
                                             allow_par = FALSE) # Run sequentially for safety in Shiny
              )
              message("Hyperparameter tuning finished.")
              shiny::incProgress(0.6, detail = "Finalizing best XGBoost model...")

              # 7. Select Best Parameters
              best_params <- tune::select_best(tune_results, metric = "rmse")
              message("Best hyperparameters selected:")
              print(best_params)

              # 8. Finalize Workflow
              final_xgb_wf <- tune::finalize_workflow(xgb_wf, best_params)

              # 9. Fit Final Model on Full Training Data
              message("Fitting final XGBoost model on full training data...")
              final_fit <- parsnip::fit(final_xgb_wf, data = train_df)
              message("Final model fitted.")

              # 10. Extract Fitted Model and PREPARED Recipe
              fitted_xgb_model <- workflows::extract_fit_parsnip(final_fit)
              prep_recipe_from_fit <- workflows::extract_recipe(final_fit, estimated = TRUE) # Get PREPPED recipe

              # Store tuned parameters for summary
              model_summary_entry$tuned_params <- best_params
              # Store original config as well? Or replace? Let's add tuned_params.
              model_summary_entry$config <- config # Keep original config for reference if needed

              # 11. Forecast using fitted model and prepared recipe
              shiny::incProgress(0.8, detail = "Forecasting with best XGBoost...")
              message("Calling forecast_xgboost with tuned model and prepared recipe...")
              # Pass the extracted parsnip model object and the PREPARED recipe
              forecast_tibble <- forecast_xgboost(
                 model = fitted_xgb_model$fit, # Extract the underlying xgb.Booster
                 prep_recipe = prep_recipe_from_fit, # Pass the PREPARED recipe
                 full_df = full_aggregated_df,
                 train_end_date = last_train_date,
                 total_periods_needed = total_periods_needed,
                 freq = freq_str
               )
              req(forecast_tibble, "XGBoost forecast failed after tuning.")
              message("XGBoost forecast generated successfully after tuning.")

              # 12. Get Fitted Values by manually baking the prepared recipe and predicting with the extracted model
              message("Getting fitted values from tuned XGBoost model by baking train_df...")
              fitted_values <- NULL # Initialize
              tryCatch({
                # Bake the *prepared* recipe using the original training data
                # Bake EVERYTHING to ensure 'y' is available for step_lag
                train_baked_everything_df <- recipes::bake(prep_recipe_from_fit, new_data = train_df, everything())
                
                # Now select only the predictors needed by the model
                model_features <- fitted_xgb_model$fit$feature_names # Get predictor names from the fitted model
                
                # Check if all required predictors exist in the baked data
                missing_train_cols <- setdiff(model_features, names(train_baked_everything_df))
                if (length(missing_train_cols) > 0) {
                  stop(paste("Training data missing required model features after baking:", paste(missing_train_cols, collapse=", ")))
                }
                
                # Select only the required predictor columns and convert to matrix
                train_matrix <- as.matrix(train_baked_everything_df[, model_features, drop=FALSE])
                
                # Predict using the extracted xgb.Booster model
                fitted_values <- predict(fitted_xgb_model$fit, newdata = train_matrix)
                
                req(fitted_values, "Prediction for fitted values returned NULL.")
                if(length(fitted_values) != nrow(train_df)) {
                   stop(paste("Fitted values length", length(fitted_values), "does not match train_df rows", nrow(train_df)))
                }
                 message("Fitted values obtained successfully using extracted model.")
              }, error = function(e_fit) {
                 warning(paste("Failed to get fitted values from tuned XGBoost model:", conditionMessage(e_fit)))
                 # Print error for debugging
                 print("--- Error during manual fitted values calculation ---")
                 print(e_fit)
                 print("--- End Error ---")
                 fitted_values <<- NULL # Ensure it's NULL on error
              })
              req(fitted_values, "Failed to calculate fitted values after tuning.") # Stop if calculation failed
              # --- End XGBoost Tuning Workflow ---
              } else {
                message("XGBoost: Hyperparameter tuning DISABLED. Using UI parameters.")
                # Original non-tuning workflow
                unprepared_recipe_xgb <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
                req(unprepared_recipe_xgb, "XGBoost recipe creation failed (tuning off).")
                
                message("Preparing recipe for XGBoost (tuning off)...")
                prep_recipe_xgb <- tryCatch({
                  recipes::prep(unprepared_recipe_xgb, training = train_df)
                }, error = function(e){
                  warning(paste("Failed to prepare recipe for XGBoost (tuning off):", conditionMessage(e)))
                  NULL
                })
                req(prep_recipe_xgb, "Recipe preparation failed for XGBoost (tuning off).")
                message("Recipe prepared for XGBoost (tuning off).")

                model_or_fcst_obj <- train_xgboost(prep_recipe_xgb, config) # config from UI
                req(model_or_fcst_obj, "XGBoost model training failed (tuning off).")
                
                forecast_tibble <- forecast_xgboost(model_or_fcst_obj, prep_recipe_xgb, full_aggregated_df, last_train_date, total_periods_needed, freq_str)
                req(forecast_tibble, "XGBoost forecast failed (tuning off).")
                
                # Get fitted values
                train_baked_df <- recipes::bake(prep_recipe_xgb, new_data = train_df, everything())
                model_features <- model_or_fcst_obj$feature_names
                missing_cols <- setdiff(model_features, names(train_baked_df))
                if (length(missing_cols) > 0) stop(paste("XGBoost (tuning off): Training data missing features:", paste(missing_cols, collapse=", ")))
                train_matrix <- as.matrix(train_baked_df[, model_features, drop=FALSE])
                fitted_values <- predict(model_or_fcst_obj, train_matrix)
                req(fitted_values, "XGBoost fitted values calculation failed (tuning off).")
                model_summary_entry$tuned_params <- NULL # Ensure no tuned params are stored
              }
            }   else if (model_name == "GAM") {
              config <- list(
                smooth_trend = model_config_reactives$gam_trend_type() == "smooth",
                use_season_y = model_config_reactives$gam_use_season_y(),
                use_season_w = model_config_reactives$gam_use_season_w()
                # Add future config items here (e.g., regressor names)
              ) # Extract GAM config
              model_summary_entry$config <- config
              model_or_fcst_obj <- train_gam(train_df, config, holidays_df = current_global_holidays)
              req(model_or_fcst_obj, "GAM training failed (returned NULL).")
              forecast_output <- forecast_gam(model_or_fcst_obj, train_df, total_periods_needed, freq_str, config, holidays_df = current_global_holidays)
              forecast_tibble <- forecast_output$forecast
              fitted_values <- forecast_output$fitted
              req(forecast_output, forecast_tibble, fitted_values, "GAM forecasting failed.")
              req(forecast_output)
              forecast_tibble <- forecast_output$forecast; fitted_values <- forecast_output$fitted
            }  else if (model_name == "RF") {
              config <- list(
                rf_num_trees = model_config_reactives$rf_num_trees(),
                rf_mtry = model_config_reactives$rf_mtry(), # Pass 0 for auto
                rf_min_node_size = model_config_reactives$rf_min_node_size()
              ) # Extract RF config
              model_summary_entry$config <- config
              # Get unprepared recipe
              unprepared_recipe_rf <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
              req(unprepared_recipe_rf, "Recipe creation failed for RF.")
              # Prepare the recipe using training data
              message("Preparing recipe for RF...")
              prep_recipe_rf <- tryCatch({
                 recipes::prep(unprepared_recipe_rf, training = train_df)
              }, error = function(e){
                 warning(paste("Failed to prepare recipe for RF:", conditionMessage(e)))
                 NULL
              })
              req(unprepared_recipe_rf, "Recipe creation failed for RF.")
              # NOTE: We no longer prepare the recipe here for tuning workflow

              # Get the enable_tuning reactive for RF
              enable_rf_tuning <- model_config_reactives$rf_enable_tuning()
              model_summary_entry$config <- config # Store original config
              model_summary_entry$tuning_enabled <- enable_rf_tuning # Store if tuning was run

              if (isTRUE(enable_rf_tuning)) {
                message("Random Forest: Hyperparameter tuning ENABLED.")
                # --- RF Tuning Workflow ---
                message("Setting up Random Forest tuning workflow...")
                # 2. Define Parsnip Model Spec with Tunable Parameters
              rf_spec <- parsnip::rand_forest(
                mode = "regression",
                engine = "ranger",
                mtry = tune::tune(), # Tune mtry
                trees = 500, # Keep trees fixed for now, could tune
                min_n = tune::tune() # Tune min_n
              ) %>%
                parsnip::set_engine("ranger", importance = "impurity", num.threads = 1) # Keep single thread

              # 3. Create Workflow
              rf_wf <- workflows::workflow() %>%
                workflows::add_recipe(unprepared_recipe_rf) %>% # Use unprepared recipe
                workflows::add_model(rf_spec)

              # 4. Define Resampling Strategy (Reuse from XGBoost)
              # Ensure ts_cv_splits is defined earlier in the observeEvent if needed
              # For now, assume it's available from XGBoost block if run together,
              # otherwise, recalculate it here if RF is run alone.
              # Let's recalculate for robustness if run alone:
              if (!exists("ts_cv_splits") || is.null(ts_cv_splits)) {
                 message("Recalculating ts_cv_splits for RF tuning.")
                 initial_periods <- max(floor(nrow(train_df) * 0.7), 20)
                 assess_periods <- max(floor(nrow(train_df) * 0.1), 5)
                 skip_periods <- max(floor(assess_periods * 0.5), 1)
                 if(initial_periods + assess_periods > nrow(train_df)) {
                    initial_periods <- floor(nrow(train_df) * 0.6); assess_periods <- floor(nrow(train_df) * 0.2); skip_periods <- floor(assess_periods * 0.5)
                 }
                 req(initial_periods > 0, assess_periods > 0, skip_periods >= 0)
                 ts_cv_splits <- timetk::time_series_cv(data = train_df, date_var = ds, initial = paste(initial_periods, freq_str), assess = paste(assess_periods, freq_str), skip = paste(skip_periods, freq_str), cumulative = FALSE, slice_limit = 5)
                 message(paste("Created", nrow(ts_cv_splits), "time series CV splits for RF."))
              }

              # 5. Define Parameter Grid
              rf_params <- dials::parameters(rf_spec)
              num_features_rf <- tryCatch({ ncol(recipes::bake(recipes::prep(unprepared_recipe_rf), new_data = NULL, has_role("predictor"))) }, error = function(e) { 10 })
              rf_params <- update(
                rf_params,
                mtry = dials::mtry(range = c(1L, max(1L, floor(num_features_rf * 0.8)))),
                min_n = dials::min_n(range = c(2L, 20L))
              )
              set.seed(456) # Use a different seed
              rf_grid <- dials::grid_latin_hypercube(rf_params, size = 10)
              message(paste("Created RF tuning grid with", nrow(rf_grid), "candidates."))

              # 6. Run Tuning
              shiny::incProgress(0.2, detail = "Tuning Random Forest Hyperparameters...")
              message("Starting RF hyperparameter tuning (tune_grid)...")
              rf_tune_results <- tune::tune_grid(
                object = rf_wf,
                resamples = ts_cv_splits,
                grid = rf_grid,
                metrics = yardstick::metric_set(yardstick::rmse),
                control = tune::control_grid(save_pred = FALSE, verbose = TRUE, allow_par = FALSE)
              )
              message("RF Hyperparameter tuning finished.")
              shiny::incProgress(0.6, detail = "Finalizing best Random Forest model...")

              # 7. Select Best Parameters
              best_rf_params <- tune::select_best(rf_tune_results, metric = "rmse")
              message("Best RF hyperparameters selected:")
              print(best_rf_params)

              # 8. Finalize Workflow
              final_rf_wf <- tune::finalize_workflow(rf_wf, best_rf_params)

              # 9. Fit Final Model
              message("Fitting final Random Forest model...")
              final_rf_fit <- parsnip::fit(final_rf_wf, data = train_df)
              message("Final RF model fitted.")

              # 10. Extract Model and Prepared Recipe
              fitted_rf_model <- workflows::extract_fit_parsnip(final_rf_fit)
              prep_recipe_rf_from_fit <- workflows::extract_recipe(final_rf_fit, estimated = TRUE)

              # Store tuned parameters
              model_summary_entry$tuned_params <- best_rf_params
              model_summary_entry$config <- config # Keep original config

              # 11. Forecast using fitted model and prepared recipe
              shiny::incProgress(0.8, detail = "Forecasting with best Random Forest...")
              message("Calling forecast_rf with tuned model and prepared recipe...")
              # Pass the extracted ranger model and the PREPARED recipe
              forecast_output <- forecast_rf(
                 model = fitted_rf_model$fit, # Extract the underlying ranger model
                 prep_recipe = prep_recipe_rf_from_fit, # Pass the PREPARED recipe
                 full_df = full_aggregated_df,
                 train_df = train_df, # Pass train_df again for fitted value calculation inside forecast_rf
                 train_end_date = last_train_date,
                 total_periods_needed = total_periods_needed,
                 freq_str = freq_str
               )
              req(forecast_output, "RF forecast_rf function returned NULL after tuning.")
              forecast_tibble <- forecast_output$forecast
              req(forecast_tibble, "RF forecast data frame is NULL after tuning.")
              fitted_values <- forecast_output$fitted # Get fitted values from forecast_rf return
              req(fitted_values, "RF fitted values are NULL after tuning.")
              message("RF forecast and fitted values generated successfully after tuning.")
              # --- End RF Tuning Workflow ---
              } else {
                message("Random Forest: Hyperparameter tuning DISABLED. Using UI parameters.")
                # Original non-tuning workflow for RF
                # Prepare the recipe using training data (already got unprepared_recipe_rf)
                message("Preparing recipe for RF (tuning off)...")
                prep_recipe_rf <- tryCatch({
                   recipes::prep(unprepared_recipe_rf, training = train_df)
                }, error = function(e){
                   warning(paste("Failed to prepare recipe for RF (tuning off):", conditionMessage(e)))
                   NULL
                })
                req(prep_recipe_rf, "Recipe preparation failed for RF (tuning off).")
                message("Recipe prepared for RF (tuning off).")

                model_or_fcst_obj <- train_rf(prep_recipe_rf, config) # config from UI
                req(model_or_fcst_obj, "Random Forest training failed (tuning off).")
                
                forecast_output <- forecast_rf(model_or_fcst_obj, prep_recipe_rf, full_aggregated_df, train_df, last_train_date, total_periods_needed, freq_str)
                req(forecast_output, "RF forecast_rf function returned NULL (tuning off).")
                forecast_tibble <- forecast_output$forecast
                req(forecast_tibble, "RF forecast data frame is NULL (tuning off).")
                fitted_values <- forecast_output$fitted
                req(fitted_values, "RF fitted values are NULL (tuning off).")
                model_summary_entry$tuned_params <- NULL # Ensure no tuned params are stored
              }
              # metrics_list <- list()
              # # Train Metrics (check logic remains the same)
              # train_actual <- train_df$y
              # # --- Keep Detailed Check Here ---
              # if(!is.null(fitted_values)) { # Checks moved here for clarity
              #   message(paste("  Length fitted:", length(fitted_values), "vs Actual:", length(train_actual)))
              #   message(paste("  Length match?:", length(fitted_values) == length(train_actual)))
              #   message(paste("  Any NAs in fitted?:", anyNA(fitted_values)))
              #   message(paste("  All !is.na(fitted)?:", all(!is.na(fitted_values))))
              # }


            } else if (model_name == "NNETAR") {
              config_nnetar <- list(
                nnetar_p = model_config_reactives$nnetar_p(),
                nnetar_P = model_config_reactives$nnetar_P(),
                nnetar_size_method = model_config_reactives$nnetar_size_method(),
                nnetar_size_manual = model_config_reactives$nnetar_size_manual(),
                nnetar_repeats = model_config_reactives$nnetar_repeats(),
                nnetar_lambda_auto = model_config_reactives$nnetar_lambda_auto(),
                nnetar_lambda_manual = model_config_reactives$nnetar_lambda_manual()
              )
              model_summary_entry$config <- config_nnetar
              
              # Pass agg_level to train_nnetar
              model_obj_nnetar <- train_nnetar(train_df, config_nnetar, agg_level) 
              req(model_obj_nnetar, "NNETAR training failed (returned NULL).")
              
              # Store frequency used if available as attribute
              if (!is.null(attr(model_obj_nnetar, "frequency_used"))) {
                model_summary_entry$frequency_used <- attr(model_obj_nnetar, "frequency_used")
              }
              # Store model method string if available (nnetar objects print this)
              model_summary_entry$fitted_method <- capture.output(print(model_obj_nnetar))[1]


              forecast_output_nnetar <- forecast_nnetar(model_obj_nnetar, total_periods_needed, last_train_date, freq_str)
              req(forecast_output_nnetar, "NNETAR forecast_nnetar function returned NULL.")
              
              forecast_tibble <- forecast_output_nnetar$forecast
              req(forecast_tibble, "NNETAR forecast data frame is NULL.")
              
              fitted_values <- forecast_output_nnetar$fitted
              # NNETAR fitted values can sometimes be shorter if lags are involved, or have NAs at the start.
              # req(fitted_values, "NNETAR fitted values are NULL.") 
              # A more robust check for fitted_values length will be done in the metrics calculation part.
              if(is.null(fitted_values)){
                  message("NNETAR: Fitted values are NULL. Metrics on training data will be skipped.")
              } else if(length(fitted_values) != nrow(train_df)) {
                  message(paste0("NNETAR: Fitted values length (", length(fitted_values), ") does not match train_df rows (", nrow(train_df), "). Check for NAs or lag effects. Metrics on training data might be affected."))
              }
            }
            # --- End Model Logic ---

            # Store results if successful
            r$forecast_list[[model_name]] <- forecast_tibble
            r$fitted_list[[model_name]] <- fitted_values
            model_summary_entry$success <- TRUE # Mark as successful
            temp_summary_list[[model_name]] <- model_summary_entry # Add to temp list
            successful_models <- c(successful_models, model_name) # Add to success list
            model_run_success <- TRUE
            message(paste("--- Finished Model:", model_name, "Successfully ---"))

          }, error = function(e){ # Catch error for INDIVIDUAL model
              user_friendly_message <- paste0(
                "Error during ", model_name, " model processing. ",
                "Please check this model's configuration and input data suitability. ",
                "Specific error: ", conditionMessage(e)
              )
              warning(paste("Error running model", model_name, ":", conditionMessage(e))) # Keep for server logs
              shiny::showNotification(user_friendly_message, type = "warning", duration = 10)

              # Reset results for THIS model specifically if needed, though current logic
              # of not assigning to r$forecast_list etc. for this model is correct.
              # model_success <<- FALSE # This assignment might not be needed if not used elsewhere before loop ends
              model_summary_entry$success <- FALSE
              model_summary_entry$error <- conditionMessage(e)
              temp_summary_list[[model_name]] <- model_summary_entry # Store error info

              # Print error object to console for detailed debugging
              print(paste("ERROR during forecast execution for model:", model_name, "at", Sys.time()))
              print("--- Full Error Object (Individual Model) ---")
              print(e)
              print("--- End Error Object (Individual Model) ---")


          }) # End inner tryCatch

          # Increment progress bar after each model attempt
          shiny::incProgress(amount = progress_inc)
          if(model_run_success) {
            shiny::showNotification(paste(model_name, "forecast complete."), type = "message", duration = 5)
          } else {
            # Error notification already shown by tryCatch
          }

        } # --- End For Loop ---
      req(length(successful_models) > 0, "All selected models failed to produce forecasts.")
      message(paste("Models run successfully:", paste(successful_models, collapse=", ")))
      # --- Metrics Calculation (NEW - Loop through successful models) ---
      message("Calculating metrics for successful models...")
      all_metrics_list <- list() # Initialize list to store metrics tables
      # Get actuals once
      train_actual <- train_df$y
      test_actual <- if (nrow(test_df %||% data.frame()) > 0) test_df$y else NULL
      n_test_periods <- length(test_actual %||% numeric(0))

      for (model_name in successful_models) {
        message(paste("Calculating metrics for:", model_name))
        fitted_values <- r$fitted_list[[model_name]]

        # --- DEBUG Metrics Check (ADD) ---
        # --- DEBUG Metrics Check (Keep basic info) ---
        message(paste0("DEBUG Metrics: Checking model: ", model_name))
        message(paste0("DEBUG Metrics: Length of fitted_values: ", length(fitted_values)))
        message(paste0("DEBUG Metrics: Length of train_actual: ", length(train_actual)))
        message(paste0("DEBUG Metrics: Any NAs in fitted_values? ", anyNA(fitted_values)))
        message(paste0("DEBUG Metrics: Class of fitted_values: ", class(fitted_values)))
        
        # --- ADD Specific Logging for ARIMA/ETS Fitted Values ---
        if (model_name %in% c("ARIMA", "ETS")) {
          message(paste0("  Detailed check for ", model_name, " fitted values:"))
          message(paste0("    str(): ", utils::capture.output(utils::str(fitted_values))))
          message(paste0("    summary(): ", paste(utils::capture.output(summary(fitted_values)), collapse=" ")))
        }
        # --- END Specific Logging ---
        
        forecast_tibble <- r$forecast_list[[model_name]]
        model_metrics <- list() # Store train/test for THIS model

        # Calculate Train Metrics
        if (!is.null(fitted_values) && length(fitted_values) == length(train_actual) && !anyNA(fitted_values)) {
          train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
          if (!is.null(train_metrics_tbl)) {
            model_metrics$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
          } else { message(paste(" WARN: Training metrics calculation failed for", model_name))}
        } else {
          message(paste(" INFO: Training metrics skipped for", model_name, "(NULL, length mismatch, or NAs)"))
        }

        # Calculate Test Metrics
        if (n_test_periods > 0 && !is.null(forecast_tibble)) {
          # Align forecast with test actuals based on date 'ds'
          test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)
          if (nrow(test_pred_df) == n_test_periods) {
            test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
            test_pred <- test_pred_ordered_df$yhat
            if(all(!is.na(test_pred))){
              test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
              if(!is.null(test_metrics_tbl)){
                model_metrics$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)
              } else { message(paste(" WARN: Test metrics calculation failed for", model_name)) }
            } else { message(paste(" INFO: Test metrics skipped for", model_name, "(NAs in predictions)")) }
          } else {
            message(" WARN: Could not align test predictions for", model_name, "(check forecast dates/length). Skipping.")
          }
        } # End if test data exists

        # Add this model's metrics (if any) to the overall list
        if(length(model_metrics) > 0) {
          all_metrics_list <- c(all_metrics_list, model_metrics)
        }
      } # End loop through successful models

      # Combine metrics from all models
      if (length(all_metrics_list) > 0) {
        r$metrics_summary <- dplyr::bind_rows(all_metrics_list) %>%
          dplyr::select(Model, DataSet, .metric, .estimate) # Ensure order
        message("Metrics summary table created.")
      } else {
        r$metrics_summary <- NULL # Ensure it's NULL if no metrics calculated
        message("No metrics were calculated for any successful model.")
      }
      # --- End Metrics Calculation --

      # --- Store Combined Model Summaries ---
      r$run_models_summary <- temp_summary_list # Update main reactive value

      # --- ADD DEBUG: Print final summary structure ---
      # --- DEBUG Plotting Check (ADD) ---
      message("DEBUG Plotting: Checking reactive values before incrementing r$run_id")
      message("DEBUG Plotting: Names in r$forecast_list:")
      print(names(r$forecast_list))
      # message("DEBUG Plotting: Structure of r$forecast_list[["ARIMA"]]:")
      # print(str(r$forecast_list[["ARIMA"]]))
      # message("DEBUG Plotting: Structure of r$forecast_list[["ETS"]]:")
      # print(str(r$forecast_list[["ETS"]]))
      # message("DEBUG Plotting: Structure of r$fitted_list[["ARIMA"]]:")
      # print(str(r$fitted_list[["ARIMA"]]))
      # message("DEBUG Plotting: Structure of r$fitted_list[["ETS"]]:")
      message("--- Final r$run_models_summary structure ---")
      print(str(r$run_models_summary))
      message("--- End final summary structure ---")
      # --- END DEBUG ---





        # --- Update Trigger for Plot ---
        # Increment run_id only AFTER loop finishes to trigger plot update once
      if(length(r$forecast_list) > 0) {
        message(paste("DEBUG: Models in r$forecast_list before plot update:", paste(names(r$forecast_list), collapse=", "))) # Log names before update
        r$run_id <- r$run_id + 1
        message("Finished all selected models.")
      }
        # --- End Update Trigger ---


        # --- Metrics Calculation (Phase 3 - Requires Loop & List Storage) ---
        # message("Calculating all metrics...")
        # combined_metrics_table <- ... logic to loop through fitted_list/forecast_list ...
        # r$metrics_summary <- combined_metrics_table
        # --- End Metrics ---
      }, error = function(e) { # Outer catch handler for the entire forecast process
        # This catches errors outside individual model loops (e.g., initial data prep, metrics combination if not caught)
        detailed_error_msg <- conditionMessage(e)
        user_facing_error_msg <- paste(
          "An unexpected error occurred during the overall forecast process. ",
          "Please review your data and general settings. ",
          "Details: ", detailed_error_msg
        )
        message(paste("ERROR caught in outer tryCatch for forecast process:", detailed_error_msg)) # Server log
        print("--- Outer tryCatch Error Object (Forecast Process) ---")
        print(e)
        print("--- End Outer tryCatch Error Object ---")

        # Reset all potentially affected reactive values to a clean state
        r$forecast_list <- list()
        r$fitted_list <- list()
        r$metrics_summary <- NULL
        r$run_models_summary <- list() # Contains error info if models ran
        # r$run_id <- r$run_id + 1 # Increment to ensure UI updates, even if it's to show no results or errors
        
        shiny::showNotification(user_facing_error_msg, type = "error", duration = 15)
      }) # End outer tryCatch

    }) # End withProgress (outer one)


    # --- Reset File Inputs (as before) ---
    # You could potentially reset the main data upload too if desired:
    # shinyjs::reset("data_input_1-fileUpload")
    shinyjs::reset("model_config_1-prophet_holidays_file")
    shinyjs::reset("model_config_1-prophet_regressors_file")
    # --- End Reset ---

    output$downloadForecastData <- downloadHandler(
      filename = function() {
        paste0("forecast_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        message("Download button triggered.") # Log download start
        # --- Prepare Data for Download ---
        req(r$run_id > 0, r$forecast_list)
        validate(need(length(r$forecast_list) > 0, "No forecast results generated yet."))

        forecasts_to_download <- r$forecast_list
        processed_list <- list() # Initialize empty list

        # --- Process each model's forecast data ---
        for(model_name in names(forecasts_to_download)) {
          df <- forecasts_to_download[[model_name]]
          message(paste("Processing", model_name, "for download..."))

          if (!is.null(df) && is.data.frame(df) && nrow(df)>0 && "ds" %in% names(df) && "yhat" %in% names(df)) {
            # Define standard columns potentially available
            cols_to_select <- c(
              "ds", "yhat",
              "yhat_lower_95", "yhat_upper_95",
              "yhat_lower_80", "yhat_upper_80",
              "yhat_lower", "yhat_upper" # Prophet defaults
            )
            # Select only the columns that actually exist in this df
            existing_cols <- intersect(cols_to_select, names(df))
            df_selected <- df %>% dplyr::select(all_of(existing_cols))

            # Rename columns (except 'ds') to prefix with model name
            df_renamed <- df_selected %>%
              dplyr::rename_with(~paste0(model_name, "_", .), .cols = -ds)

            processed_list[[model_name]] <- df_renamed # Add to list
          } else {
            message(paste("Skipping invalid/empty forecast data for", model_name))
          }
        } # End for loop

        validate(need(length(processed_list) > 0, "No valid forecast dataframes found to download."))

        # Combine all dataframes using full_join on 'ds'
        message("Joining dataframes for download...")
        combined_df <- purrr::reduce(processed_list,
                                     dplyr::full_join,
                                     by = "ds") %>%
          dplyr::arrange(ds) # Ensure sorted by date

        # --- Write to CSV ---
        message("Writing combined forecast data to CSV for download.")
        utils::write.csv(combined_df, file, row.names = FALSE, na = "")
        message("CSV writing complete.")
      },
      contentType = "text/csv"
    )




  output$global_holidays_preview <- renderPrint({
    head(r$global_holidays_data())
  })

  # --- Save Session Logic ---
  observeEvent(input$save_session_button, {
    shiny::showModal(modalDialog(
      title = "Save Session",
      textInput("session_filename_input", "Enter filename for session (e.g., my_forecast_session):", 
                value = paste0("forecast_session_", format(Sys.time(), "%Y%m%d_%H%M%S"))),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("trigger_session_save_download", "Save to RDS")
      ),
      easyClose = TRUE
    ))
  })

  output$trigger_session_save_download <- downloadHandler(
    filename = function() {
      req(input$session_filename_input)
      # Sanitize filename to prevent invalid characters
      sanitized_name <- gsub("[^a-zA-Z0-9_\\-\\.]", "_", input$session_filename_input)
      if (!grepl("\\.rds$", sanitized_name, ignore.case = TRUE)) {
        sanitized_name <- paste0(sanitized_name, ".rds")
      }
      sanitized_name
    },
    content = function(file) {
      # Gather all data to save
      # Main reactive values 'r'
      r_values_to_save <- reactiveValuesToList(r)
      
      # Data Input module state
      di_state_values <- list(
        selected_date_col = if (is.function(data_input_reactives$reactive_selected_date_col)) data_input_reactives$reactive_selected_date_col() else NULL,
        selected_value_col = if (is.function(data_input_reactives$reactive_selected_value_col)) data_input_reactives$reactive_selected_value_col() else NULL,
        selected_format = if (is.function(data_input_reactives$reactive_selected_format)) data_input_reactives$reactive_selected_format() else NULL,
        data_input_1_fileUpload_name = if (is.function(data_input_reactives$raw_data_name)) data_input_reactives$raw_data_name() else NULL # Store original filename
      )

      # Preprocessing module state
      pp_state_values <- list(
        aggregation_level = if (is.function(preprocess_reactives$reactive_agg_level)) preprocess_reactives$reactive_agg_level() else NULL,
        aggregation_function = if (is.function(preprocess_reactives$reactive_agg_func)) preprocess_reactives$reactive_agg_func() else NULL,
        train_test_split_ratio = if (is.function(preprocess_reactives$reactive_train_test_split)) preprocess_reactives$reactive_train_test_split() else NULL,
        imputation_method = if (is.function(preprocess_reactives$reactive_imputation_method)) preprocess_reactives$reactive_imputation_method() else NULL,
        transformation_method = if (is.function(preprocess_reactives$reactive_transformation_method)) preprocess_reactives$reactive_transformation_method() else NULL
      )
      
      # Model configurations - exhaustive list of all inputs
      mc_state_values <- list(
        active_tab = if (is.function(model_config_reactives$active_tab)) model_config_reactives$active_tab() else NULL,
        forecast_horizon = if (is.function(model_config_reactives$forecast_horizon)) model_config_reactives$forecast_horizon() else NULL,
        # ARIMA
        use_arima = if (is.function(model_config_reactives$use_arima)) model_config_reactives$use_arima() else NULL,
        arima_auto = if (is.function(model_config_reactives$arima_auto)) model_config_reactives$arima_auto() else NULL, 
        arima_p = if (is.function(model_config_reactives$arima_p)) model_config_reactives$arima_p() else NULL, 
        arima_d = if (is.function(model_config_reactives$arima_d)) model_config_reactives$arima_d() else NULL, 
        arima_q = if (is.function(model_config_reactives$arima_q)) model_config_reactives$arima_q() else NULL,
        arima_seasonal = if (is.function(model_config_reactives$arima_seasonal)) model_config_reactives$arima_seasonal() else NULL, 
        arima_P = if (is.function(model_config_reactives$arima_P)) model_config_reactives$arima_P() else NULL, 
        arima_D = if (is.function(model_config_reactives$arima_D)) model_config_reactives$arima_D() else NULL, 
        arima_Q = if (is.function(model_config_reactives$arima_Q)) model_config_reactives$arima_Q() else NULL, 
        arima_period = if (is.function(model_config_reactives$arima_period)) model_config_reactives$arima_period() else NULL,
        # ETS
        use_ets = if (is.function(model_config_reactives$use_ets)) model_config_reactives$use_ets() else NULL,
        ets_manual = if (is.function(model_config_reactives$ets_manual)) model_config_reactives$ets_manual() else NULL, 
        ets_e = if (is.function(model_config_reactives$ets_e)) model_config_reactives$ets_e() else NULL, 
        ets_t = if (is.function(model_config_reactives$ets_t)) model_config_reactives$ets_t() else NULL, 
        ets_s = if (is.function(model_config_reactives$ets_s)) model_config_reactives$ets_s() else NULL, 
        ets_damped_str = if (is.function(model_config_reactives$ets_damped_str)) model_config_reactives$ets_damped_str() else NULL,
        # TBATS
        use_tbats = (if (is.function(model_config_reactives$use_tbats)) model_config_reactives$use_tbats() else NULL) %||% FALSE,
        # Prophet
        use_prophet = if (is.function(model_config_reactives$use_prophet)) model_config_reactives$use_prophet() else NULL,
        prophet_growth = if (is.function(model_config_reactives$prophet_growth)) model_config_reactives$prophet_growth() else NULL, 
        prophet_yearly = if (is.function(model_config_reactives$prophet_yearly)) model_config_reactives$prophet_yearly() else NULL, 
        prophet_weekly = if (is.function(model_config_reactives$prophet_weekly)) model_config_reactives$prophet_weekly() else NULL, 
        prophet_daily = if (is.function(model_config_reactives$prophet_daily)) model_config_reactives$prophet_daily() else NULL,
        prophet_changepoint_scale = if (is.function(model_config_reactives$prophet_changepoint_scale)) model_config_reactives$prophet_changepoint_scale() else NULL, 
        prophet_capacity = if (is.function(model_config_reactives$prophet_capacity)) model_config_reactives$prophet_capacity() else NULL,
        # XGBoost
        use_xgboost = if (is.function(model_config_reactives$use_xgboost)) model_config_reactives$use_xgboost() else NULL,
        xgb_enable_tuning = if (is.function(model_config_reactives$xgb_enable_tuning)) model_config_reactives$xgb_enable_tuning() else NULL,
        xgb_nrounds = if (is.function(model_config_reactives$xgb_nrounds)) model_config_reactives$xgb_nrounds() else NULL, 
        xgb_eta = if (is.function(model_config_reactives$xgb_eta)) model_config_reactives$xgb_eta() else NULL, 
        xgb_max_depth = if (is.function(model_config_reactives$xgb_max_depth)) model_config_reactives$xgb_max_depth() else NULL,
        xgb_subsample = if (is.function(model_config_reactives$xgb_subsample)) model_config_reactives$xgb_subsample() else NULL, 
        xgb_colsample = if (is.function(model_config_reactives$xgb_colsample)) model_config_reactives$xgb_colsample() else NULL, 
        xgb_gamma = if (is.function(model_config_reactives$xgb_gamma)) model_config_reactives$xgb_gamma() else NULL,
        # GAM
        use_gam = if (is.function(model_config_reactives$use_gam)) model_config_reactives$use_gam() else NULL,
        gam_trend_type = if (is.function(model_config_reactives$gam_trend_type)) model_config_reactives$gam_trend_type() else NULL, 
        gam_use_season_y = if (is.function(model_config_reactives$gam_use_season_y)) model_config_reactives$gam_use_season_y() else NULL, 
        gam_use_season_w = if (is.function(model_config_reactives$gam_use_season_w)) model_config_reactives$gam_use_season_w() else NULL,
        # RF
        use_rf = if (is.function(model_config_reactives$use_rf)) model_config_reactives$use_rf() else NULL,
        rf_enable_tuning = if (is.function(model_config_reactives$rf_enable_tuning)) model_config_reactives$rf_enable_tuning() else NULL,
        rf_num_trees = if (is.function(model_config_reactives$rf_num_trees)) model_config_reactives$rf_num_trees() else NULL, 
        rf_mtry = if (is.function(model_config_reactives$rf_mtry)) model_config_reactives$rf_mtry() else NULL, 
        rf_min_node_size = if (is.function(model_config_reactives$rf_min_node_size)) model_config_reactives$rf_min_node_size() else NULL
      )
      
      # Include original filename of global holidays file, if it was uploaded
      global_holidays_file_name_to_save <- NULL
      if (!is.null(input$global_holidays_file$name) && nzchar(input$global_holidays_file$name)) {
        global_holidays_file_name_to_save <- input$global_holidays_file$name
      }


      session_state_to_save <- list(
        timestamp = Sys.time(),
        app_version = utils::packageVersion("forecastApp"),
        r_values = r_values_to_save,
        data_input_state_values = di_state_values,
        preprocess_state_values = pp_state_values,
        model_config_state_values = mc_state_values,
        global_holidays_file_name = global_holidays_file_name_to_save # Save original filename
      )
      
      notification_id <- shiny::showNotification("Saving session... Please wait.", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(notification_id), add = TRUE)

      tryCatch({
        saveRDS(session_state_to_save, file = file)
        shiny::removeModal()
        shiny::showNotification(paste("Session saved to", basename(file)), type = "success", duration = 5)
      }, error = function(e_save) {
        shiny::showNotification(paste("Error saving session:", e_save$message), type = "error", duration = 10)
      })
    },
    contentType = "application/octet-stream"
  )
  # --- End Save Session Logic ---

  # --- Load Session Logic ---
  observeEvent(input$load_session_button, {
    shiny::showModal(modalDialog(
      title = "Load Session",
      fileInput("load_session_file_input_modal", "Upload Session File (.rds)",
                accept = c(".rds"),
                placeholder = "No file selected"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_load_session_button", "Load Session")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_load_session_button, {
    req(input$load_session_file_input_modal)
    
    show_loading_notification <- shiny::showNotification("Loading session... Please wait.", duration = NULL, type = "message", id = "loading_session_notif")

    tryCatch({
      loaded_state <- readRDS(input$load_session_file_input_modal$datapath)
      
      # Restore r reactiveValues
      if (!is.null(loaded_state$r_values)) {
        for (name in names(loaded_state$r_values)) {
          if (name == "global_holidays_data") {
            # Handle reactiveVal separately
            r$global_holidays_data(loaded_state$r_values[[name]])
          } else {
            r[[name]] <- loaded_state$r_values[[name]]
          }
        }
      }

      # Restore input values for mod_preprocess_controls
      if (!is.null(loaded_state$preprocess_state_values)) {
        psv <- loaded_state$preprocess_state_values
        updateSelectInput(session, "preprocess_controls_1-aggregationLevel", selected = psv$aggregation_level)
        updateSelectInput(session, "preprocess_controls_1-aggregationFunction", selected = psv$aggregation_function)
        updateSliderInput(session, "preprocess_controls_1-trainTestSplit", value = psv$train_test_split_ratio)
        updateSelectInput(session, "preprocess_controls_1-imputationMethod", selected = psv$imputation_method)
        updateSelectInput(session, "preprocess_controls_1-transformationMethod", selected = psv$transformation_method)
      }

      # Restore input values for mod_model_config
      if (!is.null(loaded_state$model_config_state_values)) {
        mcsv <- loaded_state$model_config_state_values
        updateNumericInput(session, "model_config_1-forecastHorizon", value = mcsv$forecast_horizon)
        
        # Use_model checkboxes
        updateCheckboxInput(session, "model_config_1-use_arima", value = mcsv$use_arima)
        updateCheckboxInput(session, "model_config_1-use_ets", value = mcsv$use_ets)
        updateCheckboxInput(session, "model_config_1-use_tbats", value = mcsv$use_tbats %||% FALSE) # Handle potential NULL
        updateCheckboxInput(session, "model_config_1-use_prophet", value = mcsv$use_prophet)
        updateCheckboxInput(session, "model_config_1-use_xgboost", value = mcsv$use_xgboost)
        updateCheckboxInput(session, "model_config_1-use_gam", value = mcsv$use_gam)
        updateCheckboxInput(session, "model_config_1-use_rf", value = mcsv$use_rf)

        # ARIMA params
        updateCheckboxInput(session, "model_config_1-arima_auto", value = mcsv$arima_auto)
        updateNumericInput(session, "model_config_1-arima_p", value = mcsv$arima_p)
        updateNumericInput(session, "model_config_1-arima_d", value = mcsv$arima_d)
        updateNumericInput(session, "model_config_1-arima_q", value = mcsv$arima_q)
        updateCheckboxInput(session, "model_config_1-arima_seasonal", value = mcsv$arima_seasonal)
        updateNumericInput(session, "model_config_1-arima_P", value = mcsv$arima_P)
        updateNumericInput(session, "model_config_1-arima_D", value = mcsv$arima_D)
        updateNumericInput(session, "model_config_1-arima_Q", value = mcsv$arima_Q)
        updateTextInput(session, "model_config_1-arima_period", value = mcsv$arima_period)

        # ETS params
        updateCheckboxInput(session, "model_config_1-ets_manual", value = mcsv$ets_manual)
        updateSelectInput(session, "model_config_1-ets_e", selected = mcsv$ets_e)
        updateSelectInput(session, "model_config_1-ets_t", selected = mcsv$ets_t)
        updateSelectInput(session, "model_config_1-ets_s", selected = mcsv$ets_s)
        updateSelectInput(session, "model_config_1-ets_damped_str", selected = mcsv$ets_damped_str)

        # TBATS params - none in UI to update beyond use_tbats

        # Prophet params
        updateSelectInput(session, "model_config_1-prophet_growth", selected = mcsv$prophet_growth)
        updateCheckboxInput(session, "model_config_1-prophet_yearly", value = mcsv$prophet_yearly)
        updateCheckboxInput(session, "model_config_1-prophet_weekly", value = mcsv$prophet_weekly)
        updateCheckboxInput(session, "model_config_1-prophet_daily", value = mcsv$prophet_daily)
        updateNumericInput(session, "model_config_1-prophet_changepoint_scale", value = mcsv$prophet_changepoint_scale)
        updateNumericInput(session, "model_config_1-prophet_capacity", value = mcsv$prophet_capacity)
        
        # XGBoost params
        updateCheckboxInput(session, "model_config_1-xgb_enable_tuning", value = mcsv$xgb_enable_tuning)
        updateNumericInput(session, "model_config_1-xgb_nrounds", value = mcsv$xgb_nrounds)
        updateNumericInput(session, "model_config_1-xgb_eta", value = mcsv$xgb_eta)
        updateNumericInput(session, "model_config_1-xgb_max_depth", value = mcsv$xgb_max_depth)
        updateNumericInput(session, "model_config_1-xgb_subsample", value = mcsv$xgb_subsample)
        updateNumericInput(session, "model_config_1-xgb_colsample", value = mcsv$xgb_colsample)
        updateNumericInput(session, "model_config_1-xgb_gamma", value = mcsv$xgb_gamma)
        
        # GAM params
        updateSelectInput(session, "model_config_1-gam_trend_type", selected = mcsv$gam_trend_type)
        updateCheckboxInput(session, "model_config_1-gam_use_season_y", value = mcsv$gam_use_season_y)
        updateCheckboxInput(session, "model_config_1-gam_use_season_w", value = mcsv$gam_use_season_w)

        # RF params
        updateCheckboxInput(session, "model_config_1-rf_enable_tuning", value = mcsv$rf_enable_tuning)
        updateNumericInput(session, "model_config_1-rf_num_trees", value = mcsv$rf_num_trees)
        updateNumericInput(session, "model_config_1-rf_mtry", value = mcsv$rf_mtry)
        updateNumericInput(session, "model_config_1-rf_min_node_size", value = mcsv$rf_min_node_size)
      }

      # Restore Data Input State (partially - file name, selectInputs might be tricky)
      if (!is.null(loaded_state$di_state_values)) {
        disv <- loaded_state$di_state_values
        # These might not update correctly if the choices aren't available (no data loaded yet)
        # This is a known limitation. The user will need to re-upload the data file.
        updateSelectInput(session, "data_input_1-dateCol", selected = disv$selected_date_col)
        updateSelectInput(session, "data_input_1-valueCol", selected = disv$selected_value_col)
        updateSelectInput(session, "data_input_1-dataFormat", selected = disv$selected_format)
        
        # Store the original data file name for display/reference
        if (!is.null(disv$data_input_1_fileUpload_name)) {
          r$loaded_session_data_file_name <- disv$data_input_1_fileUpload_name
        } else {
          r$loaded_session_data_file_name <- NULL
        }
      }
      
      # Restore Global Holidays File Name for display/reference
      if (!is.null(loaded_state$global_holidays_file_name)) {
        r$loaded_session_holiday_file_name <- loaded_state$global_holidays_file_name
      } else {
        r$loaded_session_holiday_file_name <- NULL
      }

      removeModal()
      shiny::removeNotification(id = "loading_session_notif")
      shiny::showNotification("Session loaded successfully! Please re-upload data and holiday files if they were part of the saved session.", type = "message", duration = 10)
      
      # Trigger an update for plots/tables if r$run_id was restored to a value > 0
      # This ensures that if a forecast was part of the saved state, it attempts to re-render.
      # The user will still need to ensure the base data is loaded for plots to be meaningful.
      if (!is.null(r$run_id) && r$run_id > 0) {
         # If you want to force a re-render of plots using existing r$forecast_list etc.
         # you might consider incrementing r$run_id or having a separate trigger.
         # For now, existing r$run_id will be used by eventReactives.
      }

    }, error = function(e) {
      shiny::removeNotification(id = "loading_session_notif")
      removeModal() # Also remove modal on error
      shiny::showNotification(paste("Error loading session:", e$message), type = "error", duration = 10)
      # Optionally, reset parts of the state if loading fails catastrophically
      # For example, reset r$run_id if it was partially loaded and might cause issues
      # r$run_id <- 0 
      # r$forecast_list <- list()
      # etc.
    })
  })
  # --- End Load Session Logic ---

  # --- Validation Section Server Logic ---
  output$cv_model_selector_ui <- renderUI({
    req(r$run_models_summary)
    model_names <- names(r$run_models_summary)
    req(length(model_names) > 0)
    checkboxGroupInput("cv_model_selection_input", "Select Models for CV:", 
                       choices = model_names, selected = model_names)
  })

  observeEvent(input$run_cv_button, {
    # Get Inputs
    selected_cv_models <- input$cv_model_selection_input
    initial_window_periods <- input$cv_initial_window
    horizon_periods <- input$cv_horizon
    skip_periods <- input$cv_skip
    is_cumulative <- input$cv_cumulative
    full_train_data <- preprocess_reactives$reactive_train_df() # Using the full training set for CV
    agg_level <- preprocess_reactives$reactive_agg_level()
    freq_str_cv <- if (agg_level == "Daily") "day" else "week"
    current_global_holidays_cv <- r$global_holidays_data() # Get global holidays

    # Validations
    req(full_train_data)
    validate(
      need(nrow(full_train_data) > (initial_window_periods + horizon_periods), 
           "Not enough data for the specified initial window and horizon. Please adjust CV parameters or use a larger dataset.")
    )
    validate(
      need(length(selected_cv_models) > 0, "Please select at least one model for Cross-Validation.")
    )

    # Time Series CV Splits
    ts_cv_splits <- timetk::time_series_cv(
      data = full_train_data,
      date_var = ds,
      initial = paste(initial_window_periods, freq_str_cv),
      assess = paste(horizon_periods, freq_str_cv),
      skip = paste(skip_periods, freq_str_cv),
      cumulative = is_cumulative,
      slice_limit = 10 # Default slice limit
    )
    
    message(paste("Number of CV slices generated:", nrow(ts_cv_splits)))
    validate(need(nrow(ts_cv_splits) > 0, "Time series CV split generation resulted in 0 slices. Adjust parameters (e.g., reduce initial window, horizon, or skip)."))

    # Progress Indicator & Results Storage
    all_cv_metrics <- list() # To store metrics from each fold for each model
    
    # Initialize reactive value for CV results if it doesn't exist
    if (is.null(r$cv_results_data)) {
      r$cv_results_data <- reactiveVal(NULL)
    }
    if (is.null(r$cv_raw_metrics_list)) {
      r$cv_raw_metrics_list <- reactiveVal(NULL) # For storing raw metrics for plotting
    }


    shiny::withProgress(message = 'Running Cross-Validation...', value = 0, {
      n_total_iterations <- length(selected_cv_models) * nrow(ts_cv_splits)
      progress_counter <- 0 # Renamed to avoid conflict with shiny::incProgress internal counter

      for (model_name_cv in selected_cv_models) {
        # shiny::incProgress(amount = 1/length(selected_cv_models), detail = paste("Cross-validating Model:", model_name_cv)) # This was outer loop progress
        
        model_config_original <- r$run_models_summary[[model_name_cv]]$config
        original_agg_level <- r$run_models_summary[[model_name_cv]]$aggregation_level # Should be same as current agg_level
        # original_freq_used_for_summary <- r$run_models_summary[[model_name_cv]]$frequency_used # Not directly used here but good to have

        unprepared_recipe_cv <- NULL
        if (model_name_cv %in% c("XGBoost", "RF")) {
          unprepared_recipe_cv <- create_tree_recipe(full_train_data, freq_str = freq_str_cv) 
        }

        for (i in 1:nrow(ts_cv_splits)) {
          progress_counter <- progress_counter + 1
          shiny::setProgress(value = progress_counter / n_total_iterations, # Use setProgress with overall progress
                             detail = paste("Model:", model_name_cv, "- Slice", i, "of", nrow(ts_cv_splits)))
          
          # Skip CV for NNETAR if its tuning is enabled (hypothetical example, NNETAR tuning not implemented)
          # if (model_name_cv == "NNETAR" && isTRUE(model_config_original$enable_tuning %||% FALSE)) {
          #    message(paste("Skipping CV for NNETAR as tuning enabled and CV for tuned NNETAR not implemented."))
          #    # progress_counter is already incremented, shiny::setProgress handles overall progress
          #    next # Skip to next slice or model
          # }

          slice <- ts_cv_splits[i, ]
          train_slice <- timetk::analysis(slice)
          assess_slice <- timetk::assessment(slice)
          
          train_slice <- train_slice %>% dplyr::mutate(ds = as.Date(ds), y = as.numeric(y))
          assess_slice <- assess_slice %>% dplyr::mutate(ds = as.Date(ds), y = as.numeric(y))

          last_train_slice_date <- max(train_slice$ds)
          horizon_cv_fold <- nrow(assess_slice)

          forecast_tibble_cv <- NULL
          # fitted_values_cv <- NULL # Not strictly needed for CV accuracy

          tryCatch({
            if (model_name_cv == "ARIMA") {
              holidays_for_train_slice <- NULL
              if (!is.null(current_global_holidays_cv) && nrow(current_global_holidays_cv) > 0) {
                  holidays_for_train_slice <- current_global_holidays_cv %>% 
                      dplyr::filter(ds >= min(train_slice$ds) & ds <= max(train_slice$ds))
              }
              model_obj_cv <- train_arima(train_slice, model_config_original, aggregation_level = original_agg_level, holidays_df = holidays_for_train_slice)
              if (!is.null(model_obj_cv)) {
                future_xreg_cv <- NULL
                if (!is.null(model_obj_cv$xreg)) {
                  arima_xreg_colnames_cv <- colnames(model_obj_cv$xreg)
                  holidays_for_assess_slice <- NULL
                  if (!is.null(current_global_holidays_cv) && nrow(current_global_holidays_cv) > 0) {
                       holidays_for_assess_slice <- current_global_holidays_cv %>% 
                          dplyr::filter(ds >= min(assess_slice$ds) & ds <= max(assess_slice$ds))
                  }
                  if (!is.null(holidays_for_assess_slice) && nrow(holidays_for_assess_slice) > 0 && !is.null(arima_xreg_colnames_cv)) {
                      if (attr(model_obj_cv, "holiday_regressor_type") == "weekly_aggregated") {
                          assess_dates_for_xreg <- data.frame(ds = assess_slice$ds)
                          future_xreg_df_cv <- assess_dates_for_xreg %>%
                              dplyr::rowwise() %>%
                              dplyr::mutate(
                                  has_holiday_in_week = any(holidays_for_assess_slice$ds >= ds & holidays_for_assess_slice$ds <= (ds + lubridate::days(6)))
                              ) %>% dplyr::ungroup() %>% dplyr::mutate(has_holiday_in_week = as.integer(has_holiday_in_week))
                          if("has_holiday_in_week" %in% names(future_xreg_df_cv) && "has_holiday_in_week" %in% arima_xreg_colnames_cv) { # ensure the column name matches
                               future_xreg_cv <- as.matrix(future_xreg_df_cv %>% dplyr::select(has_holiday_in_week))
                          } else if ("has_holiday_in_week" %in% arima_xreg_colnames_cv) { # Model expects it but not generated
                                future_xreg_cv <- matrix(0, nrow = horizon_cv_fold, ncol = 1, dimnames = list(NULL, "has_holiday_in_week"))
                          }
                      } else { 
                          future_holiday_dummies_cv <- holidays_for_assess_slice %>%
                              dplyr::mutate(holiday = make.names(holiday), value = 1) %>%
                              tidyr::pivot_wider(names_from = holiday, values_from = value, values_fill = 0)
                          future_xreg_df_base_cv <- data.frame(ds = assess_slice$ds)
                          for (col_name in arima_xreg_colnames_cv) { future_xreg_df_base_cv[[col_name]] <- 0 }
                          if (nrow(future_holiday_dummies_cv) > 0 && ncol(future_holiday_dummies_cv) > 1) {
                              cols_from_dummies_cv <- setdiff(names(future_holiday_dummies_cv), "ds")
                              temp_join_cv <- future_xreg_df_base_cv %>% dplyr::select(ds) %>%
                                              dplyr::left_join(future_holiday_dummies_cv %>% dplyr::select(ds, all_of(cols_from_dummies_cv)), by = "ds")
                              for(col_h_cv in cols_from_dummies_cv) {
                                  if(col_h_cv %in% names(future_xreg_df_base_cv) && col_h_cv %in% names(temp_join_cv)) {
                                      future_xreg_df_base_cv[[col_h_cv]] <- dplyr::coalesce(temp_join_cv[[col_h_cv]], future_xreg_df_base_cv[[col_h_cv]])
                                  }
                              }
                          }
                          future_xreg_cv <- as.matrix(future_xreg_df_base_cv %>% dplyr::select(all_of(arima_xreg_colnames_cv)))
                      }
                  } else if (!is.null(arima_xreg_colnames_cv)) { 
                       future_xreg_cv <- matrix(0, nrow = horizon_cv_fold, ncol = length(arima_xreg_colnames_cv), dimnames = list(NULL, arima_xreg_colnames_cv))
                  }
                }
                forecast_obj_cv <- forecast_arima(model_obj_cv, horizon_cv_fold, last_train_slice_date, freq_str_cv, future_xreg = future_xreg_cv, holidays_df = current_global_holidays_cv)
                if(!is.null(forecast_obj_cv)) forecast_tibble_cv <- forecast_obj_cv$forecast
              }
            } else if (model_name_cv == "ETS") {
              model_obj_cv <- train_ets(train_slice, model_config_original, original_agg_level, horizon_cv_fold)
              if(!is.null(model_obj_cv)) {
                forecast_obj_cv <- forecast_ets(model_obj_cv, horizon_cv_fold, last_train_slice_date, freq_str_cv)
                if(!is.null(forecast_obj_cv)) forecast_tibble_cv <- forecast_obj_cv$forecast
              }
            } else if (model_name_cv == "TBATS") {
              model_obj_cv <- train_tbats(train_slice, model_config_original, original_agg_level)
              if(!is.null(model_obj_cv)) {
                forecast_obj_cv <- forecast_tbats(model_obj_cv, horizon_cv_fold, last_train_slice_date, freq_str_cv)
                if(!is.null(forecast_obj_cv)) forecast_tibble_cv <- forecast_obj_cv$forecast
              }
            } else if (model_name_cv == "Prophet") {
              holidays_for_prophet_cv <- NULL
              if (!is.null(current_global_holidays_cv) && nrow(current_global_holidays_cv) > 0) {
                 holidays_for_prophet_cv <- current_global_holidays_cv
              }
              prophet_train_slice_cv <- train_slice
              if(model_config_original$growth == 'logistic' && !is.null(model_config_original$capacity)) {
                  prophet_train_slice_cv$cap <- model_config_original$capacity
              }
              # Simplified: assumes no external regressors for Prophet in CV
              model_obj_cv <- train_prophet(prophet_train_slice_cv, model_config_original, holidays_input = holidays_for_prophet_cv)
              if(!is.null(model_obj_cv)) {
                forecast_tibble_cv <- forecast_prophet(model_obj_cv, horizon_cv_fold, freq_str_cv, model_config_original$capacity)
              }
            } else if (model_name_cv == "XGBoost") {
              req(unprepared_recipe_cv)
              xgb_params_for_cv <- model_config_original # Using original UI config for CV
              prep_recipe_cv_fold <- recipes::prep(unprepared_recipe_cv, training = train_slice)
              model_obj_cv <- train_xgboost(prep_recipe_cv_fold, xgb_params_for_cv)
              if(!is.null(model_obj_cv)) {
                forecast_tibble_cv <- forecast_xgboost(model_obj_cv, prep_recipe_cv_fold, full_train_data, last_train_slice_date, horizon_cv_fold, freq_str_cv)
              }
            } else if (model_name_cv == "GAM") {
              holidays_for_gam_cv <- NULL
              if (!is.null(current_global_holidays_cv) && nrow(current_global_holidays_cv) > 0) {
                 holidays_for_gam_cv <- current_global_holidays_cv
              }
              model_obj_cv <- train_gam(train_slice, model_config_original, holidays_df = holidays_for_gam_cv)
              if(!is.null(model_obj_cv)) {
                forecast_obj_cv <- forecast_gam(model_obj_cv, train_slice, horizon_cv_fold, freq_str_cv, model_config_original, holidays_df = holidays_for_gam_cv)
                if(!is.null(forecast_obj_cv)) forecast_tibble_cv <- forecast_obj_cv$forecast
              }
            } else if (model_name_cv == "RF") {
              req(unprepared_recipe_cv)
              rf_params_for_cv <- model_config_original 
              prep_recipe_cv_fold_rf <- recipes::prep(unprepared_recipe_cv, training = train_slice)
              model_obj_cv <- train_rf(prep_recipe_cv_fold_rf, rf_params_for_cv)
              if(!is.null(model_obj_cv)) {
                forecast_obj_cv <- forecast_rf(model_obj_cv, prep_recipe_cv_fold_rf, full_train_data, train_slice, last_train_slice_date, horizon_cv_fold, freq_str_cv)
                if(!is.null(forecast_obj_cv)) forecast_tibble_cv <- forecast_obj_cv$forecast
              }
            } else if (model_name_cv == "NNETAR") {
              # Use the original config for NNETAR CV (tuning not handled here for NNETAR CV)
              nnetar_config_cv <- r$run_models_summary[[model_name_cv]]$config 
              model_obj_cv <- train_nnetar(train_slice, nnetar_config_cv, original_agg_level)
              if(!is.null(model_obj_cv)) {
                forecast_output_cv <- forecast_nnetar(model_obj_cv, horizon_cv_fold, last_train_slice_date, freq_str_cv)
                if(!is.null(forecast_output_cv)) forecast_tibble_cv <- forecast_output_cv$forecast
              }
            }

            if (!is.null(forecast_tibble_cv) && nrow(forecast_tibble_cv) == nrow(assess_slice)) {
              req("yhat" %in% names(forecast_tibble_cv))
              actuals_assess <- assess_slice$y
              predictions_assess <- if("ds" %in% names(forecast_tibble_cv)) {
                  forecast_tibble_cv[match(assess_slice$ds, forecast_tibble_cv$ds), "yhat", drop = TRUE]
              } else {
                  forecast_tibble_cv$yhat
              }
              if(anyNA(predictions_assess)) {
                  message(paste("Slice", i, "Model", model_name_cv, "- NAs in predictions. Skipping metrics for this fold."))
              } else {
                  metrics_calculator <- yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::mape)
                  fold_metrics_tbl <- metrics_calculator(
                    data = tibble::tibble(truth = actuals_assess, estimate = predictions_assess),
                    truth = truth,
                    estimate = estimate
                  )
                  fold_metrics_tbl <- fold_metrics_tbl %>% 
                    dplyr::mutate(Model = model_name_cv, Fold = i, Slice_Id = slice$.id) # Use Slice_Id
                  all_cv_metrics[[length(all_cv_metrics) + 1]] <- fold_metrics_tbl
                  message(paste("Slice", i, "Model", model_name_cv, "- Metrics calculated."))
              }
            } else {
              message(paste("Slice", i, "Model", model_name_cv, "- Forecast failed or length mismatch. Skipping metrics."))
            }
          }, error = function(e_cv) {
            message(paste("Error in CV for model", model_name_cv, "slice", i, ":", conditionMessage(e_cv)))
          }) 
        } 
      } 
      
      if (length(all_cv_metrics) > 0) {
        final_cv_metrics_df <- dplyr::bind_rows(all_cv_metrics)
        r$cv_raw_metrics_list(final_cv_metrics_df) # Store raw metrics for plotting
        
        cv_summary_table <- final_cv_metrics_df %>%
          dplyr::group_by(Model, .metric) %>%
          dplyr::summarise(.estimate = mean(.estimate, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = .metric, values_from = .estimate)

        r$cv_results_data(cv_summary_table) # Store summarized metrics for table
        shiny::showNotification("Cross-validation complete.", type = "message")
      } else {
        r$cv_results_data(NULL)
        r$cv_raw_metrics_list(NULL)
        shiny::showNotification("Cross-validation ran but no metrics were calculated.", type = "warning")
      }
    }) 

    output$cv_results_table_output <- DT::renderDataTable({
      req(r$cv_results_data())
      DT::datatable(r$cv_results_data(), options = list(pageLength = 5, scrollX = TRUE), caption = "Mean Cross-Validation Metrics")
    })
    output$cv_results_plot_output <- renderPlot({
      req(r$cv_raw_metrics_list())
      final_cv_metrics_df_plot <- r$cv_raw_metrics_list()
      req(nrow(final_cv_metrics_df_plot) > 0)
      ggplot2::ggplot(final_cv_metrics_df_plot, ggplot2::aes(x = Model, y = .estimate, fill = Model)) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::facet_wrap(~ .metric, scales = "free_y") +
        ggplot2::labs(title = "Distribution of CV Metrics Across Folds", x = "Model", y = "Metric Value") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })
    
  }) 
  # --- End Validation Section Server Logic ---

}) # End app_server
}
