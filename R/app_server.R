# R/app_server.R

#' The application server-side
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny dplyr tibble forecast
#' @importFrom shinyjs reset
#' @importFrom RColorBrewer brewer.pal
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
    run_models_summary = list()
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
  )

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
    }, ignoreNULL = FALSE)
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

  # --- Model Execution Logic ---
  observeEvent(model_config_reactives$run_forecast_button(), {
    message("Run Forecast button clicked.")
    # Get required inputs reactively
    train_df <- preprocess_reactives$reactive_train_df()
    test_df <- preprocess_reactives$reactive_test_df()
    full_aggregated_df <- preprocess_reactives$reactive_aggregated_df()
    agg_level <- preprocess_reactives$reactive_agg_level()
    horizon <- model_config_reactives$forecast_horizon()


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
      RF = model_config_reactives$use_rf()
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
            # Common calculations
            n_test_periods <- nrow(test_df)
            n_future_periods <- horizon
            total_periods_needed <- n_test_periods + n_future_periods
            freq_str <- if (agg_level == "Daily") "day" else "week"
            last_train_date <- max(train_df$ds)

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


              model_or_fcst_obj <- train_arima(train_df, config, aggregation_level = agg_level)
              req(model_or_fcst_obj, "ARIMA model training failed (returned NULL).") # Check result
              # message("ARIMA model trained successfully.")

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
              forecast_output <- forecast_arima(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
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
              current_growth = model_config_reactives$prophet_growth() # Eval for logic check
              holidays_input <- model_config_reactives$prophet_holidays_df()
              regressors_input <- model_config_reactives$prophet_regressors_df()
              config <- list(
                yearly = model_config_reactives$prophet_yearly(),
                weekly = model_config_reactives$prophet_weekly(),
                daily = model_config_reactives$prophet_daily(),
                growth = current_growth,
                changepoint_scale = model_config_reactives$prophet_changepoint_scale(),
                capacity = if(current_growth == 'logistic') model_config_reactives$prophet_capacity() else NULL,
                used_holidays = !is.null(holidays_input),
                used_regressors = !is.null(regressors_input) && length(setdiff(names(regressors_input %||% list()), "ds")) > 0 # Use %||% for safety
              )
              model_summary_entry$config <- config # Store static list

              regressor_names_input <- NULL
              if(!is.null(regressors_input)){ regressor_names_input <- setdiff(names(regressors_input), "ds")}
              if(length(regressor_names_input) == 0) regressors_input <- NULL # Treat as no regressors

              prophet_train_df <- train_df
              if(current_growth == 'logistic'){ prophet_train_df$cap <- model_config_reactives$prophet_capacity() }

              print(str(prophet_train_df))
              model_or_fcst_obj <- train_prophet(prophet_train_df, config, holidays_input, regressors_input, regressor_names_input)
              req(model_or_fcst_obj, "Prophet model training failed (returned NULL).") # Stop if NULL, add message

              forecast_tibble <- forecast_prophet(model_or_fcst_obj, total_periods_needed, freq_str, config$capacity, regressors_input, regressor_names_input)
              req(forecast_tibble, "Prophet forecast generation failed (returned NULL).") # Stop if NULL, add message
              # Add check for forecast_tibble structure before proceeding
              req(is.data.frame(forecast_tibble) && all(c("ds", "yhat") %in% names(forecast_tibble)),
                  "Forecast tibble structure is invalid after forecast_prophet.")
              fitted_values <- forecast_tibble %>% dplyr::filter(ds %in% train_df$ds) %>% pull(yhat)

            } else if (model_name == "XGBoost") {
              config <- list(
                nrounds = model_config_reactives$xgb_nrounds(),
                eta = model_config_reactives$xgb_eta(),
                max_depth = model_config_reactives$xgb_max_depth(),
                subsample = model_config_reactives$xgb_subsample(),
                colsample_bytree = model_config_reactives$xgb_colsample(),
                gamma = model_config_reactives$xgb_gamma()
              ) # Extract XGBoost config
              model_summary_entry$config <- config
              recipe <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
              req(recipe, "XGBoost recipe failed")
              model_or_fcst_obj <- train_xgboost(recipe, config)
              req(model_or_fcst_obj, "XGBoost model failed")
              # shiny::incProgress(0.6, detail = "Forecasting XGBoost...")

              ##### ACÁ ESTÁ EL PROBLEMA ----
              forecast_tibble <- forecast_xgboost(model_or_fcst_obj, recipe, full_aggregated_df, last_train_date, total_periods_needed, freq_str)
              req(forecast_tibble, , "XGBoost forecast failed")
              message(print("forecast_tibble"))
              # forecast_tibble <- forecast_output$forecast
              # Get fitted values by predicting on training portion of baked data
              # Need to bake train_df using the *prepared* recipe
              train_baked <- recipes::bake(recipe, new_data = train_df, all_predictors())
              # Ensure features match model features before predicting
              model_features <- model_or_fcst_obj$feature_names
              missing_train_cols <- setdiff(model_features, names(train_baked))
              if (length(missing_train_cols) > 0) { stop("Training data missing model features after baking.")}
              train_matrix <- as.matrix(train_baked[, model_features, drop=FALSE]) # Select and order
              fitted_values <- predict(model_or_fcst_obj, train_matrix)
            }   else if (model_name == "GAM") {
              config <- list(
                smooth_trend = model_config_reactives$gam_trend_type() == "smooth",
                use_season_y = model_config_reactives$gam_use_season_y(),
                use_season_w = model_config_reactives$gam_use_season_w()
                # Add future config items here (e.g., regressor names)
              ) # Extract GAM config
              model_summary_entry$config <- config
              model_or_fcst_obj <- train_gam(train_df, config)
              req(model_or_fcst_obj, "GAM training failed (returned NULL).")
              forecast_output <- forecast_gam(model_or_fcst_obj, train_df, total_periods_needed, freq_str, config)
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
              recipe <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
              req(recipe, "Recipe creation failed for RF.")
              model_or_fcst_obj <- train_rf(recipe, config)
              req(model_or_fcst_obj, "Random Forest training failed (returned NULL).")
              forecast_output <- forecast_rf(model_or_fcst_obj, recipe, full_aggregated_df, train_df, last_train_date, total_periods_needed, freq_str)
              req(forecast_output, "RF forecast_rf function returned NULL.")
              forecast_tibble <- forecast_output$forecast
              req(forecast_tibble, "RF forecast data frame (forecast_output$forecast) is NULL.")
              fitted_values <- forecast_output$fitted
              req(fitted_values, "RF fitted values (forecast_output$fitted) is NULL.")

              # --- Metrics Calculation ---
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
            warning(paste("Error running model", model_name, ":", conditionMessage(e)))
            shiny::showNotification(paste("Failed to run:", model_name), type="warning")
            # Do not stop the loop, just skip storing results for this model
            r$metrics_list = list() # Store list of metric tibbles (for later)
            r$model_summary_list = list() # Store list of model summary info (for later)

            # r$forecast_obj <- NULL
            # r$forecast_df <- NULL
            r$metrics_summary <- NULL
            # r$model_name <- NULL
            # r$arima_selected_order <- NULL
            # r$arima_used_frequency <- NULL
            model_success <<- FALSE
            model_summary_entry$success <- FALSE
            model_summary_entry$error <- conditionMessage(e)
            temp_summary_list[[model_name]] <- model_summary_entry
            # Print error object to console for debugging
            print(paste("ERROR during forecast execution:", Sys.time()))
            print("--- Full Error Object ---")
            print(e) # Print the whole error object 'e'
            print("--- End Error Object ---")

            # shiny::showNotification(
            #   paste("Error during forecast:", conditionMessage(e)), # Still show message in UI
            #   type = "error",
            #   duration = 15
            # )


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
        message(paste0("DEBUG Metrics: Checking model: ", model_name))
        message(paste0("DEBUG Metrics: Length of fitted_values: ", length(fitted_values)))
        message(paste0("DEBUG Metrics: Length of train_actual: ", length(train_actual)))
        message(paste0("DEBUG Metrics: Any NAs in fitted_values? ", anyNA(fitted_values)))
        message(paste0("DEBUG Metrics: Class of fitted_values: ", class(fitted_values)))
        # --- End DEBUG Metrics Check ---
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
            message(paste(" WARN: Could not align test predictions for", model_name, "(Count mismatch). Skipping."))
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
        r$run_id <- r$run_id + 1
        message("Finished all selected models.")
      }
        # --- End Update Trigger ---


        # --- Metrics Calculation (Phase 3 - Requires Loop & List Storage) ---
        # message("Calculating all metrics...")
        # combined_metrics_table <- ... logic to loop through fitted_list/forecast_list ...
        # r$metrics_summary <- combined_metrics_table
        # --- End Metrics ---
      }, error = function(e) { # Outer catch handler
        message(paste("ERROR caught in outer tryCatch:", conditionMessage(e)))
        print("--- Outer tryCatch Error Object ---"); print(e); print("--- End ---")
        r$forecast_list <- list(); r$fitted_list <- list(); r$metrics_summary <- NULL; r$run_models_summary <- list(); r$run_id <- r$run_id + 1 # Reset and trigger update
        shiny::showNotification( paste("Error during forecast execution:", conditionMessage(e)), type = "error", duration = 15)
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




          # Initialize vars
          # model_or_fcst_obj <- NULL # Use generic name for return from train
          # # forecast_tibble <- NULL # To store the final df for plotting
          # # fitted_values <- NULL
          # config <- list() # Default empty config



          # if (active_model_tab == "ARIMA") {
          #   # message("--- Entering ARIMA Block ---")
          #   shiny::incProgress(0.1, detail = "Gathering ARIMA config...")
          #   config <- list(
          #     auto = model_config_reactives$arima_auto(),
          #     p = model_config_reactives$arima_p(),
          #     d = model_config_reactives$arima_d(),
          #     q = model_config_reactives$arima_q(),
          #     seasonal = model_config_reactives$arima_seasonal(),
          #     P = model_config_reactives$arima_P(),
          #     D = model_config_reactives$arima_D(),
          #     Q = model_config_reactives$arima_Q(),
          #     period = model_config_reactives$arima_period()
          #   )
          #   # --- Determine and Store Frequency Used ---
          #   freq_used <- 1 # Default
          #   if (config$seasonal) {
          #     if (config$auto) {
          #       if (agg_level == "Daily") freq_used <- 7
          #       else if (agg_level == "Weekly") freq_used <- 52
          #       # else keep freq_used = 1 or handle other agg_levels
          #     } else { # Manual seasonal
          #       manual_period <- as.integer(config$period)
          #       if (!is.na(manual_period) && manual_period > 1) {
          #         freq_used <- manual_period
          #       } else {
          #         # If manual period is invalid, treat as non-seasonal for summary
          #         config$seasonal <- FALSE # Correct the config flag locally
          #         freq_used <- 1
          #         warning("Manual seasonal period invalid (<=1), treating as non-seasonal.")
          #       }
          #     }
          #   } # else it stays 1 (non-seasonal)
          #   r$arima_used_frequency <- freq_used # Store the calculated frequency
          #   message(paste("ARIMA frequency determined as:", freq_used))
          #   # --- End Frequency Determination ---
          #
          #   # message("Calling train_arima...")
          #   shiny::incProgress(0.2, detail = "Training ARIMA...")
          #   # model <- train_arima(train_df, config)
          #   # req(model) # Stop if training failed
          #   model_or_fcst_obj <- train_arima(train_df, config, aggregation_level = agg_level)
          #   req(model_or_fcst_obj, "ARIMA model training failed (returned NULL).") # Check result
          #   # message("ARIMA model trained successfully.")
          #
          #   # --- ADD: Extract & Store Auto ARIMA Order ---
          #   if (config$auto) {
          #     message("Auto ARIMA selected. Extracting order...")
          #     sel_order <- tryCatch({
          #       forecast::arimaorder(model_or_fcst_obj) # Get the selected order
          #     }, error = function(e_ord){
          #       warning("Could not extract order from auto.arima model object.")
          #       NULL
          #     })
          #     r$arima_selected_order <- sel_order # Store it (can be NULL if failed)
          #     if(!is.null(sel_order)) {message("Stored auto order: ", paste(names(sel_order), sel_order, collapse=", "))}
          #   } else {
          #     r$arima_selected_order <- NULL # Ensure it's NULL if not auto
          #   }
          #   # --- END ADD ---
          #
          #   shiny::incProgress(0.5, detail = "Forecasting ARIMA...")
          #   # message("Calling forecast_arima...")
          #   last_train_date <- max(train_df$ds)
          #
          #   # Call updated forecast_arima
          #   forecast_output <- forecast_arima(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
          #   req(forecast_output, forecast_output$forecast, forecast_output$fitted)
          #   # message("ARIMA forecast generated.")
          #
          #   forecast_tibble <- forecast_output$forecast # Tibble for plotting/test metrics
          #   fitted_values <- forecast_output$fitted    # Vector for train metrics
          #
          #   shiny::incProgress(0.8, detail = "Calculating ARIMA Metrics...")
          #   metrics_list <- list()
          #   # --- Train Metrics ---
          #   train_actual <- train_df$y
          #   if (!is.null(fitted_values) && length(fitted_values) == length(train_actual)) {
          #     train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
          #     if (!is.null(train_metrics_tbl)) {
          #       metrics_list$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
          #     } else { message("ARIMA Training metrics calculation failed.") } # Use message for console debugging
          #   } else {
          #     message(paste("ARIMA Fitted values length mismatch:", length(fitted_values), "vs", length(train_actual), ". Skipping train metrics."))
          #   }
          #   # --- Test Metrics ---
          #   if (nrow(test_df) > 0) {
          #     test_actual <- test_df$y
          #     # Filter forecast_tibble for dates matching the test set
          #     test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)
          #
          #     if (nrow(test_pred_df) == nrow(test_df)) {
          #       # Ensure order matches test_df$ds
          #       test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
          #       test_pred <- test_pred_ordered_df$yhat
          #       test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
          #       if(!is.null(test_metrics_tbl)){
          #         metrics_list$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)
          #       } else { message("ARIMA Test metrics calculation failed.") }
          #     } else {
          #       message("ARIMA: Could not align test predictions (count mismatch). Filtered preds: ", nrow(test_pred_df), ", Actuals: ", nrow(test_df), ". Skipping test metrics.")
          #     }
          #   }
          # }  else if (active_model_tab == "ETS") {
          #   # message("--- Entering ETS Block ---")
          #   # ETS uses automatic selection, config is empty for now
          #   config <- list(
          #     manual = model_config_reactives$ets_manual(),
          #     ets_e = model_config_reactives$ets_e(),
          #     ets_t = model_config_reactives$ets_t(),
          #     ets_s = model_config_reactives$ets_s(),
          #     ets_damped_str = model_config_reactives$ets_damped_str()
          #   )
          #   # message("Calling train_ets...")
          #   # Pass the full config list
          #   # model <- train_ets(train_df, config = config, aggregation_level = agg_level)
          #   # req(model, "ETS training failed.")
          #   # message("ETS model trained successfully.")
          #
          #   model_or_fcst_obj <- train_ets(
          #     train_df,
          #     config = config,
          #     aggregation_level = agg_level,
          #     total_periods_needed = total_periods_needed # Pass total periods
          #   )
          #   # The return is EITHER an ets model OR a forecast object from stlf
          #   req(model_or_fcst_obj, "ETS/STLF training/forecasting failed (returned NULL).")
          #   # message("ETS/STLF training/forecasting finished.")
          #
          #   # Forecasting logic remains the same
          #   # message("Processing forecast output...")
          #   last_train_date <- max(train_df$ds)
          #   # forecast_output <- forecast_ets(model, total_periods_needed, last_train_date, freq_str)
          #   # req(forecast_output, forecast_output$forecast, forecast_output$fitted, "ETS forecasting failed.")
          #   forecast_output <- forecast_ets(
          #     model_or_fcst = model_or_fcst_obj, # Pass the result here
          #     total_periods_needed = total_periods_needed,
          #     train_end_date = last_train_date,
          #     freq_str = freq_str
          #   )
          #   req(forecast_output, forecast_output$forecast, forecast_output$fitted, "ETS forecast processing failed.")
          #   # message("ETS forecast generated.")
          #   forecast_tibble <- forecast_output$forecast
          #   fitted_values <- forecast_output$fitted
          #   # message("--- Exiting ETS Block ---")
          # } else if (active_model_tab == "TBATS") {
          #   # message("--- Entering TBATS Block ---")
          #   # Config is empty for automatic TBATS
          #   model_or_fcst_obj <- train_tbats(train_df, config = list(), aggregation_level = agg_level)
          #   req(model_or_fcst_obj, "TBATS training failed (returned NULL).")
          #   # message("TBATS model trained successfully.")
          #
          #   # message("Calling forecast_tbats...")
          #   last_train_date <- max(train_df$ds)
          #   forecast_output <- forecast_tbats(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
          #   req(forecast_output, forecast_output$forecast, forecast_output$fitted, "TBATS forecasting failed.")
          #   # message("TBATS forecast generated.")
          #   forecast_tibble <- forecast_output$forecast
          #   fitted_values <- forecast_output$fitted
          #   # message("--- Exiting TBATS Block ---")
          #   # --- END ADD TBATS BLOCK ---
          #
          # } else if (active_model_tab == "Prophet") {
          #   # --- Get Holidays / Regressors ---
          #   holidays_input <- model_config_reactives$prophet_holidays_df() # Will be NULL if no file
          #   regressors_input <- model_config_reactives$prophet_regressors_df()
          #   regressor_names_input <- NULL
          #   # if(!is.null(holidays_input)) { message("Head of Holidays data:"); print(head(holidays_input, 2)) }
          #   if (!is.null(regressors_input)) {
          #     # Use all columns except 'ds' as regressors by default
          #     regressor_names_input <- setdiff(names(regressors_input), "ds")
          #     if(length(regressor_names_input) == 0) {
          #       # warning("Regressor file uploaded, but no regressor columns found (only 'ds'). Ignoring regressors.")
          #       regressors_input <- NULL # Treat as no regressors
          #     } else {
          #       # message(paste("Using regressors:", paste(regressor_names_input, collapse=", ")))
          #     }
          #   }
          #   # --- End Get ---
          #   shiny::incProgress(0.1, detail = "Gathering Prophet config...")
          #   prophet_train_df <- train_df # Copy train data
          #
          #   na_in_y_count <- sum(is.na(prophet_train_df$y))
          #   if(na_in_y_count > 0) {
          #     warning(paste("!!! WARNING:", na_in_y_count, "NAs found in 'y' column passed to train_prophet !!!"))
          #     # Depending on requirements, you might stop here:
          #     # stop("Cannot train Prophet model with NAs in target variable 'y'.")
          #   }
          #
          #   # --- Train ---
          #
          #   config <- list(
          #     yearly = model_config_reactives$prophet_yearly(),
          #     weekly = model_config_reactives$prophet_weekly(),
          #     daily = model_config_reactives$prophet_daily(),
          #     growth = model_config_reactives$prophet_growth(),
          #     changepoint_scale = model_config_reactives$prophet_changepoint_scale()
          #   )
          #   capacity_value <- NULL
          #   if (config$growth == 'logistic') {
          #     capacity_value <- model_config_reactives$prophet_capacity()
          #     # Ensure capacity is added to the dataframe passed to train_prophet
          #     validate(need(!is.null(capacity_value) && is.numeric(capacity_value), "Valid Capacity value needed for logistic growth."))
          #     prophet_train_df$cap <- capacity_value
          #   }
          #   # if(any(is.na(prophet_train_df$y))) {
          #   #   # message("WARNING: NAs found in data passed to train_prophet!")
          #   #   # Consider stopping or handling imputation if necessary
          #   # }
          #
          #   shiny::incProgress(0.2, detail = "Training Prophet...")
          #
          #   model_or_fcst_obj <- train_prophet(
          #     train_df = prophet_train_df,
          #     config = config,
          #     holidays_df = holidays_input,
          #     regressors_df = regressors_input, # Must contain training dates
          #     regressor_names = regressor_names_input
          #   )
          #
          #   req(model_or_fcst_obj, "Prophet model training failed (returned NULL).") # Stop if NULL, add message
          #
          #
          #
          #   shiny::incProgress(0.5, detail = "Forecasting Prophet...")
          #   # forecast_prophet handles fitted + forecast in one df
          #   # forecast_tibble <- forecast_prophet(model, horizon, freq = freq_str, capacity = capacity_value)
          #   # req(forecast_tibble)
          #
          #   # --- Calculate total periods needed ---
          #   n_test_periods <- nrow(test_df)
          #   n_future_periods <- horizon # User requested horizon beyond test set
          #   periods_to_generate_prophet <- n_test_periods + n_future_periods
          #
          #
          #   # Pass holidays/regressors to forecast function
          #   forecast_tibble <- forecast_prophet( # Assign result back
          #     model = model_or_fcst_obj, periods_to_generate = periods_to_generate_prophet,
          #     freq = freq_str, capacity = capacity_value,
          #     regressors_df = regressors_input,
          #     regressor_names = regressor_names_input )
          #   # message(paste("forecast_prophet returned. Forecast tibble is NULL:", is.null(forecast_tibble))) # Check return
          #   req(forecast_tibble, "Prophet forecast generation failed (returned NULL).") # Stop if NULL, add message
          #
          #
          #   # --- Fitted Values Extraction ---
          #   # Add check for forecast_tibble structure before proceeding
          #   req(is.data.frame(forecast_tibble) && all(c("ds", "yhat") %in% names(forecast_tibble)),
          #       "Forecast tibble structure is invalid after forecast_prophet.")
          #
          #
          #   fitted_values_df <- forecast_tibble %>% dplyr::filter(ds %in% train_df$ds)
          #   if(nrow(fitted_values_df) != nrow(train_df)) {
          #     warning("Fitted values row count doesn't match train data row count!")
          #     fitted_values <- NULL # Set to NULL if mismatch
          #   }else {
          #     fitted_values_ordered_df <- fitted_values_df[match(train_df$ds, fitted_values_df$ds), ]
          #     fitted_values <- fitted_values_ordered_df$yhat
          #
          #
          #
          #     na_in_fitted_count <- sum(is.na(fitted_values))
          #     if (na_in_fitted_count > 0) {
          #       warning(paste("!!! WARNING:", na_in_fitted_count, "NAs found in Prophet fitted values (yhat for training period) !!!"))
          #       # Print the dates where yhat is NA
          #       na_dates <- train_df$ds[is.na(fitted_values)]
          #       message("Dates with NA fitted values:")
          #       print(na_dates)
          #     }
          #   }
          #
          #   # --- Metrics Calculation (Prophet Part) ---
          #   shiny::incProgress(0.8, detail = "Calculating Prophet Metrics...")
          #   metrics_list <- list()
          #   # --- Train Metrics ---
          #   train_actual <- train_df$y
          #
          #   # The existing check !anyNA(fitted_values) will use the result of the check above
          #   if (!is.null(fitted_values) && length(fitted_values) == length(train_actual) && all(!is.na(fitted_values))) {
          #     train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
          #     if (!is.null(train_metrics_tbl)) {
          #       metrics_list$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
          #     } else { message("Prophet Training metrics calculation failed.") }
          #   } else {
          #     # This message will now appear if length mismatch OR if NAs were found above
          #     message("Prophet Training metrics skipped due to length mismatch or NAs in fitted values.")
          #   }
          #
          #
          #   # --- Test Metrics ---
          #   if (nrow(test_df) > 0) {
          #     test_actual <- test_df$y
          #
          #     # Check if forecast dates *contain* the test date range
          #     fcst_contains_test_start <- min(forecast_tibble$ds) <= min(test_df$ds)
          #     fcst_contains_test_end <- max(forecast_tibble$ds) >= max(test_df$ds)
          #
          #     # Align forecast with test actuals based on date 'ds'
          #     test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)
          #
          #
          #     if (nrow(test_pred_df) == nrow(test_df)) {
          #       # Ensure order matches test_df$ds
          #       test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
          #       test_pred <- test_pred_ordered_df$yhat
          #
          #       if(any(is.na(test_pred))){
          #         message("WARNING: NAs found in test predictions!")
          #         test_metrics_tbl <- NULL # Skip metrics if NAs present
          #       } else {
          #         test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
          #       }
          #
          #       if(!is.null(test_metrics_tbl)){
          #         metrics_list$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)
          #
          #       } else {
          #         message("Prophet Test metrics calculation failed or skipped due to NAs.")
          #       }
          #     } else {
          #       message(paste0("Prophet: Test alignment failed (count mismatch). Filtered=", nrow(test_pred_df), ", Actual=", nrow(test_df), "). Skipping test metrics."))
          #       # Show the dates that *did* match, if any
          #       if(nrow(test_pred_df) > 0) {
          #         message("Matching forecast dates found:")
          #         print(head(test_pred_df$ds)) # Print head of matching dates
          #       } else {
          #         message("No matching forecast dates found in the test range.")
          #       }
          #     }
          #
          #   }else {
          #     message("Prophet: No test data found (test_df has 0 rows).")
          #   }
          #
          #
          #
          # ##### XGBoost ----
          # } else if (active_model_tab == "XGBoost") {
          #   shiny::incProgress(0.1, detail = "Gathering XGBoost config...")
          #   config <- list(
          #     nrounds = model_config_reactives$xgb_nrounds(),
          #     eta = model_config_reactives$xgb_eta(),
          #     max_depth = model_config_reactives$xgb_max_depth(),
          #     subsample = model_config_reactives$xgb_subsample(),
          #     colsample_bytree = model_config_reactives$xgb_colsample(),
          #     gamma = model_config_reactives$xgb_gamma()
          #   )
          #
          #   shiny::incProgress(0.2, detail = "Preprocessing XGBoost Data...")
          #   # Create recipe using the *full aggregated data* before split
          #   # recipe <- create_xgb_recipe(full_aggregated_df)
          #   # Pass freq_str to the recipe creation function
          #   recipe <- create_tree_recipe(
          #     df = full_aggregated_df, # Use full agg df for recipe prep
          #     freq_str = freq_str      # Pass the frequency
          #     # Can also pass max_lag, window_sizes from UI later if desired
          #   )
          #   req(recipe)
          #   # print(summary(recipe))
          #
          #   shiny::incProgress(0.4, detail = "Training XGBoost...")
          #   model_or_fcst_obj <- train_xgboost(recipe, config)
          #   req(model_or_fcst_obj)
          #
          #   shiny::incProgress(0.6, detail = "Forecasting XGBoost...")
          #   # Get last date of training data
          #   last_train_date <- max(train_df$ds) # Get end date of train set
          #
          #   # Call forecast_xgboost with train_end_date
          #   forecast_tibble <- forecast_xgboost(
          #     model = model_or_fcst_obj,
          #     prep_recipe = recipe,
          #     full_df = full_aggregated_df,    # Still needed for history in lags
          #     train_end_date = last_train_date, # Pass train end date
          #     total_periods_needed = total_periods_needed,
          #     freq = freq_str
          #   )
          #   # forecast_tibble <- forecast_xgboost(model, recipe, full_aggregated_df, total_periods_needed, freq = freq_str)
          #   req(forecast_tibble)
          #
          #   # Get fitted values by predicting on training portion of baked data
          #   # Need to bake train_df using the *prepared* recipe
          #   train_baked <- recipes::bake(recipe, new_data = train_df, all_predictors())
          #   # Ensure features match model features before predicting
          #   model_features <- model_or_fcst_obj$feature_names
          #   missing_train_cols <- setdiff(model_features, names(train_baked))
          #   if (length(missing_train_cols) > 0) { stop("Training data missing model features after baking.")}
          #   train_matrix <- as.matrix(train_baked[, model_features, drop=FALSE]) # Select and order
          #   fitted_values <- predict(model_or_fcst_obj, train_matrix)
          #
          # } else if (active_model_tab == "GAM") {
          #   # message("--- Entering GAM Block ---")
          #   # Extract GAM Config
          #   config <- list(
          #     smooth_trend = model_config_reactives$gam_trend_type() == "smooth",
          #     use_season_y = model_config_reactives$gam_use_season_y(),
          #     use_season_w = model_config_reactives$gam_use_season_w()
          #     # Add future config items here (e.g., regressor names)
          #   )
          #
          #   # message("Calling train_gam...")
          #   # Train GAM model (feature engineering happens inside train_gam)
          #   model_or_fcst_obj <- train_gam(train_df, config = config)
          #   req(model_or_fcst_obj, "GAM training failed (returned NULL).")
          #   # message("GAM model trained successfully.")
          #
          #   # Forecast GAM model
          #   # message("Calling forecast_gam...")
          #   last_train_date <- max(train_df$ds) # Used to start forecast dates
          #   forecast_output <- forecast_gam(
          #     model = model_or_fcst_obj,
          #     train_df = train_df, # Pass original train_df for context
          #     total_periods_needed = total_periods_needed,
          #     freq_str = freq_str,
          #     config = config # Pass config if forecast features depend on it
          #   )
          #   req(forecast_output, forecast_output$forecast, forecast_output$fitted, "GAM forecasting failed.")
          #   # message("GAM forecast processed.")
          #   forecast_tibble <- forecast_output$forecast
          #   fitted_values <- forecast_output$fitted
          #   # message("--- Exiting GAM Block ---")
          #   # --- END ADD GAM BLOCK ---
          #
          # } else if (active_model_tab == "RF") {
          # #   # message("--- Entering RF Block ---")
          # #   # Extract RF Config
          # #   config <- list(
          # #     rf_num_trees = model_config_reactives$rf_num_trees(),
          # #     rf_mtry = model_config_reactives$rf_mtry(), # Pass 0 for auto
          # #     rf_min_node_size = model_config_reactives$rf_min_node_size()
          # #   )
          # #
          # #   # message("Preprocessing Tree Data...")
          # #   # --- Use Renamed Recipe Function ---
          # #   recipe <- create_tree_recipe(
          # #     df = full_aggregated_df,
          # #     freq_str = freq_str
          # #     # Pass other args like max_lag if configurable later
          # #   )
          # #   req(recipe, "Recipe creation failed for RF.")
          # #   # message("Recipe created successfully.")
          # #
          # #   # message("Calling train_rf...")
          # #   model_or_fcst_obj <- train_rf(recipe, config = config)
          # #   req(model_or_fcst_obj, "Random Forest training failed (returned NULL).")
          # #   # message("Random Forest model trained successfully.")
          # #
          # #   # message("Calling forecast_rf...")
          # #   last_train_date <- max(train_df$ds)
          # #   forecast_output <- forecast_rf(
          # #     model = model_or_fcst_obj,
          # #     prep_recipe = recipe,
          # #     full_df = full_aggregated_df, # Still needed for history
          # #     train_df = train_df, # <<< Pass train_df here
          # #     train_end_date = last_train_date,
          # #     total_periods_needed = total_periods_needed,
          # #     freq_str = freq_str
          # #   )
          # #   # --- Add Robust Checks ---
          # #   req(forecast_output, "RF forecast_rf function returned NULL.")
          # #   req(forecast_output$forecast, "RF forecast data frame (forecast_output$forecast) is NULL.")
          # #   # Fitted values might be NULL if baking failed on train_df, allow NULL for now
          # #   # req(forecast_output$fitted, "RF fitted values (forecast_output$fitted) is NULL.")
          # #   # --- End Checks ---
          # #
          # #   # message("RF forecast processed.")
          # #   forecast_tibble <- forecast_output$forecast
          # #   fitted_values <- forecast_output$fitted # Might be NULL
          # #   # model_or_fcst_obj <- model
          # #
          # #   # --- Metrics Calculation ---
          # #   metrics_list <- list()
          # #   # Train Metrics (check logic remains the same)
          # #   train_actual <- train_df$y
          # #   # --- Keep Detailed Check Here ---
          # #   if(!is.null(fitted_values)) { # Checks moved here for clarity
          # #     message(paste("  Length fitted:", length(fitted_values), "vs Actual:", length(train_actual)))
          # #     message(paste("  Length match?:", length(fitted_values) == length(train_actual)))
          # #     message(paste("  Any NAs in fitted?:", anyNA(fitted_values)))
          # #     message(paste("  All !is.na(fitted)?:", all(!is.na(fitted_values))))
          # #   }
          # # } else {
          #   stop(paste("Selected model tab not recognized:", active_model_tab))
          # } # End model selection if/else

          # # --- 3. Metrics Calculation ---
          # shiny::incProgress(0.8, detail = "Calculating Metrics...")
          #
          # metrics_list <- list() # Reset metrics list
          #
          # # --- Train Metrics ---
          # train_actual <- train_df$y
          #
          # # message("--- Checking RF Train Metrics Conditions ---") # Add header
          # # message(paste("  fitted_values is NULL?:", is.null(fitted_values)))
          # if(!is.null(fitted_values)) {
          #   message(paste("  Length fitted:", length(fitted_values), "vs Actual:", length(train_actual)))
          #   message(paste("  Length match?:", length(fitted_values) == length(train_actual)))
          #   message(paste("  Any NAs in fitted?:", anyNA(fitted_values)))
          #   message(paste("  All !is.na(fitted)?:", all(!is.na(fitted_values)))) # Check this specific part
          # }
          # # message("--- End Check ---")
          #
          # # Ensure fitted values align with train_actual
          # if (!is.null(fitted_values) && length(fitted_values) == length(train_actual) && all(!is.na(fitted_values))) {
          #   train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
          #   if (!is.null(train_metrics_tbl)) {
          #     metrics_list$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
          #   } else { warning("Training metrics calculation failed.") }
          # } else {
          #   warning("Fitted values length mismatch or NULL. Skipping train metrics.")
          # }
          #
          # # --- Test Metrics ---
          # if (nrow(test_df) > 0) {
          #   test_actual <- test_df$y
          #
          #   # Align forecast with test actuals based on date 'ds'
          #   test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)
          #
          #   if (nrow(test_pred_df) == nrow(test_df)) {
          #     # Ensure order matches test_df$ds for correct comparison
          #
          #     test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
          #     test_pred <- test_pred_ordered_df$yhat
          #
          #     if(all(!is.na(test_pred))){
          #       test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
          #       if(!is.null(test_metrics_tbl)){
          #         metrics_list$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)
          #
          #       } else { message("Test metrics calculation failed.") } # Generic Label
          #     } else {
          #       message("Test predictions contain NAs. Skipping.") # Generic Label
          #     }
          #   } else {
          #     warning("Could not align test predictions with actuals (check forecast dates/length). Skipping test metrics.")
          #     message(paste0("Could not align test predictions (count mismatch). Filtered=", nrow(test_pred_df), ", Actual=", nrow(test_df), "). Skipping test metrics."))
          #   }
          # } else {
          #   # No test set, maybe add a note to metrics output?
          #   # For now, just don't add test metrics to the list
          #   message("No test data found.") # Generic Label
          # }
          # # Combine metrics and store
          # if (length(metrics_list) > 0) {
          #   r$metrics_summary <- bind_rows(metrics_list) %>%
          #     select(Model, DataSet, .metric, .estimate) # Ensure column order
          # } else {
          #   r$metrics_summary <- NULL # Set to null if no metrics calculated
          #   shiny::showNotification("Metrics calculation skipped or failed.", type="warning")
          # }

          # --- 4. Update Results ---
          # r$forecast_df <- forecast_tibble # Update reactive value for plot
          # r$model_name <- model_name      # Store which model was run
          # r$forecast_obj <- model_or_fcst_obj
          # model_success <- TRUE
          # message(paste("--- Exiting", model_name, "Block Successfully ---"))
          # shiny::incProgress(1.0, detail = "Done.")

      #   }, # End tryCatch block
      #   error = function(e) { # Error handler for tryCatch
      #     # Clear results on any error during the process
      #     r$forecast_obj <- NULL
      #     r$forecast_df <- NULL
      #     r$metrics_summary <- NULL
      #     r$model_name <- NULL
      #     r$arima_selected_order <- NULL
      #     r$arima_used_frequency <- NULL
      #     model_success <<- FALSE
      #     # Print error object to console for debugging
      #     print(paste("ERROR during forecast execution:", Sys.time()))
      #     print("--- Full Error Object ---")
      #     print(e) # Print the whole error object 'e'
      #     print("--- End Error Object ---")
      #
      #     shiny::showNotification(
      #       paste("Error during forecast:", conditionMessage(e)), # Still show message in UI
      #       type = "error",
      #       duration = 15
      #     )
      #   }
      # ) # End tryCatch
    # }) # End withProgress

    # Final notification outside withProgress based on success
    # if(model_success) {
    #   shiny::showNotification(paste(r$model_name, "forecast complete."), type = "message", duration = 5)
    # } else {
    #   # Error notification already shown by tryCatch
    # }
    #
    # # --- Reset File Inputs AFTER execution attempt (success or fail) ---
    # # Use the specific namespaced IDs
    # # message("Attempting to reset file inputs...") # Log the attempt
    # shinyjs::reset("model_config_1-prophet_holidays_file")
    # shinyjs::reset("model_config_1-prophet_regressors_file")
    # # You could potentially reset the main data upload too if desired:
    # # shinyjs::reset("data_input_1-fileUpload")
    # # --- End Reset ---


  }) # End observeEvent

} # End app_server
