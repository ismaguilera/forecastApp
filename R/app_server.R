# R/app_server.R

#' The application server-side
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tibble
#' @import forecast
#' @importFrom shinyjs reset
# Needed for forecast() call inside observeEvent
# Add other necessary imports if functions are called directly here
#' @noRd
app_server <- function(input, output, session) {

  # --- Reactive Values Store ---
  r <- reactiveValues(
    forecast_obj = NULL, # Can store raw forecast output if needed
    forecast_df = NULL, # Tibble for plotting
    metrics_summary = NULL, # Tibble for table
    model_name = NULL, # Store name of model run
    arima_selected_order = NULL,
    arima_used_frequency = NULL
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
    reactive_forecast_df = reactive({ r$forecast_df }) # Use forecast_df for plot
  )
  mod_model_summary_server(
    "model_summary_1",
    reactive_model_name = reactive({ r$model_name }), # Pass reactive model name
    reactive_model_config = model_config_reactives,  # Pass the whole list of config reactives
    reactive_arima_selected_order = reactive({ r$arima_selected_order }),
    reactive_aggregation_level = preprocess_reactives$reactive_agg_level,
    reactive_arima_used_frequency = reactive({ r$arima_used_frequency }) # Pass new reactive
  )

  mod_results_table_server(
    "results_table_1",
    reactive_metrics_summary = reactive({ r$metrics_summary })
  )
  mod_extra_plots_server(
    "extra_plots_1",
    reactive_train_df = preprocess_reactives$reactive_train_df,
    reactive_test_df = preprocess_reactives$reactive_test_df,
    reactive_forecast_df = reactive({ r$forecast_df }))


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

    # --- 1. Input Validation & Data Prep ---
    # Use req() to ensure data and configs are available
    req(
      preprocess_reactives$reactive_train_df(),
      preprocess_reactives$reactive_test_df(),
      preprocess_reactives$reactive_aggregated_df(), # Needed for XGBoost features
      preprocess_reactives$reactive_agg_level(),
      model_config_reactives$active_tab(),
      model_config_reactives$forecast_horizon()
    )


    train_df <- preprocess_reactives$reactive_train_df()
    test_df <- preprocess_reactives$reactive_test_df()
    full_aggregated_df <- preprocess_reactives$reactive_aggregated_df()
    active_model_tab <- model_config_reactives$active_tab()
    horizon <- model_config_reactives$forecast_horizon()
    agg_level <- preprocess_reactives$reactive_agg_level()
    freq_str <- if (agg_level == "Daily") "day" else "week"

    n_test_periods <- nrow(test_df)
    n_future_periods <- horizon # User requested future horizon
    total_periods_needed <- n_test_periods + n_future_periods

    # Basic check for enough training data
    validate(need(nrow(train_df) >= 5, "Need at least 5 training data points.")) # Adjust as needed

    # Reset previous results
    r$forecast_obj <- NULL
    r$forecast_df <- NULL
    r$metrics_summary <- NULL
    r$model_name <- NULL
    r$arima_selected_order <- NULL
    r$arima_used_frequency <- NULL
    model_success <- FALSE # Flag

    shiny::withProgress(message = 'Running Forecast...', value = 0, {
      tryCatch(
        { # Wrap entire process in tryCatch
          # --- 2. Model Selection & Execution ---
          model_name <- active_model_tab # Assuming tab name is model name
          message(paste("Attempting to run model:", model_name))
          # Initialize vars
          model_or_fcst_obj <- NULL # Use generic name for return from train
          forecast_tibble <- NULL # To store the final df for plotting
          fitted_values <- NULL
          config <- list() # Default empty config



          if (active_model_tab == "ARIMA") {
            message("--- Entering ARIMA Block ---")
            shiny::incProgress(0.1, detail = "Gathering ARIMA config...")
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
            )
            # --- Determine and Store Frequency Used ---
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
            r$arima_used_frequency <- freq_used # Store the calculated frequency
            message(paste("ARIMA frequency determined as:", freq_used))
            # --- End Frequency Determination ---

            message("Calling train_arima...")
            shiny::incProgress(0.2, detail = "Training ARIMA...")
            # model <- train_arima(train_df, config)
            # req(model) # Stop if training failed
            model_or_fcst_obj <- train_arima(train_df, config, aggregation_level = agg_level)
            req(model_or_fcst_obj, "ARIMA model training failed (returned NULL).") # Check result
            message("ARIMA model trained successfully.")

            # --- ADD: Extract & Store Auto ARIMA Order ---
            if (config$auto) {
              message("Auto ARIMA selected. Extracting order...")
              sel_order <- tryCatch({
                forecast::arimaorder(model_or_fcst_obj) # Get the selected order
              }, error = function(e_ord){
                warning("Could not extract order from auto.arima model object.")
                NULL
              })
              r$arima_selected_order <- sel_order # Store it (can be NULL if failed)
              if(!is.null(sel_order)) {message("Stored auto order: ", paste(names(sel_order), sel_order, collapse=", "))}
            } else {
              r$arima_selected_order <- NULL # Ensure it's NULL if not auto
            }
            # --- END ADD ---

            shiny::incProgress(0.5, detail = "Forecasting ARIMA...")
            message("Calling forecast_arima...")
            last_train_date <- max(train_df$ds)

            # Call updated forecast_arima
            forecast_output <- forecast_arima(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
            req(forecast_output, forecast_output$forecast, forecast_output$fitted)
            message("ARIMA forecast generated.")

            forecast_tibble <- forecast_output$forecast # Tibble for plotting/test metrics
            fitted_values <- forecast_output$fitted    # Vector for train metrics

            shiny::incProgress(0.8, detail = "Calculating ARIMA Metrics...")
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
          }  else if (active_model_tab == "ETS") {
            message("--- Entering ETS Block ---")
            # ETS uses automatic selection, config is empty for now
            config <- list(
              manual = model_config_reactives$ets_manual(),
              ets_e = model_config_reactives$ets_e(),
              ets_t = model_config_reactives$ets_t(),
              ets_s = model_config_reactives$ets_s(),
              ets_damped_str = model_config_reactives$ets_damped_str()
            )
            message("Calling train_ets...")
            # Pass the full config list
            # model <- train_ets(train_df, config = config, aggregation_level = agg_level)
            # req(model, "ETS training failed.")
            # message("ETS model trained successfully.")

            model_or_fcst_obj <- train_ets(
              train_df,
              config = config,
              aggregation_level = agg_level,
              total_periods_needed = total_periods_needed # Pass total periods
            )
            # The return is EITHER an ets model OR a forecast object from stlf
            req(model_or_fcst_obj, "ETS/STLF training/forecasting failed (returned NULL).")
            message("ETS/STLF training/forecasting finished.")

            # Forecasting logic remains the same
            message("Processing forecast output...")
            last_train_date <- max(train_df$ds)
            # forecast_output <- forecast_ets(model, total_periods_needed, last_train_date, freq_str)
            # req(forecast_output, forecast_output$forecast, forecast_output$fitted, "ETS forecasting failed.")
            forecast_output <- forecast_ets(
              model_or_fcst = model_or_fcst_obj, # Pass the result here
              total_periods_needed = total_periods_needed,
              train_end_date = last_train_date,
              freq_str = freq_str
            )
            req(forecast_output, forecast_output$forecast, forecast_output$fitted, "ETS forecast processing failed.")
            message("ETS forecast generated.")
            forecast_tibble <- forecast_output$forecast
            fitted_values <- forecast_output$fitted
            message("--- Exiting ETS Block ---")
          } else if (active_model_tab == "TBATS") {
            message("--- Entering TBATS Block ---")
            # Config is empty for automatic TBATS
            model_or_fcst_obj <- train_tbats(train_df, config = list(), aggregation_level = agg_level)
            req(model_or_fcst_obj, "TBATS training failed (returned NULL).")
            message("TBATS model trained successfully.")

            message("Calling forecast_tbats...")
            last_train_date <- max(train_df$ds)
            forecast_output <- forecast_tbats(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
            req(forecast_output, forecast_output$forecast, forecast_output$fitted, "TBATS forecasting failed.")
            message("TBATS forecast generated.")
            forecast_tibble <- forecast_output$forecast
            fitted_values <- forecast_output$fitted
            message("--- Exiting TBATS Block ---")
            # --- END ADD TBATS BLOCK ---

          } else if (active_model_tab == "Prophet") {
            # --- Get Holidays / Regressors ---
            holidays_input <- model_config_reactives$prophet_holidays_df() # Will be NULL if no file
            regressors_input <- model_config_reactives$prophet_regressors_df()
            regressor_names_input <- NULL
            # if(!is.null(holidays_input)) { message("Head of Holidays data:"); print(head(holidays_input, 2)) }
            if (!is.null(regressors_input)) {
              # Use all columns except 'ds' as regressors by default
              regressor_names_input <- setdiff(names(regressors_input), "ds")
              if(length(regressor_names_input) == 0) {
                # warning("Regressor file uploaded, but no regressor columns found (only 'ds'). Ignoring regressors.")
                regressors_input <- NULL # Treat as no regressors
              } else {
                # message(paste("Using regressors:", paste(regressor_names_input, collapse=", ")))
              }
            }
            # --- End Get ---
            shiny::incProgress(0.1, detail = "Gathering Prophet config...")
            prophet_train_df <- train_df # Copy train data

            na_in_y_count <- sum(is.na(prophet_train_df$y))
            if(na_in_y_count > 0) {
              warning(paste("!!! WARNING:", na_in_y_count, "NAs found in 'y' column passed to train_prophet !!!"))
              # Depending on requirements, you might stop here:
              # stop("Cannot train Prophet model with NAs in target variable 'y'.")
            }

            # --- Train ---

            config <- list(
              yearly = model_config_reactives$prophet_yearly(),
              weekly = model_config_reactives$prophet_weekly(),
              daily = model_config_reactives$prophet_daily(),
              growth = model_config_reactives$prophet_growth(),
              changepoint_scale = model_config_reactives$prophet_changepoint_scale()
            )
            capacity_value <- NULL
            if (config$growth == 'logistic') {
              capacity_value <- model_config_reactives$prophet_capacity()
              # Ensure capacity is added to the dataframe passed to train_prophet
              validate(need(!is.null(capacity_value) && is.numeric(capacity_value), "Valid Capacity value needed for logistic growth."))
              prophet_train_df$cap <- capacity_value
            }
            # if(any(is.na(prophet_train_df$y))) {
            #   # message("WARNING: NAs found in data passed to train_prophet!")
            #   # Consider stopping or handling imputation if necessary
            # }

            shiny::incProgress(0.2, detail = "Training Prophet...")

            model_or_fcst_obj <- train_prophet(
              train_df = prophet_train_df,
              config = config,
              holidays_df = holidays_input,
              regressors_df = regressors_input, # Must contain training dates
              regressor_names = regressor_names_input
            )

            req(model_or_fcst_obj, "Prophet model training failed (returned NULL).") # Stop if NULL, add message



            shiny::incProgress(0.5, detail = "Forecasting Prophet...")
            # forecast_prophet handles fitted + forecast in one df
            # forecast_tibble <- forecast_prophet(model, horizon, freq = freq_str, capacity = capacity_value)
            # req(forecast_tibble)

            # --- Calculate total periods needed ---
            n_test_periods <- nrow(test_df)
            n_future_periods <- horizon # User requested horizon beyond test set
            periods_to_generate_prophet <- n_test_periods + n_future_periods


            # Pass holidays/regressors to forecast function
            forecast_tibble <- forecast_prophet( # Assign result back
              model = model_or_fcst_obj, periods_to_generate = periods_to_generate_prophet,
              freq = freq_str, capacity = capacity_value,
              regressors_df = regressors_input,
              regressor_names = regressor_names_input )
            # message(paste("forecast_prophet returned. Forecast tibble is NULL:", is.null(forecast_tibble))) # Check return
            req(forecast_tibble, "Prophet forecast generation failed (returned NULL).") # Stop if NULL, add message


            # --- Fitted Values Extraction ---
            # Add check for forecast_tibble structure before proceeding
            req(is.data.frame(forecast_tibble) && all(c("ds", "yhat") %in% names(forecast_tibble)),
                "Forecast tibble structure is invalid after forecast_prophet.")


            fitted_values_df <- forecast_tibble %>% dplyr::filter(ds %in% train_df$ds)
            if(nrow(fitted_values_df) != nrow(train_df)) {
              warning("Fitted values row count doesn't match train data row count!")
              fitted_values <- NULL # Set to NULL if mismatch
            }else {
              fitted_values_ordered_df <- fitted_values_df[match(train_df$ds, fitted_values_df$ds), ]
              fitted_values <- fitted_values_ordered_df$yhat



              na_in_fitted_count <- sum(is.na(fitted_values))
              if (na_in_fitted_count > 0) {
                warning(paste("!!! WARNING:", na_in_fitted_count, "NAs found in Prophet fitted values (yhat for training period) !!!"))
                # Print the dates where yhat is NA
                na_dates <- train_df$ds[is.na(fitted_values)]
                message("Dates with NA fitted values:")
                print(na_dates)
              }
            }

            # --- Metrics Calculation (Prophet Part) ---
            shiny::incProgress(0.8, detail = "Calculating Prophet Metrics...")
            metrics_list <- list()
            # --- Train Metrics ---
            train_actual <- train_df$y

            # The existing check !anyNA(fitted_values) will use the result of the check above
            if (!is.null(fitted_values) && length(fitted_values) == length(train_actual) && all(!is.na(fitted_values))) {
              train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
              if (!is.null(train_metrics_tbl)) {
                metrics_list$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
              } else { message("Prophet Training metrics calculation failed.") }
            } else {
              # This message will now appear if length mismatch OR if NAs were found above
              message("Prophet Training metrics skipped due to length mismatch or NAs in fitted values.")
            }


            # --- Test Metrics ---
            if (nrow(test_df) > 0) {
              test_actual <- test_df$y

              # Check if forecast dates *contain* the test date range
              fcst_contains_test_start <- min(forecast_tibble$ds) <= min(test_df$ds)
              fcst_contains_test_end <- max(forecast_tibble$ds) >= max(test_df$ds)

              # Align forecast with test actuals based on date 'ds'
              test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)


              if (nrow(test_pred_df) == nrow(test_df)) {
                # Ensure order matches test_df$ds
                test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
                test_pred <- test_pred_ordered_df$yhat

                if(any(is.na(test_pred))){
                  message("WARNING: NAs found in test predictions!")
                  test_metrics_tbl <- NULL # Skip metrics if NAs present
                } else {
                  test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
                }

                if(!is.null(test_metrics_tbl)){
                  metrics_list$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)

                } else {
                  message("Prophet Test metrics calculation failed or skipped due to NAs.")
                }
              } else {
                message(paste0("Prophet: Test alignment failed (count mismatch). Filtered=", nrow(test_pred_df), ", Actual=", nrow(test_df), "). Skipping test metrics."))
                # Show the dates that *did* match, if any
                if(nrow(test_pred_df) > 0) {
                  message("Matching forecast dates found:")
                  print(head(test_pred_df$ds)) # Print head of matching dates
                } else {
                  message("No matching forecast dates found in the test range.")
                }
              }

            }else {
              message("Prophet: No test data found (test_df has 0 rows).")
            }



          ##### XGBoost ----
          } else if (active_model_tab == "XGBoost") {
            shiny::incProgress(0.1, detail = "Gathering XGBoost config...")
            config <- list(
              nrounds = model_config_reactives$xgb_nrounds(),
              eta = model_config_reactives$xgb_eta(),
              max_depth = model_config_reactives$xgb_max_depth(),
              subsample = model_config_reactives$xgb_subsample(),
              colsample_bytree = model_config_reactives$xgb_colsample(),
              gamma = model_config_reactives$xgb_gamma()
            )

            shiny::incProgress(0.2, detail = "Preprocessing XGBoost Data...")
            # Create recipe using the *full aggregated data* before split
            # recipe <- create_xgb_recipe(full_aggregated_df)
            # Pass freq_str to the recipe creation function
            recipe <- create_xgb_recipe(
              df = full_aggregated_df, # Use full agg df for recipe prep
              freq_str = freq_str      # Pass the frequency
              # Can also pass max_lag, window_sizes from UI later if desired
            )
            req(recipe)
            # print(summary(recipe))

            shiny::incProgress(0.4, detail = "Training XGBoost...")
            model_or_fcst_obj <- train_xgboost(recipe, config)
            req(model_or_fcst_obj)

            shiny::incProgress(0.6, detail = "Forecasting XGBoost...")
            # Get last date of training data
            last_train_date <- max(train_df$ds) # Get end date of train set

            # Call forecast_xgboost with train_end_date
            forecast_tibble <- forecast_xgboost(
              model = model_or_fcst_obj,
              prep_recipe = recipe,
              full_df = full_aggregated_df,    # Still needed for history in lags
              train_end_date = last_train_date, # Pass train end date
              total_periods_needed = total_periods_needed,
              freq = freq_str
            )
            # forecast_tibble <- forecast_xgboost(model, recipe, full_aggregated_df, total_periods_needed, freq = freq_str)
            req(forecast_tibble)

            # Get fitted values by predicting on training portion of baked data
            # Need to bake train_df using the *prepared* recipe
            train_baked <- recipes::bake(recipe, new_data = train_df, all_predictors())
            # Ensure features match model features before predicting
            model_features <- model_or_fcst_obj$feature_names
            missing_train_cols <- setdiff(model_features, names(train_baked))
            if (length(missing_train_cols) > 0) { stop("Training data missing model features after baking.")}
            train_matrix <- as.matrix(train_baked[, model_features, drop=FALSE]) # Select and order
            fitted_values <- predict(model_or_fcst_obj, train_matrix)

          } else if (active_model_tab == "GAM") {
            message("--- Entering GAM Block ---")
            # Extract GAM Config
            config <- list(
              smooth_trend = model_config_reactives$gam_trend_type() == "smooth",
              use_season_y = model_config_reactives$gam_use_season_y(),
              use_season_w = model_config_reactives$gam_use_season_w()
              # Add future config items here (e.g., regressor names)
            )

            message("Calling train_gam...")
            # Train GAM model (feature engineering happens inside train_gam)
            model_or_fcst_obj <- train_gam(train_df, config = config)
            req(model_or_fcst_obj, "GAM training failed (returned NULL).")
            message("GAM model trained successfully.")

            # Forecast GAM model
            message("Calling forecast_gam...")
            last_train_date <- max(train_df$ds) # Used to start forecast dates
            forecast_output <- forecast_gam(
              model = model_or_fcst_obj,
              train_df = train_df, # Pass original train_df for context
              total_periods_needed = total_periods_needed,
              freq_str = freq_str,
              config = config # Pass config if forecast features depend on it
            )
            req(forecast_output, forecast_output$forecast, forecast_output$fitted, "GAM forecasting failed.")
            message("GAM forecast processed.")
            forecast_tibble <- forecast_output$forecast
            fitted_values <- forecast_output$fitted
            message("--- Exiting GAM Block ---")
            # --- END ADD GAM BLOCK ---

          }else {
            stop(paste("Selected model tab not recognized:", active_model_tab))
          } # End model selection if/else

          # --- 3. Metrics Calculation ---
          shiny::incProgress(0.8, detail = "Calculating Metrics...")

          metrics_list <- list() # Reset metrics list

          # --- Train Metrics ---
          train_actual <- train_df$y
          # Ensure fitted values align with train_actual
          if (!is.null(fitted_values) && length(fitted_values) == length(train_actual)) {
            train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
            if (!is.null(train_metrics_tbl)) {
              metrics_list$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
            } else { warning("Training metrics calculation failed.") }
          } else {
            warning("Fitted values length mismatch or NULL. Skipping train metrics.")
          }

          # --- Test Metrics ---
          if (nrow(test_df) > 0) {
            test_actual <- test_df$y

            # Align forecast with test actuals based on date 'ds'
            test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)

            if (nrow(test_pred_df) == nrow(test_df)) {
              # Ensure order matches test_df$ds for correct comparison

              test_pred_ordered_df <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]
              test_pred <- test_pred_ordered_df$yhat

              if(all(!is.na(test_pred))){
                test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
                if(!is.null(test_metrics_tbl)){
                  metrics_list$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)

                } else { message("Test metrics calculation failed.") } # Generic Label
              } else {
                message("Test predictions contain NAs. Skipping.") # Generic Label
              }
            } else {
              warning("Could not align test predictions with actuals (check forecast dates/length). Skipping test metrics.")
              message(paste0("Could not align test predictions (count mismatch). Filtered=", nrow(test_pred_df), ", Actual=", nrow(test_df), "). Skipping test metrics."))
            }
          } else {
            # No test set, maybe add a note to metrics output?
            # For now, just don't add test metrics to the list
            message("No test data found.") # Generic Label
          }
          # Combine metrics and store
          if (length(metrics_list) > 0) {
            r$metrics_summary <- bind_rows(metrics_list) %>%
              select(Model, DataSet, .metric, .estimate) # Ensure column order
          } else {
            r$metrics_summary <- NULL # Set to null if no metrics calculated
            shiny::showNotification("Metrics calculation skipped or failed.", type="warning")
          }

          # --- 4. Update Results ---
          r$forecast_df <- forecast_tibble # Update reactive value for plot
          r$model_name <- model_name      # Store which model was run
          r$forecast_obj <- model_or_fcst_obj
          model_success <- TRUE
          message(paste("--- Exiting", model_name, "Block Successfully ---"))
          shiny::incProgress(1.0, detail = "Done.")

        }, # End tryCatch block
        error = function(e) { # Error handler for tryCatch
          # Clear results on any error during the process
          r$forecast_obj <- NULL
          r$forecast_df <- NULL
          r$metrics_summary <- NULL
          r$model_name <- NULL
          r$arima_selected_order <- NULL
          r$arima_used_frequency <- NULL
          model_success <<- FALSE
          # Print error object to console for debugging
          print(paste("ERROR during forecast execution:", Sys.time()))
          print("--- Full Error Object ---")
          print(e) # Print the whole error object 'e'
          print("--- End Error Object ---")

          shiny::showNotification(
            paste("Error during forecast:", conditionMessage(e)), # Still show message in UI
            type = "error",
            duration = 15
          )
        }
      ) # End tryCatch
    }) # End withProgress

    # Final notification outside withProgress based on success
    if(model_success) {
      shiny::showNotification(paste(r$model_name, "forecast complete."), type = "message", duration = 5)
    } else {
      # Error notification already shown by tryCatch
    }

    # --- Reset File Inputs AFTER execution attempt (success or fail) ---
    # Use the specific namespaced IDs
    # message("Attempting to reset file inputs...") # Log the attempt
    shinyjs::reset("model_config_1-prophet_holidays_file")
    shinyjs::reset("model_config_1-prophet_regressors_file")
    # You could potentially reset the main data upload too if desired:
    # shinyjs::reset("data_input_1-fileUpload")
    # --- End Reset ---


  }) # End observeEvent

} # End app_server
