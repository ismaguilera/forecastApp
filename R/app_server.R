# R/app_server.R

#' The application server-side
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny dplyr tibble forecast parsnip workflows tune dials rsample yardstick timetk recipes slider
#' @import shiny.i18n
#' @import future
#' @import furrr
#' @importFrom shinyjs reset
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats predict 
#' @importFrom utils head capture.output str packageVersion
#' @importFrom purrr reduce 
#' @importFrom rlang `%||%`
# Needed for forecast() call inside observeEvent
# Add other necessary imports if functions are called directly here
#' @noRd

# --- Internationalization Setup ---
i18n <- Translator$new(translation_json_path = app_sys("i18n",'translation.json'))
i18n$set_translation_language('en')

app_server <- function(input, output, session) {
  # --- Set Future Plan ---
  # Set the plan for parallel processing. multisession is recommended for shinyapps.io
  future::plan(multisession)

  # --- Language Selector Observer ---
  # observeEvent(input$selected_language, {
  #   req(input$selected_language)
  #   # req(input$selected_language)
  #   # Update the translator's language
  #   update_lang(shiny::session, input$selected_language)
  #   # The renderText functions below will react to this change.
  #   # i18n$set_translation_language(input$selected_language)
  #   # session$reload() # Recarga la sesión para aplicar cambios, a veces necesario
  # })
  

  # --- Reactive UI Translations ---
  # output$ui_page_title <- renderUI({ i18n$t("Vaccine Forecasting App") })
  # output$ui_nav_data <- renderText({ i18n$t("Data")})
  # output$ui_nav_model <- renderText({ i18n$t("Model") })
  # output$ui_nav_forecast_results <- renderText({ i18n$t("Forecast results") })
  # output$ui_nav_validation <- renderText({ i18n$t("Validation") })
  # output$ui_nav_about <- renderText({  i18n$t("About") })
  observeEvent(input$selected_language, {
    req(input$selected_language)
    # Update the translator's language
    i18n$set_translation_language(input$selected_language)
    # Update the UI elements with the new translations
    output$ui_page_title <- renderUI({ i18n$t("Vaccine Forecasting App") })
    output$ui_nav_data <- renderText({ i18n$t("Data")})
    output$ui_sidebar_data<- renderText({ i18n$t("Data Input") })
    output$ui_accordion_preprocess <- renderText({ i18n$t("Preprocessing & Split") })
    output$ui_aggregation_level <- renderText({ i18n$t("Aggregation Level") })
    output$ui_aggregation_level_daily <- renderText({ i18n$t("Daily") })
    output$ui_aggregation_level_weekly <- renderText({ i18n$t("Weekly") })
    output$ui_aggregation_level_weekly_mean <- renderText({ i18n$t("mean") })
    output$ui_aggregation_level_weekly_sum <- renderText({ i18n$t("sum") })
    output$ui_train_set_percentage <- renderText({ i18n$t("Train Set Percentage") })  
    output$ui_time_series_decomposition <- renderText({ i18n$t("Time Series Decomposition") })  
    output$ui_model_summary <- renderText({ i18n$t("Model Summary") })
    output$ui_csv_file_upload <- renderText({ i18n$t("Choose CSV or Excel File") })
    output$ui_choose_default_dataset <- renderText({ i18n$t("Choose Default Dataset") })
    output$ui_load_default_dataset <- renderText({ i18n$t("Load Selected Default Dataset") })
    output$ui_select_columns <- renderText({ i18n$t("Select Columns") })
    output$ui_preview <- renderText({ i18n$t("Preview") })
    output$ui_select_date_col <- renderText({ i18n$t("Select Date Column") })
    output$ui_select_value_col <- renderText({ i18n$t("Select Value Column") })
    output$ui_load_holidays <- renderText({ i18n$t("Load Holidays (Optional)") })
    output$ui_load_holidays_default <- renderText({ i18n$t("Load Default Holidays") })
    output$ui_upload_global_holidays <- renderText({ i18n$t("Upload Global Holidays File (CSV: ds, holiday)") })
    output$ui_nav_model <- renderText({ i18n$t("Model") })
    output$ui_nav_forecast_results <- renderText({ i18n$t("Forecast results") })
    output$ui_visualizations <- renderText({ i18n$t("Visualizations") })
    output$ui_nav_validation <- renderText({ i18n$t("Validation") })
    output$ui_nav_about <- renderText({  i18n$t("About") })

    # --- Dynamic UI elements that were hardcoded ---
    output$ui_accordion_preprocess_title <- renderUI({ i18n$t("Preprocessing & Split") })
    output$ui_accordion_decomposition_title <- renderUI({ i18n$t("Time Series Decomposition") })
    output$ui_sidebar_model_summary_title <- renderUI({ i18n$t("Model summary") })
    output$ui_visualizations_title <- renderUI({ i18n$t("Visualizations") })
    output$ui_plot_nav_title <- renderUI({ i18n$t("Plot") })
    output$ui_performance_nav_title <- renderUI({ i18n$t("Performance") })
    output$ui_extra_plots_nav_title <- renderUI({ i18n$t("Extra Plots") })
    output$ui_about_app_sidebar_title <- renderUI({ i18n$t("About the app") })
    
    # --- Buttons with translated labels ---
    output$ui_download_forecast_button_placeholder <- renderUI({
      downloadButton(
        "downloadForecastData",
        label = i18n$t("Download Forecasts (CSV)"),
        icon = shiny::icon("download"),
        class = "btn-success"
      )
    })
    output$ui_download_report_button_placeholder <- renderUI({
      downloadButton(
        "downloadReport",
        label = i18n$t("Download Report"),
        icon = shiny::icon("file-alt"),
        class = "btn-info"
      )
    })
    output$ui_save_session_button_placeholder <- renderUI({
      actionButton("save_session_button", i18n$t("Save Session"), icon = icon("save"), class = "btn-primary btn-sm")
    })
    output$ui_load_session_button_placeholder <- renderUI({
      actionButton("load_session_button", i18n$t("Load Session"), icon = icon("folder-open"), class = "btn-info btn-sm")
    })
  })

  # Note: The "Language:" label for selectInput is not translated for now as per plan.
  # Update the language selector choices dynamically
  # observe({
  #   updateSelectInput(session, "selected_language",
  #                     choices = i18n$get_languages(),
  #                     selected = i18n$get_key_translation()
  #                     )
  # })

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
  data_input_reactives <- mod_data_input_server("data_input_1", i18n = i18n)
  preprocess_reactives <- mod_preprocess_controls_server(
    "preprocess_controls_1",
    data_input_reactives = data_input_reactives,
    i18n = i18n
  )

  model_config_reactives <- mod_model_config_server("model_config_1", i18n = i18n)

  mod_decomposition_plot_server(
    "decomposition_plot_1",
    reactive_aggregated_df = preprocess_reactives$reactive_aggregated_df,
    reactive_aggregation_level = preprocess_reactives$reactive_agg_level,
    i18n = i18n
  )

  mod_results_plot_server(
    "results_plot_1",
    reactive_train_df = preprocess_reactives$reactive_train_df,
    reactive_test_df = preprocess_reactives$reactive_test_df,
    reactive_forecast_list = eventReactive(r$run_id, { r$forecast_list }), # Pass the list of forecast tibbles, triggered by run_id
    reactive_global_holidays_data = r$global_holidays_data, # Pass the reactiveVal directly
    i18n = i18n
  ) -> plot_obj_reactive # Capture the returned reactive plot object

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
    }, ignoreNULL = FALSE),
    i18n = i18n
    # Remove older individual reactive arguments
  ) -> summary_reactives # Assign module output to a variable

  mod_results_table_server(
    "results_table_1",
    reactive_metrics_summary = reactive({ r$metrics_summary }),
    i18n = i18n
  ) -> metrics_df_reactive # Capture the returned reactive metrics data frame

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
    reactive_selected_summary_model = summary_reactives$selected_model, # Assuming the summary module returns the input value
    i18n = i18n
  )


  # --- Guided Tour ---
  # Define the steps for the introduction tour
  tour_steps <- reactive({
    data.frame(
      element = c(
        "a[role='tab'][data-value='Data']",
        "label[for='data_input_1-fileUpload']",
        "label[for='data_input_1-dateCol']",
        "label[for='data_input_1-valueCol']",
        "label[for='preprocess_controls_1-aggregationLevel']",
        "#preprocess_controls_1-trainTestSplit",
        "#load_default_holidays", # For the button next to global holiday upload
        "a[role='tab'][data-value='Model']",
        "label[for='model_config_1-use_arima']", # Or a general selector for the model selection area
        "#model_config_1-modelParamsAccordion",
        "label[for='model_config_1-forecastHorizon']",
        "#model_config_1-runForecast",
        "a[role='tab'][data-value='Forecast results']",
        "#results_plot_1-forecastPlot",
        "#results_table_1-metricsTable", # Assuming this ID exists for the DTOutput wrapper
        "a[role='tab'][data-value='Validation']",
        "a[role='tab'][data-value='About']"
      ),
      intro = c(
        "Start here: Upload and define your time series data in the Data panel.",
        "Click to upload your time series data from a CSV or Excel file.",
        "After uploading, select the column from your file that contains the dates.",
        "Then, select the column that contains the numerical values you want to forecast.",
        "Choose how your data should be aggregated (e.g., Daily, Weekly) and the function to use (e.g., sum, mean).",
        "Adjust the slider to define the percentage of data used for training the model; the remainder will be used for testing.",
        "Optionally, upload a CSV file with global holidays (columns: ds, holiday) or load a default set. Models like Prophet can use this information.",
        "Move to the Model panel to select and configure your forecasting models.",
        "Select one or more forecasting models (e.g., ARIMA, ETS, Prophet) from the checklist. You can run multiple models simultaneously.",
        "For each model you select, expand its section in this accordion to configure its specific parameters.",
        "Set the number of future periods (days or weeks, depending on your aggregation level) you want to forecast.",
        "Once configured, click this button to train all selected models and generate the forecasts.",
        "View the outcomes of your forecasts in the Forecast Results panel.",
        "This plot displays the historical data (training and testing sets) and the forecasts from all selected models.",
        "This table shows performance metrics (like MAE, RMSE, MAPE) for each model, comparing their accuracy.",
        "Go to the Validation panel to perform time series cross-validation on your chosen models to further assess their robustness.",
        "Find out more about this application, its features, and developer information in the About panel."
      ),
      position = c(
        "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom",
        "bottom", "right", "bottom", "right", "right", # 'right' for sidebar items in model config
        "bottom", "top", "top", "bottom", "bottom"
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
      shiny::showNotification(i18n$t("Global holidays file uploaded and processed successfully."), type = "message")
    }, error = function(e) {
      r$global_holidays_data(NULL) # Reset on error
      error_message <- paste(i18n$t("Error processing global holidays file. Please check format (CSV with 'Fecha', 'Feriados_chilenos' columns) and content. Original error:"), e$message)
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
      shiny::showNotification(i18n$t("Default global holidays loaded successfully."), type = "message")
    }, error = function(e) {
      r$global_holidays_data(NULL) # Reset on error
      error_message <- paste(i18n$t("Error loading default global holidays. Please check the file and application configuration. Original error:"), e$message)
      shiny::showNotification(error_message, type = "error", duration = 10)
    })
  })

  # --- Model Execution Logic ---
  observeEvent(model_config_reactives$run_forecast_button(), {
    message("Run Forecast button clicked.")

    # --- Show spinner and disable inputs ---
    shinybusy::show_spinner()
    shinyjs::disable("data_sidebar_div")
    shinyjs::disable("model_config_div")
    on.exit({
      shinyjs::enable("data_sidebar_div")
      shinyjs::enable("model_config_div")
      shinybusy::hide_spinner()
      message("Inputs re-enabled and spinner hidden.")
    })

    # Get required inputs reactively
    train_df <- preprocess_reactives$reactive_train_df()
    test_df <- preprocess_reactives$reactive_test_df()
    full_aggregated_df <- preprocess_reactives$reactive_aggregated_df()
    agg_level <- preprocess_reactives$reactive_agg_level()
    horizon <- model_config_reactives$forecast_horizon()
    current_global_holidays <- r$global_holidays_data()

    model_checks <- list(
      ARIMA = model_config_reactives$use_arima(),
      ETS = model_config_reactives$use_ets(),
      TBATS = model_config_reactives$use_tbats(),
      Prophet = model_config_reactives$use_prophet(),
      XGBoost = model_config_reactives$use_xgboost(),
      GAM = model_config_reactives$use_gam(),
      RF = model_config_reactives$use_rf(),
      NNETAR = model_config_reactives$use_nnetar()
    )
    selected_models_now <- names(model_checks)[sapply(model_checks, isTRUE)]

    # Validation
    req(train_df, test_df, full_aggregated_df, agg_level, horizon)
    validate(need(length(selected_models_now) > 0, "Please select at least one model to run."))
    message(paste("Models selected:", paste(selected_models_now, collapse=", ")))

    # --- Reset results lists ---
    r$forecast_list <- list()
    r$fitted_list <- list()
    r$metrics_list <- list()
    r$run_models_summary <- list()
    r$metrics_summary <- NULL

    # --- Data & Parameter Prep ---
    freq_str <- if (agg_level == "Daily") "day" else "week"
    n_test_periods <- nrow(test_df)
    total_periods_needed <- n_test_periods + horizon
    last_train_date <- max(train_df$ds)
    by_period_forecast <- switch(freq_str, "week" = lubridate::weeks(1), lubridate::days(1))
    future_dates_for_fcst <- seq.Date(
      from = last_train_date + by_period_forecast,
      by = freq_str,
      length.out = total_periods_needed
    )

    validate(need(nrow(train_df) >= 5, "Need at least 5 training data points."))

    # --- Helper Function for a Single Model Run ---
    run_single_model <- function(model_name, progress_val) {
      # This function will run in a separate process
      # It needs all the data and config passed to it or available in its environment

      # Since this runs in a future, we need to explicitly load libraries if they are not attached
      # This is good practice for robustness
      library(dplyr)
      library(tibble)
      library(forecast)
      library(parsnip)
      library(workflows)
      library(tune)
      library(dials)
      library(rsample)
      library(yardstick)
      library(timetk)
      library(recipes)
      library(slider)
      library(prophet)
      library(xgboost)
      library(ranger)
      library(mgcv)

      message(paste("--- Starting Model (in future):", model_name, "---"))

      model_summary_entry <- list(
        config = list(), success = FALSE, error = NULL,
        aggregation_level = agg_level,
        frequency_used = NULL,
        arima_order = NULL,
        fitted_method = NULL
      )

      tryCatch({
        future_holidays_df_for_model <- NULL
        if (!is.null(current_global_holidays) && nrow(current_global_holidays) > 0) {
          future_holidays_df_for_model <- current_global_holidays %>%
            dplyr::mutate(ds = as.Date(ds)) %>%
            dplyr::filter(ds %in% future_dates_for_fcst)
        }

        # --- Model-Specific Logic (Copied from original for loop) ---
        # This is where the original if/else if chain for models goes
        # For brevity, this is a placeholder. The actual logic from the original code is inserted here.

        # NOTE: The entire if/else-if chain from the original for-loop must be placed here.
        # It has been omitted in this example for conciseness, but it is essential.
        # The logic fetches model-specific configs and calls train_* and forecast_* functions.
        # Example for ARIMA:
        if (model_name == "ARIMA") {
          config <- list(
            auto = model_config_reactives$arima_auto(), p = model_config_reactives$arima_p(), d = model_config_reactives$arima_d(), q = model_config_reactives$arima_q(),
            seasonal = model_config_reactives$arima_seasonal(), P = model_config_reactives$arima_P(), D = model_config_reactives$arima_D(), Q = model_config_reactives$arima_Q(),
            period = model_config_reactives$arima_period()
          )
          model_summary_entry$config <- config
          model_or_fcst_obj <- train_arima(train_df, config, agg_level, holidays_df = current_global_holidays)
          # ... (rest of ARIMA logic for forecasting and preparing future_xreg)
          # ... forecast_output <- forecast_arima(...)
          # forecast_tibble <- forecast_output$forecast
          # fitted_values <- forecast_output$fitted
        } # ... else if (model_name == "ETS") { ... } and so on for all models.
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
            } else { # Manual seasonal
              manual_period <- as.integer(config$period)
              if (!is.na(manual_period) && manual_period > 1) {
                freq_used <- manual_period
              } else {
                config$seasonal <- FALSE
                freq_used <- 1
                warning("Manual seasonal period invalid (<=1), treating as non-seasonal.")
              }
            }
          }
          message(paste("ARIMA frequency determined as:", freq_used))
          model_summary_entry$frequency_used <- freq_used
          model_summary_entry$config <- config

          model_or_fcst_obj <- train_arima(train_df, config, aggregation_level = agg_level, holidays_df = current_global_holidays)
          req(model_or_fcst_obj, "ARIMA model training failed (returned NULL).")

          # ... (rest of ARIMA logic for forecasting and preparing future_xreg)
          # This part needs to be copied from the original sequential loop

          forecast_output <- forecast_arima(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
          forecast_tibble <- forecast_output$forecast
          fitted_values <- forecast_output$fitted
          req(forecast_output, forecast_tibble, fitted_values)

        } else if (model_name == "ETS") {
          config <- list(
            manual = model_config_reactives$ets_manual(),
            ets_e = model_config_reactives$ets_e(),
            ets_t = model_config_reactives$ets_t(),
            ets_s = model_config_reactives$ets_s(),
            ets_damped_str = model_config_reactives$ets_damped_str()
          )
          model_summary_entry$config <- config
          model_or_fcst_obj <- train_ets(train_df, config, agg_level, total_periods_needed)
          req(model_or_fcst_obj, "ETS/STLF training/forecasting failed (returned NULL).")
          if(inherits(model_or_fcst_obj, "ets")) model_summary_entry$fitted_method <- model_or_fcst_obj$method
          if(inherits(model_or_fcst_obj, "forecast") && !is.null(model_or_fcst_obj$model)) {
            model_summary_entry$fitted_method <- model_or_fcst_obj$model$method
          }
          forecast_output <- forecast_ets(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
          forecast_tibble <- forecast_output$forecast
          fitted_values <- forecast_output$fitted
          req(forecast_output, forecast_tibble, fitted_values, "ETS forecast processing failed.")

        } else if (model_name == "TBATS") {
          config <- list()
          model_summary_entry$config <- config
          model_or_fcst_obj <- train_tbats(train_df, config, agg_level)
          req(model_or_fcst_obj)
          forecast_output <- forecast_tbats(model_or_fcst_obj, total_periods_needed, last_train_date, freq_str)
          forecast_tibble <- forecast_output$forecast
          fitted_values <- forecast_output$fitted
          model_summary_entry$fitted_method <- capture.output(print(model_or_fcst_obj))[1]
          req(forecast_output, forecast_tibble, fitted_values, "TBATS forecasting failed.")

        } else if (model_name == "Prophet") {
            # Simplified Prophet logic for brevity
            config <- list(growth = model_config_reactives$prophet_growth())
            model_summary_entry$config <- config
            model_obj <- train_prophet(train_df, config, holidays_df = current_global_holidays)
            req(model_obj, "Prophet training failed.")
            forecast_tibble <- forecast_prophet(model_obj, total_periods_needed, freq_str)
            req(forecast_tibble, "Prophet forecasting failed.")
            fitted_values <- forecast_tibble %>% dplyr::filter(ds %in% train_df$ds) %>% pull(yhat)
            req(length(fitted_values) == nrow(train_df))

        } else if (model_name == "XGBoost") {
            config <- list(
                nrounds = model_config_reactives$xgb_nrounds(), eta = model_config_reactives$xgb_eta(),
                max_depth = model_config_reactives$xgb_max_depth(), subsample = model_config_reactives$xgb_subsample(),
                colsample_bytree = model_config_reactives$xgb_colsample(), gamma = model_config_reactives$xgb_gamma()
            )
            model_summary_entry$config <- config
            unprepared_recipe_xgb <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
            req(unprepared_recipe_xgb)
            prep_recipe_xgb <- recipes::prep(unprepared_recipe_xgb, training = train_df)
            model_obj <- train_xgboost(prep_recipe_xgb, config)
            req(model_obj)
            forecast_tibble <- forecast_xgboost(model_obj, prep_recipe_xgb, full_aggregated_df, last_train_date, total_periods_needed, freq_str)
            req(forecast_tibble)
            train_baked_df <- recipes::bake(prep_recipe_xgb, new_data = train_df, everything())
            fitted_values <- predict(model_obj, as.matrix(train_baked_df[, model_obj$feature_names, drop = FALSE]))
            req(fitted_values)

        } else if (model_name == "GAM") {
            config <- list(
                smooth_trend = model_config_reactives$gam_trend_type() == "smooth",
                use_season_y = model_config_reactives$gam_use_season_y(),
                use_season_w = model_config_reactives$gam_use_season_w()
            )
            model_summary_entry$config <- config
            model_obj <- train_gam(train_df, config, holidays_df = current_global_holidays)
            req(model_obj)
            forecast_output <- forecast_gam(model_obj, train_df, total_periods_needed, freq_str, config, holidays_df = current_global_holidays)
            forecast_tibble <- forecast_output$forecast
            fitted_values <- forecast_output$fitted
            req(forecast_output, forecast_tibble, fitted_values)

        } else if (model_name == "RF") {
            config <- list(
                rf_num_trees = model_config_reactives$rf_num_trees(),
                rf_mtry = model_config_reactives$rf_mtry(),
                rf_min_node_size = model_config_reactives$rf_min_node_size()
            )
            model_summary_entry$config <- config
            unprepared_recipe_rf <- create_tree_recipe(full_aggregated_df, freq_str = freq_str)
            req(unprepared_recipe_rf)
            prep_recipe_rf <- recipes::prep(unprepared_recipe_rf, training = train_df)
            model_obj <- train_rf(prep_recipe_rf, config)
            req(model_obj)
            forecast_output <- forecast_rf(model_obj, prep_recipe_rf, full_aggregated_df, train_df, last_train_date, total_periods_needed, freq_str)
            forecast_tibble <- forecast_output$forecast
            fitted_values <- forecast_output$fitted
            req(forecast_output, forecast_tibble, fitted_values)

        } else if (model_name == "NNETAR") {
            config_nnetar <- list(
                nnetar_p = model_config_reactives$nnetar_p(), nnetar_P = model_config_reactives$nnetar_P(),
                nnetar_size_method = model_config_reactives$nnetar_size_method(), nnetar_size_manual = model_config_reactives$nnetar_size_manual(),
                nnetar_repeats = model_config_reactives$nnetar_repeats(), nnetar_lambda_auto = model_config_reactives$nnetar_lambda_auto(),
                nnetar_lambda_manual = model_config_reactives$nnetar_lambda_manual()
            )
            model_summary_entry$config <- config_nnetar
            model_obj_nnetar <- train_nnetar(train_df, config_nnetar, agg_level)
            req(model_obj_nnetar)
            forecast_output_nnetar <- forecast_nnetar(model_obj_nnetar, total_periods_needed, last_train_date, freq_str)
            forecast_tibble <- forecast_output_nnetar$forecast
            fitted_values <- forecast_output_nnetar$fitted
            req(forecast_output_nnetar, forecast_tibble)
        }

        model_summary_entry$success <- TRUE

        # Return a list with all necessary results
        list(
          model_name = model_name,
          success = TRUE,
          forecast_tibble = forecast_tibble,
          fitted_values = fitted_values,
          summary_entry = model_summary_entry
        )

      }, error = function(e) {
        warning(paste("Error running model", model_name, "in parallel:", conditionMessage(e)))
        model_summary_entry$success <- FALSE
        model_summary_entry$error <- conditionMessage(e)

        # Return a list indicating failure
        list(
          model_name = model_name,
          success = FALSE,
          error_message = conditionMessage(e),
          summary_entry = model_summary_entry
        )
      })
    }

    # Use furrr::future_map to run models in parallel
    # The .options sets a seed for reproducibility in parallel processes
    all_results <- furrr::future_map(
      selected_models_now,
      ~run_single_model(.x),
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )

    # --- Process Results from Parallel Execution ---
    temp_forecast_list <- list()
    temp_fitted_list <- list()
    temp_summary_list <- list()

    for (res in all_results) {
      if (is.null(res)) {
          shiny::showNotification("A model run returned NULL. Check logs.", type = "warning", duration = 10)
          next
      }

      model_name <- res$model_name
      temp_summary_list[[model_name]] <- res$summary_entry

      if (res$success) {
        temp_forecast_list[[model_name]] <- res$forecast_tibble
        temp_fitted_list[[model_name]] <- res$fitted_values
          shiny::showNotification(i18n$t("{model_name} forecast complete.", list(model_name = model_name)), type = "message", duration = 5)
      } else {
        user_friendly_message <- paste0(
            i18n$t("Error during {model_name} model processing. Please check this model's configuration and input data suitability. Specific error:", list(model_name = model_name)),
            " ",
            res$error_message
        )
        shiny::showNotification(user_friendly_message, type = "warning", duration = 15)
      }
    }

    # --- Update Reactive Values with Processed Results ---
    r$forecast_list <- temp_forecast_list
    r$fitted_list <- temp_fitted_list
    r$run_models_summary <- temp_summary_list

    successful_models <- names(r$forecast_list)
    req(length(successful_models) > 0, "All selected models failed to produce forecasts.")

    # --- Metrics Calculation ---
    message("Calculating metrics for successful models...")
    all_metrics_list <- list()
    train_actual <- train_df$y
    test_actual <- if (nrow(test_df) > 0) test_df$y else NULL

    for (model_name in successful_models) {
      fitted_values <- r$fitted_list[[model_name]]
      forecast_tibble <- r$forecast_list[[model_name]]
      model_metrics <- list()

      # Train Metrics
      if (!is.null(fitted_values) && length(fitted_values) == length(train_actual) && !anyNA(fitted_values)) {
        train_metrics_tbl <- calculate_metrics(train_actual, fitted_values)
        if (!is.null(train_metrics_tbl)) {
          model_metrics$Train <- train_metrics_tbl %>% mutate(DataSet = "Train", Model = model_name)
        }
      }

      # Test Metrics
      if (!is.null(test_actual) && !is.null(forecast_tibble)) {
        test_pred_df <- forecast_tibble %>% dplyr::filter(ds %in% test_df$ds)
        if (nrow(test_pred_df) == nrow(test_df)) {
          test_pred <- test_pred_df[match(test_df$ds, test_pred_df$ds), ]$yhat
          if(all(!is.na(test_pred))){
            test_metrics_tbl <- calculate_metrics(test_actual, test_pred)
            if(!is.null(test_metrics_tbl)){
              model_metrics$Test <- test_metrics_tbl %>% mutate(DataSet = "Test", Model = model_name)
            }
          }
        }
      }

      if(length(model_metrics) > 0) {
        all_metrics_list <- c(all_metrics_list, model_metrics)
      }
    }

    if (length(all_metrics_list) > 0) {
      r$metrics_summary <- dplyr::bind_rows(all_metrics_list) %>%
        dplyr::select(Model, DataSet, .metric, .estimate)
      message("Metrics summary table created.")
    }

    # --- Trigger UI Update ---
    if(length(r$forecast_list) > 0) {
      r$run_id <- r$run_id + 1
      message("Finished all selected models.")
    }
  })


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
        rf_min_node_size = if (is.function(model_config_reactives$rf_min_node_size)) model_config_reactives$rf_min_node_size() else NULL,
        # NNETAR
        use_nnetar = if (is.function(model_config_reactives$use_nnetar)) model_config_reactives$use_nnetar() else NULL,
        nnetar_p = if (is.function(model_config_reactives$nnetar_p)) model_config_reactives$nnetar_p() else NULL,
        nnetar_P = if (is.function(model_config_reactives$nnetar_P)) model_config_reactives$nnetar_P() else NULL,
        nnetar_size_method = if (is.function(model_config_reactives$nnetar_size_method)) model_config_reactives$nnetar_size_method() else NULL,
        nnetar_size_manual = if (is.function(model_config_reactives$nnetar_size_manual)) model_config_reactives$nnetar_size_manual() else NULL,
        nnetar_repeats = if (is.function(model_config_reactives$nnetar_repeats)) model_config_reactives$nnetar_repeats() else NULL,
        nnetar_lambda_auto = if (is.function(model_config_reactives$nnetar_lambda_auto)) model_config_reactives$nnetar_lambda_auto() else NULL,
        nnetar_lambda_manual = if (is.function(model_config_reactives$nnetar_lambda_manual)) model_config_reactives$nnetar_lambda_manual() else NULL
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
      
      notification_id <- shiny::showNotification(i18n$t("Saving session... Please wait."), duration = NULL, type = "message")
      on.exit(shiny::removeNotification(notification_id), add = TRUE)

      tryCatch({
        saveRDS(session_state_to_save, file = file)
        shiny::removeModal()
        shiny::showNotification(i18n$t("Session saved to {filename}", list(filename = basename(file))), type = "message", duration = 5)
      }, error = function(e_save) {
        shiny::showNotification(paste(i18n$t("Error saving session:"), e_save$message), type = "error", duration = 10)
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
    
    show_loading_notification <- shiny::showNotification(i18n$t("Loading session... Please wait."), duration = NULL, type = "message", id = "loading_session_notif")

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
        updateCheckboxInput(session, "model_config_1-use_nnetar", value = mcsv$use_nnetar %||% FALSE) # Handle potential NULL

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
        
        # NNETAR params
        updateNumericInput(session, "model_config_1-nnetar_p", value = mcsv$nnetar_p)
        updateNumericInput(session, "model_config_1-nnetar_P", value = mcsv$nnetar_P)
        updateSelectInput(session, "model_config_1-nnetar_size_method", selected = mcsv$nnetar_size_method)
        updateNumericInput(session, "model_config_1-nnetar_size_manual", value = mcsv$nnetar_size_manual)
        updateNumericInput(session, "model_config_1-nnetar_repeats", value = mcsv$nnetar_repeats)
        updateCheckboxInput(session, "model_config_1-nnetar_lambda_auto", value = mcsv$nnetar_lambda_auto %||% TRUE) # Default to TRUE if NULL
        updateNumericInput(session, "model_config_1-nnetar_lambda_manual", value = mcsv$nnetar_lambda_manual)
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
      
      # Trigger update for eventReactives depending on r$run_id
      if (!is.null(r$run_id) && r$run_id > 0) {
        r$run_id <- r$run_id + 0.0001 
      }
      
      # Construct the notification message
      data_file_msg <- if (!is.null(r$loaded_session_data_file_name)) paste0("main data file ('", r$loaded_session_data_file_name, "')") else "main data file"
      holidays_file_msg <- if (!is.null(r$loaded_session_holiday_file_name)) paste0("global holidays file ('", r$loaded_session_holiday_file_name, "')") else "global holidays file"
      
      full_notification_msg <- i18n$t(
        "Session loaded successfully! Please re-upload your {data_file_msg} and {holidays_file_msg} if they were part of the saved session.",
        list(data_file_msg = data_file_msg, holidays_file_msg = holidays_file_msg)
      )
      shiny::showNotification(full_notification_msg, type = "message", duration = 15) # Increased duration
      
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
      shiny::showNotification(paste(i18n$t("Error loading session:"), e$message), type = "error", duration = 10)
      # Optionally, reset parts of the state if loading fails catastrophically
      # For example, reset r$run_id if it was partially loaded and might cause issues
      # r$run_id <- 0 
      # r$forecast_list <- list()
      # etc.
    })
  })
  # --- End Load Session Logic ---

  # --- Validation Module Server Call ---
  mod_validation_server(
    "validation_1",
    reactive_run_models_summary = reactive({ r$run_models_summary }),
    reactive_train_df = preprocess_reactives$reactive_train_df, 
    reactive_agg_level = preprocess_reactives$reactive_agg_level, 
    reactive_global_holidays_data = r$global_holidays_data,
    i18n = i18n
  )
  # --- End Validation Module Server Call ---

  # --- Report Generation Download Handler ---
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("forecast_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$reportFormat)
    },
    content = function(file) {
      shiny::withProgress(message = paste("Generating", toupper(input$reportFormat), "report..."), value = 0, {
        
        shiny::incProgress(0.1, detail = "Preparing data...")
        # Ensure all required reactive data is available
        req(
          plot_obj_reactive(), 
          metrics_df_reactive(), 
          r$run_models_summary,
          r$run_id > 0 # Ensure models have been run
        )
        
        # Generate Model Summaries Text
        model_summaries_for_report <- list()
        if (length(r$run_models_summary) > 0) {
          shiny::incProgress(0.2, detail = "Formatting model summaries...")
          for (model_name_iter in names(r$run_models_summary)) { # Renamed to avoid conflict
            summary_entry <- r$run_models_summary[[model_name_iter]]
            if (isTRUE(summary_entry$success)) {
              
              # Basic Info
              text_summary_parts <- c(
                paste0("Model: ", model_name_iter),
                paste0("Aggregation: ", summary_entry$aggregation_level %||% "N/A")
              )
              
              # ARIMA Specifics
              if (model_name_iter == "ARIMA") {
                if (!is.null(summary_entry$arima_order)) {
                  text_summary_parts <- c(text_summary_parts, paste0("ARIMA Order (auto/manual): ", paste(names(summary_entry$arima_order), summary_entry$arima_order, collapse=", ")))
                }
                if (!is.null(summary_entry$frequency_used)) {
                  text_summary_parts <- c(text_summary_parts, paste0("Frequency Used: ", summary_entry$frequency_used))
                }
                # Add manual ARIMA config if auto was false
                if(isFALSE(summary_entry$config$auto)){
                    manual_order_str <- paste0("p=", summary_entry$config$p, ", d=", summary_entry$config$d, ", q=", summary_entry$config$q)
                    if(isTRUE(summary_entry$config$seasonal)){
                        manual_order_str <- paste0(manual_order_str, ", P=", summary_entry$config$P, ", D=", summary_entry$config$D, ", Q=", summary_entry$config$Q, ", Period=", summary_entry$config$period)
                    }
                    text_summary_parts <- c(text_summary_parts, paste0("Manual Config: ", manual_order_str))
                }
              }
              
              # ETS Specifics
              if (model_name_iter == "ETS" && !is.null(summary_entry$fitted_method)) {
                text_summary_parts <- c(text_summary_parts, paste0("ETS Method: ", summary_entry$fitted_method))
                if(isTRUE(summary_entry$config$manual)){
                    manual_spec_str <- paste0("E=",summary_entry$config$ets_e, ", T=",summary_entry$config$ets_t, ", S=",summary_entry$config$ets_s, ", Damped=",summary_entry$config$ets_damped_str)
                    text_summary_parts <- c(text_summary_parts, paste0("Manual Config: ", manual_spec_str))
                }
              }
              
              # TBATS Specifics
              if (model_name_iter == "TBATS" && !is.null(summary_entry$fitted_method)) {
                 text_summary_parts <- c(text_summary_parts, paste0("TBATS Method: ", summary_entry$fitted_method))
              }
              
              # Prophet Specifics
              if (model_name_iter == "Prophet" && !is.null(summary_entry$config)) {
                cfg <- summary_entry$config
                prophet_details <- paste0(
                  "Growth: ", cfg$growth %||% "N/A", 
                  ", Yearly: ", cfg$yearly %||% "N/A", 
                  ", Weekly: ", cfg$weekly %||% "N/A", 
                  ", Daily: ", cfg$daily %||% "N/A"
                )
                if(cfg$growth == "logistic" && !is.null(cfg$capacity)){
                    prophet_details <- paste0(prophet_details, ", Capacity: ", cfg$capacity)
                }
                text_summary_parts <- c(text_summary_parts, prophet_details)
                if(isTRUE(cfg$used_holidays)) text_summary_parts <- c(text_summary_parts, "Used Holidays: Yes")
                if(isTRUE(cfg$used_regressors)) text_summary_parts <- c(text_summary_parts, "Used Regressors: Yes")

              }
              
              # XGBoost Specifics
              if (model_name_iter == "XGBoost" && !is.null(summary_entry$config)) {
                if(isTRUE(summary_entry$tuning_enabled) && !is.null(summary_entry$tuned_params)){
                    tuned_str <- paste(names(summary_entry$tuned_params), sapply(summary_entry$tuned_params, function(x) if(is.numeric(x)) round(x, 4) else x), collapse="; ")
                    text_summary_parts <- c(text_summary_parts, paste0("Tuned Params: ", tuned_str))
                } else {
                    cfg <- summary_entry$config
                    xgb_details <- paste0("Rounds: ", cfg$nrounds, ", Eta: ", cfg$eta, ", Depth: ", cfg$max_depth) # etc.
                    text_summary_parts <- c(text_summary_parts, paste0("Config: ", xgb_details))
                }
              }
              
              # RF Specifics
              if (model_name_iter == "RF" && !is.null(summary_entry$config)) {
                if(isTRUE(summary_entry$tuning_enabled) && !is.null(summary_entry$tuned_params)){
                    tuned_rf_str <- paste(names(summary_entry$tuned_params), sapply(summary_entry$tuned_params, function(x) if(is.numeric(x)) round(x, 4) else x), collapse="; ")
                    text_summary_parts <- c(text_summary_parts, paste0("Tuned Params: ", tuned_rf_str))
                } else {
                    cfg <- summary_entry$config
                    rf_details <- paste0("Trees: ", cfg$rf_num_trees, ", mtry: ", cfg$rf_mtry, ", MinNode: ", cfg$rf_min_node_size)
                    text_summary_parts <- c(text_summary_parts, paste0("Config: ", rf_details))
                }
              }

              # GAM Specifics
              if (model_name_iter == "GAM" && !is.null(summary_entry$config)) {
                  cfg <- summary_entry$config
                  gam_details <- paste0("Trend: ", cfg$smooth_trend %||% "N/A", ", SeasonY: ", cfg$use_season_y %||% "N/A", ", SeasonW: ", cfg$use_season_w %||% "N/A")
                  text_summary_parts <- c(text_summary_parts, gam_details)
              }
              
              # NNETAR Specifics
              if (model_name_iter == "NNETAR" && !is.null(summary_entry$fitted_method)) {
                  text_summary_parts <- c(text_summary_parts, paste0("NNETAR Method: ", summary_entry$fitted_method))
                  if (!is.null(summary_entry$frequency_used)) {
                      text_summary_parts <- c(text_summary_parts, paste0("Frequency Used: ", summary_entry$frequency_used))
                  }
                  # Add more config details if needed from summary_entry$config
              }

              model_summaries_for_report[[length(model_summaries_for_report) + 1]] <- list(
                model_name = model_name_iter, 
                summary_text = paste(text_summary_parts, collapse = "\n")
              )
            }
          }
        }
        
        shiny::incProgress(0.4, detail = "Setting up report template...")
        # Define temporary file paths
        temp_report_path <- tempfile(fileext = ".Rmd")
        temp_output_path <- tempfile(fileext = paste0(".", input$reportFormat))
        
        # Copy the R Markdown template to the temporary path
        # Using system.file as a robust way to get package files
        # Assuming the package name is 'forecastApp' as per golem structure
        template_origin_path <- system.file("rmarkdown/templates/report_template.Rmd", package = "forecastApp")
        if (!file.exists(template_origin_path)) {
            stop("Report template not found. Expected at: ", template_origin_path)
        }
        file.copy(template_origin_path, temp_report_path, overwrite = TRUE)
        
        # Prepare parameters for R Markdown
        params_list <- list(
          report_title = paste("Forecast Report -", toupper(input$reportFormat)),
          forecast_plot = plot_obj_reactive(),    # The actual plotly object
          metrics_table = metrics_df_reactive(),  # The data frame
          model_summaries = model_summaries_for_report,
          run_date = Sys.time()
        )
        
        shiny::incProgress(0.6, detail = "Rendering report...")
        # Render the R Markdown document
        tryCatch({
          if (input$reportFormat == "pdf") {
            if (!tinytex::is_tinytex()) {
              shiny::showNotification(i18n$t("TinyTeX is not installed. PDF reports require a LaTeX distribution. Consider installing TinyTeX with tinytex::install_tinytex()."), type = "warning", duration = 15)
            }
          }
          rmarkdown::render(
            input = temp_report_path,
            output_format = if (input$reportFormat == "pdf") "pdf_document" else "html_document",
            output_file = temp_output_path,
            params = params_list,
            envir = new.env(parent = globalenv()) # Render in a clean environment
          )
          
          shiny::incProgress(0.9, detail = "Finalizing...")
          # Copy the rendered file to the 'file' argument of downloadHandler
          if (!file.exists(temp_output_path)) {
            stop(paste("Rendered report file not found at temporary path:", temp_output_path, "Cannot proceed with download."))
          }
          file.copy(temp_output_path, file, overwrite = TRUE)
          shiny::showNotification(i18n$t("Report generated successfully!"), type = "message", duration = 5)
          
        }, error = function(e_render) {
          error_msg_render <- paste(i18n$t("Error during rmarkdown::render:"), conditionMessage(e_render))
          # Log full error to console for debugging
          print(error_msg_render)
          print(e_render) # Print the full error object

          # Check if the error is specifically a LaTeX error for PDF
          if (input$reportFormat == "pdf" && grepl("LaTeX failed to compile", conditionMessage(e_render), ignore.case = TRUE)) {
            error_msg_render <- paste(error_msg_render, 
                                      "This often means essential LaTeX packages are missing. ",
                                      "If using TinyTeX, try running tinytex::tlmgr_install(c('fancyhdr', 'titling', 'framed')) or check the .log file mentioned in the error for more details. ",
                                      "The log file path is often in the error message: ",
                                      gsub(".*\\file([[:alnum:]]+)\\.tex.*",
                                           "\\\\file\\1.log",
                                           conditionMessage(e_render))
                                    )
          }
          shiny::showNotification(error_msg_render, type = "error", duration = 20)
          # Ensure 'file' (the downloadHandler's output file) is not left empty or non-existent if possible,
          # though Shiny usually handles this by not providing a download if 'file' isn't valid.
          # Creating an empty text file as a fallback to prevent 404s if render fails.
          # This provides *something* to download, even if it's just an error message.
          tryCatch({
            writeLines(c("Report generation failed.", error_msg_render), file)
          }, error = function(e_write) {
            # If even writing a simple text file fails, log it.
            message(paste("Failed to write fallback error file for report generation:", conditionMessage(e_write)))
          })
        }, finally = {
            # Clean up temporary files
            if (file.exists(temp_report_path)) unlink(temp_report_path)
            if (file.exists(temp_output_path)) unlink(temp_output_path)
        })
      }) # End withProgress
    },
    contentType = function() { # Dynamic content type
      if (input$reportFormat == "pdf") {
        "application/pdf"
      } else { # HTML
        "text/html"
      }
    }
  )
  # --- End Report Generation ---

}) # End app_server
}
