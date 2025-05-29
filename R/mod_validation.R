# R/mod_validation.R

#' Validation Module UI
#'
#' @param id Module ID.
#' @noRd
#' @import shiny bslib DT
mod_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::nav_panel(
      title = "Validation",
      icon = shiny::icon("check-circle"), # Bootstrap icon for validation
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "CV Controls",
          width = "300px", # Adjust width as needed
          uiOutput(ns("cv_model_selector_ui")), # Dynamic UI for model selection
          hr(),
          h5("Cross-Validation Parameters:"),
          numericInput(ns("cv_initial_window"), "Initial Training Window (periods):", value = 90, min = 10), # e.g. 90 days
          numericInput(ns("cv_horizon"), "Forecast Horizon (periods per fold):", value = 30, min = 1), # e.g. 30 days
          numericInput(ns("cv_skip"), "Skip (periods between folds):", value = 15, min = 0), # e.g. 15 days
          checkboxInput(ns("cv_cumulative"), "Cumulative Training Window?", value = FALSE),
          actionButton(ns("run_cv_button"), "Run Cross-Validation", icon = icon("play"), class = "btn-primary btn-block")
        ), # End sidebar
        # Main panel for CV results
        h4("Cross-Validation Results"),
        bslib::card(
          bslib::card_header("Cross-Validation Mean Metrics"),
          DT::dataTableOutput(ns("cv_results_table_output"))
        ),
        bslib::card(
          bslib::card_header("Cross-Validation Metric Distributions"),
          plotOutput(ns("cv_results_plot_output"))
        )
      ) # End layout_sidebar
    ) # End nav_panel
  )
}

#' Validation Module Server
#'
#' @param id Module ID.
#' @param reactive_run_models_summary Reactive expression returning the run_models_summary list.
#' @param reactive_train_df Reactive expression returning the training dataframe.
#' @param reactive_agg_level Reactive expression returning the aggregation level.
#' @param reactive_global_holidays_data ReactiveVal containing global holidays data.
#'
#' @noRd
#' @import shiny dplyr tibble timetk yardstick ggplot2 forecast recipes mgcv ranger xgboost
#' @importFrom stats predict ts frequency as.formula
#' @importFrom rlang `%||%`
mod_validation_server <- function(id, reactive_run_models_summary, reactive_train_df, reactive_agg_level, reactive_global_holidays_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Recommended way to get ns in module server

    # Internal reactiveVals for storing CV results
    cv_results_data_rv <- reactiveVal(NULL)
    cv_raw_metrics_list_rv <- reactiveVal(NULL)

    output$cv_model_selector_ui <- renderUI({
      req(reactive_run_models_summary())
      model_summaries <- reactive_run_models_summary()
      # Filter out models that failed or were not run
      successful_model_names <- names(model_summaries)[sapply(model_summaries, function(x) isTRUE(x$success))]
      req(length(successful_model_names) > 0)
      
      checkboxGroupInput(ns("cv_model_selection_input"), # Use ns() here
                         "Select Models for CV:",
                         choices = successful_model_names,
                         selected = successful_model_names)
    })

    observeEvent(input$run_cv_button, {
      # Get Inputs
      selected_cv_models <- input$cv_model_selection_input
      initial_window_periods <- input$cv_initial_window
      horizon_periods <- input$cv_horizon
      skip_periods <- input$cv_skip
      is_cumulative <- input$cv_cumulative
      
      full_train_data <- reactive_train_df()
      agg_level <- reactive_agg_level()
      freq_str_cv <- if (agg_level == "Daily") "day" else "week"
      current_global_holidays_cv <- reactive_global_holidays_data()

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

      all_cv_metrics <- list()

      shiny::withProgress(message = 'Running Cross-Validation...', value = 0, {
        n_total_iterations <- length(selected_cv_models) * nrow(ts_cv_splits)
        progress_counter <- 0

        for (model_name_cv in selected_cv_models) {
          model_summary_list_val <- reactive_run_models_summary()
          model_config_original <- model_summary_list_val[[model_name_cv]]$config
          original_agg_level <- model_summary_list_val[[model_name_cv]]$aggregation_level
          
          unprepared_recipe_cv <- NULL
          if (model_name_cv %in% c("XGBoost", "RF")) {
            # Ensure full_train_data is passed for recipe creation if it's used as the basis
            unprepared_recipe_cv <- create_tree_recipe(full_train_data, freq_str = freq_str_cv)
          }

          for (i in 1:nrow(ts_cv_splits)) {
            progress_counter <- progress_counter + 1
            shiny::setProgress(value = progress_counter / n_total_iterations,
                               detail = paste("Model:", model_name_cv, "- Slice", i, "of", nrow(ts_cv_splits)))

            slice <- ts_cv_splits[i, ]
            train_slice <- rsample::training(slice)
            assess_slice <- rsample::assessment(slice)

            train_slice <- train_slice %>% dplyr::mutate(ds = as.Date(ds), y = as.numeric(y))
            assess_slice <- assess_slice %>% dplyr::mutate(ds = as.Date(ds), y = as.numeric(y))

            last_train_slice_date <- max(train_slice$ds)
            horizon_cv_fold <- nrow(assess_slice)
            forecast_tibble_cv <- NULL
            
            # --- BEGIN PASTE OF MODEL LOGIC ---
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
                          if("has_holiday_in_week" %in% names(future_xreg_df_cv) && "has_holiday_in_week" %in% arima_xreg_colnames_cv) { 
                               future_xreg_cv <- as.matrix(future_xreg_df_cv %>% dplyr::select(has_holiday_in_week))
                          } else if ("has_holiday_in_week" %in% arima_xreg_colnames_cv) { 
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
              model_obj_cv <- train_prophet(prophet_train_slice_cv, model_config_original, holidays_input = holidays_for_prophet_cv) # Simplified regressors for CV
              if(!is.null(model_obj_cv)) {
                forecast_tibble_cv <- forecast_prophet(model_obj_cv, horizon_cv_fold, freq_str_cv, model_config_original$capacity) # Simplified regressors for CV
              }
            } else if (model_name_cv == "XGBoost") {
              req(unprepared_recipe_cv)
              xgb_params_for_cv <- model_config_original 
              prep_recipe_cv_fold <- recipes::prep(unprepared_recipe_cv, training = train_slice)
              model_obj_cv <- train_xgboost(prep_recipe_cv_fold, xgb_params_for_cv)
              if(!is.null(model_obj_cv)) {
                # Pass full_train_data (the overall training set for CV) to forecast_xgboost
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
              nnetar_config_cv <- reactive_run_models_summary()[[model_name_cv]]$config 
              model_obj_cv <- train_nnetar(train_slice, nnetar_config_cv, original_agg_level)
              if(!is.null(model_obj_cv)) {
                forecast_output_cv <- forecast_nnetar(model_obj_cv, horizon_cv_fold, last_train_slice_date, freq_str_cv)
                if(!is.null(forecast_output_cv)) forecast_tibble_cv <- forecast_output_cv$forecast
              }
            }
            # --- END PASTE OF MODEL LOGIC ---

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
                    dplyr::mutate(Model = model_name_cv, Fold = i, Slice_Id = slice$.id)
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
      })

      if (length(all_cv_metrics) > 0) {
        final_cv_metrics_df <- dplyr::bind_rows(all_cv_metrics)
        cv_raw_metrics_list_rv(final_cv_metrics_df)

        cv_summary_table <- final_cv_metrics_df %>%
          dplyr::group_by(Model, .metric) %>%
          dplyr::summarise(.estimate = mean(.estimate, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = .metric, values_from = .estimate)

        cv_results_data_rv(cv_summary_table)
        shiny::showNotification("Cross-validation complete.", type = "message")
      } else {
        cv_results_data_rv(NULL)
        cv_raw_metrics_list_rv(NULL)
        shiny::showNotification("Cross-validation ran but no metrics were calculated.", type = "warning")
      }
    })

    output$cv_results_table_output <- DT::renderDataTable({
      req(cv_results_data_rv())
      DT::datatable(cv_results_data_rv(), options = list(pageLength = 5, scrollX = TRUE), caption = "Mean Cross-Validation Metrics")
    })

    output$cv_results_plot_output <- renderPlot({
      req(cv_raw_metrics_list_rv())
      final_cv_metrics_df_plot <- cv_raw_metrics_list_rv()
      req(nrow(final_cv_metrics_df_plot) > 0)
      ggplot2::ggplot(final_cv_metrics_df_plot, ggplot2::aes(x = Model, y = .estimate, fill = Model)) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::facet_wrap(~ .metric, scales = "free_y") +
        ggplot2::labs(title = "Distribution of CV Metrics Across Folds", x = "Model", y = "Metric Value") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })
  })
}

# Helper function (if not already globally available or in another utils file)
# Ensure this is defined if not already.
# create_tree_recipe <- function(df, freq_str) {
#   # Placeholder for the actual recipe creation logic
#   # This should match the one in your app_server.R or utils_train_forecast.R
#   recipes::recipe(y ~ ., data = df) %>%
#     recipes::step_timeseries_signature(ds) %>%
#     recipes::step_rm(matches("(iso$)|(xts$)|(hour)|(min)|(sec)|(am.pm)")) %>%
#     recipes::step_normalize(all_numeric_predictors()) %>%
#     recipes::step_dummy(all_nominal_predictors(), one_hot = TRUE)
# }
