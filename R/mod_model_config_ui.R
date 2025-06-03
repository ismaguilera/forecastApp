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
    h4(textOutput(ns("ui_model_config_title_h4_label"), inline = TRUE)),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5(textOutput(ns("ui_select_models_to_run_label"), inline = TRUE)),
        checkboxInput(ns("use_arima"), textOutput(ns("ui_arima_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_ets"), textOutput(ns("ui_ets_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_tbats"), textOutput(ns("ui_tbats_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_prophet"), textOutput(ns("ui_prophet_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_xgboost"), textOutput(ns("ui_xgboost_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_gam"), textOutput(ns("ui_gam_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_rf"), textOutput(ns("ui_random_forest_label"), inline = TRUE), value = TRUE),
        checkboxInput(ns("use_nnetar"), textOutput(ns("ui_nnetar_label"), inline = TRUE), value = FALSE),
        hr(),
        shiny::tooltip(
          numericInput(ns("forecastHorizon"), textOutput(ns("ui_forecast_horizon_label"), inline = TRUE), value = default_horizon, min = 1, step = 1, max = 365),
          title = i18n$t("Number of future periods (e.g., days, weeks based on aggregation) to forecast."),
          placement = "right"
        ),
        actionButton(ns("runForecast"), textOutput(ns("ui_run_forecast_label"), inline = TRUE), icon = icon("play-circle"), class = "btn-primary btn-block")
      ),
      mainPanel(
        width = 9,
        h5(textOutput(ns("ui_configure_model_parameters_label"), inline = TRUE)),
        bslib::accordion( # Using accordion for model parameters
          id = ns("modelParamsAccordion"), # Add an ID if needed for control
          multiple = FALSE, # Allow only one panel open at a time
          # ARIMA Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_arima_parameters_title"), inline = TRUE),
            value = "ARIMA", # Value for conditional logic if needed
            shiny::tooltip(
              checkboxInput(ns("arima_auto"), textOutput(ns("ui_auto_arima_label"), inline = TRUE), value = TRUE),
              title = i18n$t("Check to use automated ARIMA order selection (auto.arima). Uncheck for manual p,d,q,P,D,Q specification."),
              placement = "right"
            ),
            conditionalPanel(
              condition = paste0("input['", ns("arima_auto"), "'] == false"),
              shiny::tooltip(
                numericInput(ns("arima_p"), textOutput(ns("ui_order_p_ar_label"), inline = TRUE), value = 1, min = 0, max = 5),
                title = i18n$t("Order of the non-seasonal autoregressive (AR) part."),
                placement = "right"
              ),
              shiny::tooltip(
                numericInput(ns("arima_d"), textOutput(ns("ui_order_d_diff_label"), inline = TRUE), value = 1, min = 0, max = 3),
                title = i18n$t("Order of non-seasonal differencing."),
                placement = "right"
              ),
              shiny::tooltip(
                numericInput(ns("arima_q"), textOutput(ns("ui_order_q_ma_label"), inline = TRUE), value = 1, min = 0, max = 5),
                title = i18n$t("Order of the non-seasonal moving average (MA) part."),
                placement = "right"
              ),
              shiny::tooltip(
                checkboxInput(ns("arima_seasonal"), textOutput(ns("ui_seasonal_arima_label"), inline = TRUE), value = TRUE),
                title = i18n$t("Check to include seasonal components (P,D,Q,Period) in the ARIMA model."),
                placement = "right"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("arima_seasonal"), "'] == true && input['", ns("arima_auto"), "'] == false"),
                numericInput(ns("arima_P"), textOutput(ns("ui_seasonal_p_sar_label"), inline = TRUE), value = 1, min = 0, max = 3), # Tooltip for P, D, Q can be added if needed
                numericInput(ns("arima_D"), textOutput(ns("ui_seasonal_d_sdiff_label"), inline = TRUE), value = 1, min = 0, max = 2),
                numericInput(ns("arima_Q"), textOutput(ns("ui_seasonal_q_sma_label"), inline = TRUE), value = 1, min = 0, max = 3),
                shiny::tooltip(
                  numericInput(ns("arima_period"), textOutput(ns("ui_seasonal_period_label"), inline = TRUE), value = 7, min = 1, max = 366),
                  title = i18n$t("Number of observations per seasonal cycle (e.g., 7 for daily data with weekly seasonality, 52 for weekly data with yearly seasonality)."),
                  placement = "right"
                )
              )
            )
          ),
          # ETS Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_ets_parameters_title"), inline = TRUE),
            value = "ETS",
            checkboxInput(ns("ets_manual"), textOutput(ns("ui_manual_ets_configuration_label"), inline = TRUE), value = FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("ets_manual"), "'] == true"),
              selectInput(ns("ets_e"), textOutput(ns("ui_error_component_e_label"), inline = TRUE), choices = c("Auto (Z)"="Z", "Additive (A)"="A", "Multiplicative (M)"="M"), selected = "Z"),
              selectInput(ns("ets_t"), textOutput(ns("ui_trend_component_t_label"), inline = TRUE), choices = c("Auto (Z)"="Z", "None (N)"="N", "Additive (A)"="A", "Multiplicative (M)"="M"), selected = "Z"),
              selectInput(ns("ets_s"), textOutput(ns("ui_seasonal_component_s_label"), inline = TRUE), choices = c("Auto (Z)"="Z", "None (N)"="N", "Additive (A)"="A", "Multiplicative (M)"="M"), selected = "Z"),
              selectInput(ns("ets_damped_str"), textOutput(ns("ui_damped_trend_label"), inline = TRUE), choices = c("Auto (NULL)"="NULL", "Yes (TRUE)"="TRUE", "No (FALSE)"="FALSE"), selected = "NULL")
            )
          ),
          # TBATS Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_tbats_parameters_title"), inline = TRUE),
            value = "TBATS",
            tags$p(i18n$t("TBATS model is run with automatic parameter selection."))
          ),
          # Prophet Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_prophet_parameters_title"), inline = TRUE),
            value = "Prophet",
            checkboxInput(ns("prophet_yearly"), textOutput(ns("ui_yearly_seasonality_label"), inline = TRUE), value = TRUE),
            checkboxInput(ns("prophet_weekly"), textOutput(ns("ui_weekly_seasonality_label"), inline = TRUE), value = TRUE),
            checkboxInput(ns("prophet_daily"), textOutput(ns("ui_daily_seasonality_label"), inline = TRUE), value = FALSE),
            selectInput(ns("prophet_growth"), textOutput(ns("ui_growth_model_label"), inline = TRUE), choices = c("linear", "logistic"), selected = "linear"),
            conditionalPanel(
              condition = paste0("input['", ns("prophet_growth"), "'] == 'logistic'"),
              numericInput(ns("prophet_capacity"), textOutput(ns("ui_capacity_cap_label"), inline = TRUE), value = 100000) # Tooltip can be added if complex
            ),
            shiny::tooltip(
              numericInput(ns("prophet_changepoint_scale"), textOutput(ns("ui_changepoint_prior_scale_label"), inline = TRUE), value = 0.05, min = 0.001, max = 0.5, step = 0.01),
              title = i18n$t("Flexibility of the automatic changepoint detection. Larger values allow more changepoints and a more flexible trend."),
              placement = "right"
            ),
            shiny::tooltip(
              fileInput(ns("prophet_regressors_file"), textOutput(ns("ui_upload_external_regressors_label"), inline = TRUE), accept = ".csv"),
              title = i18n$t("Optional CSV file with additional regressors. Must include 'ds' column and regressor columns. Data should cover training and forecast periods."),
              placement = "right"
            )
          ),
          # XGBoost Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_xgboost_parameters_title"), inline = TRUE),
            value = "XGBoost",
            shiny::tooltip(
              checkboxInput(ns("xgb_enable_tuning"), textOutput(ns("ui_enable_hyperparameter_tuning_label"), inline = TRUE), value = TRUE),
              title = i18n$t("Enable automatic hyperparameter tuning for XGBoost using time series cross-validation. If unchecked, uses the parameters below."),
              placement = "right"
            ),
            shiny::tooltip(
              numericInput(ns("xgb_nrounds"), textOutput(ns("ui_number_of_rounds_trees_label"), inline = TRUE), value = 100, min = 10, max = 2000, step = 10),
              title = i18n$t("Maximum number of boosting rounds (trees)."),
              placement = "right"
            ),
            shiny::tooltip(
              numericInput(ns("xgb_eta"), textOutput(ns("ui_learning_rate_eta_label"), inline = TRUE), value = 0.1, min = 0.001, max = 0.5, step = 0.01),
              title = i18n$t("Learning rate. Smaller values make the boosting process more conservative."),
              placement = "right"
            ),
            numericInput(ns("xgb_max_depth"), textOutput(ns("ui_max_tree_depth_label"), inline = TRUE), value = 6, min = 1, max = 20, step = 1),
            numericInput(ns("xgb_subsample"), textOutput(ns("ui_subsample_ratio_label"), inline = TRUE), value = 0.8, min = 0.1, max = 1, step = 0.1),
            numericInput(ns("xgb_colsample"), textOutput(ns("ui_column_sample_ratio_label"), inline = TRUE), value = 0.8, min = 0.1, max = 1, step = 0.1),
            numericInput(ns("xgb_gamma"), textOutput(ns("ui_min_split_loss_gamma_label"), inline = TRUE), value = 0, min = 0, max = 10, step = 0.1)
          ),
          # Random Forest Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_random_forest_parameters_title"), inline = TRUE),
            value = "RF",
            shiny::tooltip(
              checkboxInput(ns("rf_enable_tuning"), textOutput(ns("ui_rf_enable_hyperparameter_tuning_label"), inline = TRUE), value = TRUE),
              title = i18n$t("Enable automatic hyperparameter tuning for Random Forest using time series cross-validation. If unchecked, uses the parameters below."),
              placement = "right"
            ),
            numericInput(ns("rf_num_trees"), textOutput(ns("ui_number_of_trees_label"), inline = TRUE), value = 500, min = 50, max = 2000, step = 50),
            shiny::tooltip(
              numericInput(ns("rf_mtry"), textOutput(ns("ui_variables_per_split_mtry_label"), inline = TRUE), value = 0, min = 0, max = 100, step = 1),
              title = i18n$t("Number of variables randomly sampled as candidates at each split. Set to 0 for default (sqrt(number of features)) if tuning is off."),
              placement = "right"
            ),
            numericInput(ns("rf_min_node_size"), textOutput(ns("ui_min_node_size_label"), inline = TRUE), value = 5, min = 1, max = 50, step = 1)
          ),
          # GAM Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_gam_parameters_title"), inline = TRUE),
            value = "GAM",
            selectInput(ns("gam_trend_type"), textOutput(ns("ui_trend_type_label"), inline = TRUE), choices = c("Linear" = "linear", "Smooth (Spline)" = "smooth"), selected = "linear"),
            checkboxInput(ns("gam_use_season_y"), textOutput(ns("ui_include_yearly_seasonality_label"), inline = TRUE), value = TRUE),
            checkboxInput(ns("gam_use_season_w"), textOutput(ns("ui_include_weekly_seasonality_label"), inline = TRUE), value = TRUE)
          ),
          # NNETAR Panel
          bslib::accordion_panel(
            title = textOutput(ns("ui_nnetar_parameters_title"), inline = TRUE),
            value = "NNETAR", 
            id = ns("nnetar_accordion_panel"),
            conditionalPanel(
              condition = paste0("input['", ns("use_nnetar"), "'] == true"),
              tags$p(i18n$t("Neural Network Autoregressive Model. Predicts based on lagged values of the time series.")),
              hr(),
              h5(textOutput(ns("ui_model_structure_label"), inline = TRUE)),
              fluidRow(
                column(6, numericInput(ns("nnetar_p"), textOutput(ns("ui_non_seasonal_lags_p_label"), inline = TRUE), value = 1, min = 0, step = 1, width = '100%')),
                column(6, numericInput(ns("nnetar_P"), textOutput(ns("ui_seasonal_lags_p_label"), inline = TRUE), value = 1, min = 0, step = 1, width = '100%'))
              ),
              helpText(i18n$t("Set p/P to 0 to let nnetar choose automatically. If both >0, specific lags are used. Seasonal P is only effective if data frequency > 1 (e.g., daily/weekly).")),
              fluidRow(
                column(6, 
                       selectInput(ns("nnetar_size_method"), textOutput(ns("ui_hidden_layer_neurons_method_label"), inline = TRUE),
                                   choices = c("Auto" = "auto", "Manual" = "manual"), selected = "auto", width = '100%'),
                       helpText(i18n$t("If 'Auto': For seasonal models (P>0), size is approx. (p+P+1)/2. For non-seasonal (P=0, p>0), size is approx. (p+1)/2. If p=0 and P=0, nnetar attempts to choose p, P, and size."))
                ),
                column(6, conditionalPanel(
                  condition = paste0("input['", ns("nnetar_size_method"), "'] == 'manual'"),
                  numericInput(ns("nnetar_size_manual"), textOutput(ns("ui_number_of_hidden_neurons_label"), inline = TRUE), value = 5, min = 1, step = 1, width = '100%')
                ))
              ),
              numericInput(ns("nnetar_repeats"), textOutput(ns("ui_repeats_for_stability_label"), inline = TRUE), value = 20, min = 1, step = 5, width = '100%'),
              hr(),
              h5(textOutput(ns("ui_data_preprocessing_label"), inline = TRUE)),
              checkboxInput(ns("nnetar_lambda_auto"), textOutput(ns("ui_box_cox_lambda_auto_label"), inline = TRUE), value = TRUE),
              conditionalPanel(
                condition = paste0("!input['", ns("nnetar_lambda_auto"), "']"),
                numericInput(ns("nnetar_lambda_manual"), textOutput(ns("ui_manual_lambda_label"), inline = TRUE), value = NA, min = 0, max = 1, step = 0.01, width = '100%')
              )
            )
          )
        ) # End bslib::accordion
      ) # End mainPanel
    ) # End sidebarLayout
  ) # End tagList
}
