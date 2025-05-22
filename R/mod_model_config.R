# R/mod_model_config.R
# Helper for displaying NA nicely (put in R/utils.R or similar if needed often)
#' model_config UI Function
#'
#' @description A shiny Module for configuring forecasting models (ARIMA, Prophet, XGBoost)
#'   and setting the forecast horizon.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny shinyjs bslib
#' @importFrom shinyjs useShinyjs disabled hidden
mod_model_config_ui <- function(id){
  ns <- NS(id)
  default_horizon <- get_golem_config("forecast_horizon_default") # Returns 150 by default
  tagList(
    # shinyjs::useShinyjs(), # Initialize shinyjs
    # --- Model Selection ---
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Models",
        # h3("Model Configuration"), # Heading for the section
        checkboxInput(ns("use_arima"), "ARIMA", value = TRUE),
        checkboxInput(ns("use_ets"), "ETS", value = FALSE),
        checkboxInput(ns("use_tbats"), "TBATS", value = FALSE),
        checkboxInput(ns("use_prophet"), "Prophet", value = FALSE),
        checkboxInput(ns("use_xgboost"), "XGBoost", value = FALSE),
        checkboxInput(ns("use_gam"), "GAM", value = FALSE),
        checkboxInput(ns("use_rf"), "RF", value = FALSE),
        # checkboxGroupInput(
        #   inputId = ns("selected_models"),
        #   label = "Select Models to Run:",
        #   choices = list("ARIMA", "ETS", "TBATS","Prophet", "XGBoost", "GAM", "RF"),
        #   selected = c("ARIMA"), # Default selection
        #   inline = TRUE
        # ),
        # width = 350,
        # Placeholder for Model Selection/Config Module UI
        # tags$p("Placeholder for Model Configuration Tabs (e.g., mod_model_config_ui)"),
        # tags$p("Placeholder for Run Forecast Button"),
         # Module UI would go here
      ), # End sidebar
    # Main content for Model panel
    bslib::card(
      bslib::card_header("Model Configuration"),
      bslib::card_body(
        # Placeholder - Config details might be primarily in sidebar
        # tags$p("Detailed model parameters appear in the sidebar.")

    # checkboxGroupInput(
    #   inputId = ns("selected_models"),
    #   label = "Select Models to Run:",
    #   choices = list("ARIMA", "ETS", "TBATS","Prophet", "XGBoost", "GAM", "RF"),
    #   selected = c("ARIMA"), # Default selection
    #   inline = TRUE
    # ),
    # hr(),
    p(tags$strong("Configure parameters for selected models below:")),

    # --- Accordion for Parameters ---
    bslib::accordion(
      id = ns("model_params_accordion"),
      open = FALSE, # Start collapsed
      multiple = FALSE, # Allow multiple open

      # # --- ARIMA Accordion Panel (Initially Hidden) ---
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "ARIMA Parameters",
          value = "ARIMA", # Optional value for state
          # icon = icon("graph-up"),
          id = ns("arima_accordion_panel"), # ID for toggling visibility
          conditionalPanel(
            condition = paste0("input['", ns("use_arima"), "'] == true"),
            checkboxInput(ns("arima_auto"), "Use auto.arima? (Overrides manual settings)", value = TRUE),
            # shinyjs::disabled( # Start disabled if auto is checked by default
            conditionalPanel(
            # JS Condition: !input['ID_auto'] && input['ID_seasonal']
              condition = paste0("!input['", ns("arima_auto"), "']"),
              div(id = ns("arima_manual_params"),
                    h4("Manual ARIMA Order"),
                    fluidRow(column(4, numericInput(ns("arima_p"), "p (AR order)", min=0, step=1, value=1)),
                             column(4, numericInput(ns("arima_d"), "d (Differencing)", min=0, step=1, value=1)),
                             column(4, numericInput(ns("arima_q"), "q (MA order)", min=0, step=1, value=1))
                             )
                    )
              ),
              hr(), h4("Seasonal Component"),
              checkboxInput(ns("arima_seasonal"), "Include Seasonal Component (P, D, Q)?", value = TRUE), # Clarified label
              conditionalPanel(
                # JS Condition: !input['ID_auto'] && input['ID_seasonal']
                condition = paste0("!input['", ns("arima_auto"), "'] && input['", ns("arima_seasonal"), "']"),
                h4("Manual Seasonal Order (P, D, Q, Period)"),
                fluidRow(
                  column(3, numericInput(ns("arima_P"), "Seasonal P", min=0, step=1, value=1)),
                  column(3, numericInput(ns("arima_D"), "Seasonal D", min=0, step=1, value=1)),
                  column(3, numericInput(ns("arima_Q"), "Seasonal Q", min=0, step=1, value=1)),
                  column(3, numericInput(ns("arima_period"), "Seasonal Period", min=1, step=1, value=7)) # Removed placeholder previously
                )
              ) # End Conditional Panel for Manual Seasonal Inputs
        ) # End accordion_panel
      ), # End hidden() wrapper
      #
      # # ETS Parameters Div (Initially Hidden)
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "ETS Parameters", value = "ETS", #icon = icon("lightning-charge"),
          id = ns("ets_accordion_panel"),
          conditionalPanel(
            condition = paste0("input['", ns("use_ets"), "'] == true"),
            tags$p("Exponential Smoothing state space models. Components can be selected automatically (default) or specified manually."),
            # --- Paste ALL ETS controls here ---
            checkboxInput(ns("ets_manual"), "Specify Model Manually?", value = FALSE), hr(),
            conditionalPanel(
              condition = paste0("input['", ns("ets_manual"), "']"), # Show if manual checkbox is TRUE
              h5("Manual ETS Components:"),
              tags$p(tags$em("Note: 'Auto (Z)' will let ets() choose the best component based on AICc.")),
              fluidRow(
                column(4, selectInput(ns("ets_e"), "Error (E)", choices = c("Auto ('Z')"="Z", "Additive ('A')"="A", "Multiplicative ('M')"="M"), selected = "Z")),
                column(4, selectInput(ns("ets_t"), "Trend (T)", choices = c("Auto ('Z')"="Z", "None ('N')"="N", "Additive ('A')"="A", "Multiplicative ('M')"="M"), selected = "Z")),
                column(4, selectInput(ns("ets_s"), "Season (S)", choices = c("Auto ('Z')"="Z", "None ('N')"="N", "Additive ('A')"="A", "Multiplicative ('M')"="M"), selected = "Z"))
              ),
              selectInput(ns("ets_damped"), "Damped Trend?", choices = c("Auto (NULL)"="NULL", "Yes (TRUE)"="TRUE", "No (FALSE)"="FALSE"), selected = "NULL"),
              helpText("Multiplicative components require strictly positive data (> 0). Seasonal component requires frequency > 1 (automatically determined by Daily/Weekly aggregation)."),
            ), # End conditionalPanel for Manual Settings E,T,S, Damped inputs
            conditionalPanel(
              condition = paste0("!input['", ns("ets_manual"), "']"), # Show if manual checkbox is FALSE
              h5("Automatic ETS Selection:"),
              tags$p("Model components (Error, Trend, Seasonality) and damping will be selected automatically based on AICc using ", tags$code("forecast::ets()"), ".")
            ) # End conditionalPanel for Auto info text
        )
      ),
      #
      # # TBATS Parameters Div (Initially Hidden)
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "TBATS Parameters", value = "TBATS", #icon = icon("reception-4"),
          id = ns("tbats_accordion_panel"),
          conditionalPanel(
            condition = paste0("input['", ns("use_tbats"), "'] == true"),
            # --- Paste ALL TBATS controls/text here ---
            tags$p("TBATS models complex seasonality using Trigonometry, Box-Cox transformations, ARMA errors, Trend and Seasonality components."),
            tags$p(tags$strong("Configuration:")),
            tags$ul(
              tags$li("All model components and seasonal periods are determined automatically using ", tags$code("forecast::tbats()"), "."),
              tags$li("It attempts to identify relevant seasonal periods (e.g., weekly/yearly based on data frequency) if sufficient data exists (ideally > 3 cycles).")
            ),
            tags$p(tags$em("No manual parameters are required for this implementation."))
        )
      ),
      #
      # # Prophet Parameters Div (Initially Hidden)
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "Prophet Parameters", value = "Prophet", #icon = icon("calendar-heart"),
          id = ns("prophet_accordion_panel"), # ID for toggling visibility
          conditionalPanel(
            condition = paste0("input['", ns("use_prophet"), "'] == true"),
            # --- Paste ALL Prophet controls here ---
            h4("Seasonality Components"),
            checkboxInput(ns("prophet_yearly"), "Yearly Seasonality", value = TRUE),
            checkboxInput(ns("prophet_weekly"), "Weekly Seasonality", value = TRUE),
            checkboxInput(ns("prophet_daily"), "Daily Seasonality", value = FALSE),
            hr(),
            h4("Growth Model"),
            selectInput(ns("prophet_growth"), "Growth Model Type", choices = c("linear", "logistic"), selected = "linear"),
            # Conditional Panel for Capacity (Logistic Growth)
            conditionalPanel(
              condition = paste0("input['", ns("prophet_growth"), "'] == 'logistic'"), # JS condition
              numericInput(ns("prophet_capacity"), "Capacity (Max Value)", value = 1, min=0)
            ),
            hr(),
            h4("Hyperparameters"),
            numericInput(ns("prophet_changepoint_scale"), "Changepoint Prior Scale", value = 0.05, min = 0.001, max = 1.0, step = 0.01),
            # Can add more Prophet params here (holidays, regressors later)
            hr(),
            h4("Additional Features (Optional)"),
            fileInput(ns("prophet_holidays_file"),
                      "Upload Holidays File (CSV: Col 1 = Date [yyyy-mm-dd], Col 2 = Holiday Name, [Opt: Lower/Upper Window])", # Updated label
                      multiple = FALSE,
                      accept = c("text/csv", ".csv")),
            actionButton(ns("loadDefaultHolidays"), "Load Default Holidays (Chile)",
                         icon = icon("calendar-check"), class="btn-sm"),
            br(), # Add line break
            fileInput(ns("prophet_regressors_file"), "Upload External Regressors File (CSV: ds, regressor1, regressor2, ...)",
                      multiple = FALSE,
                      accept = c("text/csv", ".csv")),
            # Placeholder for selecting regressor columns if needed later
            # uiOutput(ns("prophet_regressor_selector_ui"))
            helpText("Note: Regressor file must include future dates/values for the forecast horizon.")
        )
      ),
      #
      #
      # # XGBoost Parameters Div (Initially Hidden)
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "XGBoost Parameters", value = "XGBoost", #icon = icon("tree"),
          id = ns("xgb_accordion_panel"), # ID for toggling visibility
          conditionalPanel(
            condition = paste0("input['", ns("use_xgboost"), "'] == true"),
          # --- Paste ALL XGBoost controls here ---
            fluidRow(
              column(6, numericInput(ns("xgb_nrounds"), "Number of Boosting Rounds", value = 100, min = 1, step = 10)),
              column(6, numericInput(ns("xgb_max_depth"), "Max Tree Depth", value = 6, min = 1, step = 1))
            ),
            fluidRow(
              column(6, numericInput(ns("xgb_eta"), "Learning Rate (eta)", value = 0.1, min = 0.001, max = 1, step = 0.01)),
              column(6, numericInput(ns("xgb_gamma"), "Min Split Loss (gamma)", value = 0, min = 0, step = 0.1)) # Added gamma
            ),
            hr(),
            h4("Regularization & Sampling"),
            fluidRow(
              column(6, sliderInput(ns("xgb_subsample"), "Subsample Ratio (per tree)", min = 0.1, max = 1, value = 0.8, step = 0.05)),
              column(6, sliderInput(ns("xgb_colsample"), "Column Sample Ratio (per tree)", min = 0.1, max = 1, value = 0.8, step = 0.05))
            ),
            hr(),
            helpText("Note: Time series features (e.g., lags, day of week, month, year) will be automatically generated before training XGBoost.")
        )
      ),
      #
      # # GAM Parameters Div (Initially Hidden)
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "GAM Parameters", value = "GAM", #icon = icon("bezier"),
          id = ns("gam_accordion_panel"),
          conditionalPanel(
            condition = paste0("input['", ns("use_gam"), "'] == true"),
          # --- Paste ALL GAM controls here ---
            tags$p("Models non-linear trends and seasonalities using smooth functions (splines)."),
            hr(),
            h5("Configuration:"),
            selectInput(ns("gam_trend_type"), "Trend Type",
                        choices = c("Smooth (Spline)" = "smooth", "Linear" = "linear"),
                        selected = "linear"),
            checkboxInput(ns("gam_use_season_y"), "Model Yearly Seasonality (Smooth, Day of Year)", value = TRUE),
            checkboxInput(ns("gam_use_season_w"), "Model Weekly Seasonality (Smooth, Day of Week)", value = TRUE),
            # Add controls for regressors/holidays here later if needed
            hr(),
            helpText("GAM requires feature engineering (time index, date components) which is done automatically based on selections above.")
        )
      ),
      #
      # # Random Forest Parameters Div (Initially Hidden)
      # # shinyjs::hidden(
        bslib::accordion_panel(
          title = "Random Forest Parameters", value = "RF", #icon = icon("diagram-3"),
          id = ns("rf_accordion_panel"),
          conditionalPanel(
            condition = paste0("input['", ns("use_rf"), "'] == true"),
          # --- Paste ALL RF controls here ---
            tags$p("An ensemble model using multiple decision trees. Good for non-linear patterns and interactions."),
            hr(),
            h5("Configuration:"),
            fluidRow(
              # Num Trees
              column(6, numericInput(ns("rf_num_trees"), "Number of Trees",
                                     min=10, max=2000, value=500, step=50)),
              # Mtry
              column(6, numericInput(ns("rf_mtry"), "Variables per Split (mtry)",
                                     min=0, value=0, step=1)) # 0 indicates auto-calculate
            ),
            fluidRow(
              # Min Node Size
              column(6, numericInput(ns("rf_min_node_size"), "Min Node Size",
                                     min=1, value=5, step=1))
            ),
            hr(),
            helpText("Note 1: Uses the 'ranger' package. Set mtry=0 for automatic calculation (sqrt(num_predictors))."),
            helpText("Note 2: Time series features (lags, date parts, etc.) are generated automatically via the same recipe as XGBoost.")
          )
        )
      )
    )
      )
    ),



    # --- End Parameter Sections ---
    # --- End Model Selection ---
    # navlistPanel(
    #   id = ns("modelTabs"),
    #   # type = "tabs",
    #
    #   # --- ARIMA Tab ---
    #   tabPanel("ARIMA",
    #            br(), # Add some space
    #            checkboxInput(ns("arima_auto"), "Use auto.arima? (Overrides manual settings)", value = TRUE),
    #            # Div to group manual inputs for easy enable/disable
    #            shinyjs::disabled( # Start disabled if auto is checked by default
    #              div(id = ns("arima_manual_params"),
    #                  h4("Manual ARIMA Order"),
    #                  fluidRow(
    #                    column(4, numericInput(ns("arima_p"), "p (AR order)", min=0, step=1, value=1)),
    #                    column(4, numericInput(ns("arima_d"), "d (Differencing)", min=0, step=1, value=1)),
    #                    column(4, numericInput(ns("arima_q"), "q (MA order)", min=0, step=1, value=1))
    #                  )
    #              )
    #            ),
    #            hr(),
    #            h4("Seasonal Component"),
    #            checkboxInput(ns("arima_seasonal"), "Include Seasonal Component (P, D, Q)?", value = TRUE), # Clarified label
    #            # --- CORRECTED Conditional Panel ---
    #            # Show manual seasonal inputs ONLY if Manual mode is selected AND Seasonal is enabled
    #            conditionalPanel(
    #              # JS Condition: !input['ID_auto'] && input['ID_seasonal']
    #              condition = paste0("!input['", ns("arima_auto"), "'] && input['", ns("arima_seasonal"), "']"),
    #              h4("Manual Seasonal Order (P, D, Q, Period)"),
    #              fluidRow(
    #                column(3, numericInput(ns("arima_P"), "Seasonal P", min=0, step=1, value=1)),
    #                column(3, numericInput(ns("arima_D"), "Seasonal D", min=0, step=1, value=1)),
    #                column(3, numericInput(ns("arima_Q"), "Seasonal Q", min=0, step=1, value=1)),
    #                column(3, numericInput(ns("arima_period"), "Seasonal Period", min=1, step=1, value=7)) # Removed placeholder previously
    #              )
    #            ) # End Conditional Panel for Manual Seasonal Inputs
    #   ), # End ARIMA tabPanel
    #   # --- ETS Tab ---
    #   # tabPanel("ETS", value = "ETS", # Use value for server-side check
    #   #          br(),
    #   #          h4("ETS (Error, Trend, Seasonality)"),
    #   #          tags$p("This model uses Exponential Smoothing state space methods."),
    #   #          tags$p(tags$strong("Configuration:")),
    #   #          tags$ul(
    #   #            tags$li("Model components (Error, Trend, Seasonality) and damping will be selected automatically based on AICc using ", tags$code("forecast::ets()"), "."),
    #   #            tags$li("Seasonality (weekly for daily data, yearly for weekly data) will be automatically included if sufficient data exists."),
    #   #            tags$li("Additive methods will be forced if data contains zeros or negative values.")
    #   #          ),
    #   #          tags$p(tags$em("No manual parameters are required for this implementation."))
    #   # ), # End ETS tabPanel
    #   tabPanel("ETS", value = "ETS", # Use value for server-side check
    #            br(),
    #            h4("ETS (Error, Trend, Seasonality)"),
    #            tags$p("Exponential Smoothing state space models. Components can be selected automatically (default) or specified manually."),
    #            hr(),
    #            # --- Add Manual Selection Checkbox ---
    #            checkboxInput(ns("ets_manual"), "Specify Model Manually?", value = FALSE),
    #            hr(),
    #            # --- End Add ---
    #
    #            # --- Manual ETS Configuration (Conditional) ---
    #            conditionalPanel(
    #              condition = paste0("input['", ns("ets_manual"), "']"), # Show if manual checkbox is TRUE
    #              h5("Manual ETS Components:"),
    #              tags$p(tags$em("Note: 'Auto (Z)' will let ets() choose the best component based on AICc.")),
    #              fluidRow(
    #                column(4, selectInput(ns("ets_e"), "Error (E)", choices = c("Auto ('Z')"="Z", "Additive ('A')"="A", "Multiplicative ('M')"="M"), selected = "Z")),
    #                column(4, selectInput(ns("ets_t"), "Trend (T)", choices = c("Auto ('Z')"="Z", "None ('N')"="N", "Additive ('A')"="A", "Multiplicative ('M')"="M"), selected = "Z")),
    #                column(4, selectInput(ns("ets_s"), "Season (S)", choices = c("Auto ('Z')"="Z", "None ('N')"="N", "Additive ('A')"="A", "Multiplicative ('M')"="M"), selected = "Z"))
    #              ),
    #              selectInput(ns("ets_damped"), "Damped Trend?", choices = c("Auto (NULL)"="NULL", "Yes (TRUE)"="TRUE", "No (FALSE)"="FALSE"), selected = "NULL"),
    #              helpText("Multiplicative components require strictly positive data (> 0). Seasonal component requires frequency > 1 (automatically determined by Daily/Weekly aggregation).")
    #            ), # End conditionalPanel for Manual Settings
    #
    #            # --- Automatic Configuration Info (Shown when Manual is FALSE) ---
    #            conditionalPanel(
    #              condition = paste0("!input['", ns("ets_manual"), "']"), # Show if manual checkbox is FALSE
    #              h5("Automatic ETS Selection:"),
    #              tags$p("Model components (Error, Trend, Seasonality) and damping will be selected automatically based on AICc using ", tags$code("forecast::ets()"), ".")
    #            ) # End conditionalPanel for Auto Info
    #
    #   ), # End ETS tabPanel
    #   tabPanel("TBATS", value = "TBATS", # Use value for server-side check
    #            br(),
    #            h4("TBATS Model"),
    #            tags$p("TBATS models complex seasonality using Trigonometry, Box-Cox transformations, ARMA errors, Trend and Seasonality components."),
    #            tags$p(tags$strong("Configuration:")),
    #            tags$ul(
    #              tags$li("All model components and seasonal periods are determined automatically using ", tags$code("forecast::tbats()"), "."),
    #              tags$li("It attempts to identify relevant seasonal periods (e.g., weekly/yearly based on data frequency) if sufficient data exists (ideally > 3 cycles).")
    #            ),
    #            tags$p(tags$em("No manual parameters are required for this implementation."))
    #   ), # End TBATS tabPanel
    #
    #   # --- Prophet Tab ---
    #   tabPanel("Prophet",
    #            br(),
    #            h4("Seasonality Components"),
    #            checkboxInput(ns("prophet_yearly"), "Yearly Seasonality", value = TRUE),
    #            checkboxInput(ns("prophet_weekly"), "Weekly Seasonality", value = TRUE),
    #            checkboxInput(ns("prophet_daily"), "Daily Seasonality", value = FALSE),
    #            hr(),
    #            h4("Growth Model"),
    #            selectInput(ns("prophet_growth"), "Growth Model Type", choices = c("linear", "logistic"), selected = "linear"),
    #            # Conditional Panel for Capacity (Logistic Growth)
    #            conditionalPanel(
    #              condition = paste0("input['", ns("prophet_growth"), "'] == 'logistic'"), # JS condition
    #              numericInput(ns("prophet_capacity"), "Capacity (Max Value)", value = 1, min=0)
    #            ),
    #            hr(),
    #            h4("Hyperparameters"),
    #            numericInput(ns("prophet_changepoint_scale"), "Changepoint Prior Scale", value = 0.05, min = 0.001, max = 1.0, step = 0.01),
    #            # Can add more Prophet params here (holidays, regressors later)
    #            hr(),
    #            h4("Additional Features (Optional)"),
    #            fileInput(ns("prophet_holidays_file"),
    #                      "Upload Holidays File (CSV: Col 1 = Date [yyyy-mm-dd], Col 2 = Holiday Name, [Opt: Lower/Upper Window])", # Updated label
    #                      multiple = FALSE,
    #                      accept = c("text/csv", ".csv")),
    #            actionButton(ns("loadDefaultHolidays"), "Load Default Holidays (Chile)",
    #                         icon = icon("calendar-check"), class="btn-sm"),
    #            br(), # Add line break
    #            fileInput(ns("prophet_regressors_file"), "Upload External Regressors File (CSV: ds, regressor1, regressor2, ...)",
    #                      multiple = FALSE,
    #                      accept = c("text/csv", ".csv")),
    #            # Placeholder for selecting regressor columns if needed later
    #            # uiOutput(ns("prophet_regressor_selector_ui"))
    #            helpText("Note: Regressor file must include future dates/values for the forecast horizon.")
    #
    #   ), # End Prophet tabPanel
    #
    #   # --- XGBoost Tab ---
    #   tabPanel("XGBoost",
    #            br(),
    #            h4("Tree Parameters"),
    #            fluidRow(
    #              column(6, numericInput(ns("xgb_nrounds"), "Number of Boosting Rounds", value = 100, min = 1, step = 10)),
    #              column(6, numericInput(ns("xgb_max_depth"), "Max Tree Depth", value = 6, min = 1, step = 1))
    #            ),
    #            fluidRow(
    #              column(6, numericInput(ns("xgb_eta"), "Learning Rate (eta)", value = 0.1, min = 0.001, max = 1, step = 0.01)),
    #              column(6, numericInput(ns("xgb_gamma"), "Min Split Loss (gamma)", value = 0, min = 0, step = 0.1)) # Added gamma
    #            ),
    #            hr(),
    #            h4("Regularization & Sampling"),
    #            fluidRow(
    #              column(6, sliderInput(ns("xgb_subsample"), "Subsample Ratio (per tree)", min = 0.1, max = 1, value = 0.8, step = 0.05)),
    #              column(6, sliderInput(ns("xgb_colsample"), "Column Sample Ratio (per tree)", min = 0.1, max = 1, value = 0.8, step = 0.05))
    #            ),
    #            hr(),
    #            helpText("Note: Time series features (e.g., lags, day of week, month, year) will be automatically generated before training XGBoost.")
    #   ), # End XGBoost tabPanel
    #   tabPanel("GAM", value = "GAM",
    #            br(),
    #            h4("GAM (Generalized Additive Model)"),
    #            tags$p("Models non-linear trends and seasonalities using smooth functions (splines)."),
    #            hr(),
    #            h5("Configuration:"),
    #            selectInput(ns("gam_trend_type"), "Trend Type",
    #                        choices = c("Smooth (Spline)" = "smooth", "Linear" = "linear"),
    #                        selected = "smooth"),
    #            checkboxInput(ns("gam_use_season_y"), "Model Yearly Seasonality (Smooth, Day of Year)", value = TRUE),
    #            checkboxInput(ns("gam_use_season_w"), "Model Weekly Seasonality (Smooth, Day of Week)", value = TRUE),
    #            # Add controls for regressors/holidays here later if needed
    #            hr(),
    #            helpText("GAM requires feature engineering (time index, date components) which is done automatically based on selections above.")
    #   ), # End GAM tabPanel
    #   tabPanel("Random Forest", value = "RF", # Use value for server-side check
    #            br(),
    #            h4("Random Forest Model"),
    #            tags$p("An ensemble model using multiple decision trees. Good for non-linear patterns and interactions."),
    #            hr(),
    #            h5("Configuration:"),
    #            fluidRow(
    #              # Num Trees
    #              column(6, numericInput(ns("rf_num_trees"), "Number of Trees",
    #                                     min=10, max=2000, value=500, step=50)),
    #              # Mtry
    #              column(6, numericInput(ns("rf_mtry"), "Variables per Split (mtry)",
    #                                     min=0, value=0, step=1)) # 0 indicates auto-calculate
    #            ),
    #            fluidRow(
    #              # Min Node Size
    #              column(6, numericInput(ns("rf_min_node_size"), "Min Node Size",
    #                                     min=1, value=5, step=1))
    #            ),
    #            hr(),
    #            helpText("Note 1: Uses the 'ranger' package. Set mtry=0 for automatic calculation (sqrt(num_predictors))."),
    #            helpText("Note 2: Time series features (lags, date parts, etc.) are generated automatically via the same recipe as XGBoost.")
    #
    #   ) # End RF tabPanel
    #   # --- END ADD ---
    # ), # End Accordion
    # hr(),
    # --- Forecast Horizon and Run Button (Below Tabs) ---
    bslib::card_footer(
      fluidRow(
        column(6,
               numericInput(ns("forecastHorizon"), "Forecast Horizon (periods)", value = default_horizon, min = 1, step = 1)
        ),
        column(6, style = "margin-top: 25px;", # Align button vertically roughly
               actionButton(ns("runForecast"), "Run Forecast(s)", icon = icon("play"), class = "btn-primary btn-lg") # Make button bigger
        )
      )
    )
  ) # End tagList
}

#' model_config Server Functions
#'
#' @description Server logic for the model configuration module. Primarily handles
#'   UI interactions (like enabling/disabling fields) and returns reactive
#'   expressions for all input values.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @return A list of reactive expressions corresponding to each input control.
#'
#' @noRd
#'
#' @import shiny bslib shinyjs
#' @importFrom shinyjs enable disable hidden show hide toggle reset
#' @importFrom rlang %||%
mod_model_config_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- UI Interaction Logic ---

    # Enable/disable manual ARIMA inputs based on auto.arima checkbox
    observeEvent(input$arima_auto, {
      if (input$arima_auto) {
        shinyjs::disable("arima_manual_params")
      } else {
        shinyjs::enable("arima_manual_params")
      }
    }, ignoreNULL = FALSE) # Use ignoreNULL = FALSE to run on initialization

    processed_holidays_rv <- reactiveVal(NULL)
    processed_regressors_rv <- reactiveVal(NULL)

    observeEvent(input$loadDefaultHolidays, {
      df_processed <- NULL
      default_h_file_name <- NULL
      tryCatch({
        default_h_file_name <- get_golem_config("default_holiday_file")
        req(default_h_file_name)
        default_h_file_path <- app_sys("extdata", default_h_file_name)
        req(file.exists(default_h_file_path))
        df <- utils::read.csv(default_h_file_path, stringsAsFactors = FALSE, header = TRUE)
        if (is.null(df) || ncol(df) < 2 || nrow(df) == 0) stop("Default holiday data is invalid.")
        df_processed <- tibble::tibble( ds = lubridate::as_date(df[[1]]), holiday = as.character(df[[2]]) )
        if(anyNA(df_processed$ds) || anyNA(df_processed$holiday)) stop("NAs found/generated in default holidays.")
        # if (ncol(df) >= 3 && is.numeric(df[[3]])) { df_processed$lower_window <- as.integer(df[[3]]) }
        # if (ncol(df) >= 4 && is.numeric(df[[4]])) { df_processed$upper_window <- as.integer(df[[4]]) }

        processed_holidays_rv(df_processed) # STORE the processed data
        shiny::showNotification( # SHOW notification immediately
          paste("Default holidays ('", default_h_file_name, "') loaded successfully."),
          type = "message", duration = 5
        )
        # Reset the file upload input to avoid confusion
        shinyjs::reset("prophet_holidays_file")

      }, error = function(e) {
        processed_holidays_rv(NULL) # Reset stored data on error
        shiny::showNotification(paste("Error loading default holidays:", e$message), type = "error", duration=10)
      })
    })
    # --- Observe UPLOAD file input change ---
    # observeEvent(input$prophet_holidays_file, {
    #   message("Holiday file uploaded.") # Log
    #   inFile <- input$prophet_holidays_file
    #   # Need req here because observeEvent triggers on reset too
    #   req(inFile, inFile$datapath)
    #   df_processed <- NULL

    #   tryCatch({
    #     # --- Load and Process Uploaded Holidays ---
    #     df <- utils::read.csv(inFile$datapath, stringsAsFactors = FALSE, header = TRUE)
    #     message("Uploaded holiday file read.")
    #     # --- Minimal Processing ---
    #     if (is.null(df) || ncol(df) < 2 || nrow(df) == 0) stop("Uploaded holiday data is invalid.")
    #     df_processed <- tibble::tibble( ds = lubridate::as_date(df[[1]]), holiday = as.character(df[[2]]) )
    #     if(anyNA(df_processed$ds) || anyNA(df_processed$holiday)) stop("NAs found/generated in uploaded holidays.")
    #     # if (ncol(df) >= 3 && is.numeric(df[[3]])) { df_processed$lower_window <- as.integer(df[[3]]) }
    #     # if (ncol(df) >= 4 && is.numeric(df[[4]])) { df_processed$upper_window <- as.integer(df[[4]]) }
    #     message("Uploaded holidays processed minimally.")
    #     # --- End Processing ---

    #     # --- SUCCESS: Update reactiveVal and Notify ---
    #     processed_holidays_rv(df_processed) # STORE the processed data
    #     shiny::showNotification( # SHOW notification immediately
    #       paste("Uploaded holidays file ('", inFile$name ,"') processed successfully."),
    #       type = "message", duration = 5
    #     )

    #   }, error = function(e) {
    #     processed_holidays_rv(NULL) # Reset stored data on error
    #     shiny::showNotification(paste("Error processing uploaded holidays file:", e$message), type = "error", duration=10)
    #   })
    # }) # End observeEvent prophet_holidays_file

    # --- ObserveEvent for Regressor Upload ---
    observeEvent(input$prophet_regressors_file, {
      message("Regressor file upload triggered.") # Log
      inFile <- input$prophet_regressors_file
      # Check if it's a valid file trigger (ignore resets which also trigger observeEvent)
      req(inFile, inFile$datapath)
      df_processed <- NULL

      tryCatch({
        # --- Load Uploaded Regressors ---
        df <- utils::read.csv(inFile$datapath, stringsAsFactors = FALSE, header = TRUE)
        message("Uploaded regressor file read.")

        # --- Validation & Processing ---
        validate(
          need("ds" %in% names(df), "Regressor file must contain a 'ds' column."),
          need(ncol(df) > 1, "Regressor file must contain at least one regressor column besides 'ds'."),
          need(nrow(df) > 0, "Regressor file appears to be empty.")
        )
        # Attempt date conversion
        ds_col <- tryCatch({ lubridate::as_date(df[["ds"]]) },
                           error = function(e) { stop("Could not convert 'ds' column to Date.") })
        validate(need(!anyNA(ds_col), "NAs found/generated in regressor 'ds' column."))
        df$ds <- ds_col # Update ds column

        # Optional: Check for NAs in regressor columns
        regressor_cols <- setdiff(names(df), "ds")
        if (any(sapply(df[regressor_cols], function(col) anyNA(col)))) {
          warning("NAs found in regressor columns. Check file or model might fail.")
        }
        message("Uploaded regressors processed minimally.")
        # --- End Validation & Processing ---

        df_processed <- df # Use the validated & processed df

        # --- SUCCESS ---
        processed_regressors_rv(df_processed) # STORE the processed data
        shiny::showNotification( # SHOW notification immediately
          paste("Uploaded regressors file ('", inFile$name ,"') processed successfully."),
          type = "message", duration = 5
        )

      }, error = function(e) {
        processed_regressors_rv(NULL) # Reset stored data on error
        shiny::showNotification(paste("Error processing uploaded regressors file:", e$message), type = "error", duration=10)
      })
    }) # End observeEvent prophet_regressors_file

    # --- Reactive: Read Regressors File (remains the same) ---
    # regressors_data <- reactive({
    #   req(input$prophet_regressors_file)
    #   inFile <- input$prophet_regressors_file
    #   # ... (rest of existing logic) ...
    #   tryCatch({
    #     df <- utils::read.csv(inFile$datapath, stringsAsFactors = FALSE)
    #     req("ds" %in% names(df))
    #     df$ds <- lubridate::as_date(df$ds)
    #     req(ncol(df) > 1)
    #     df
    #   }, error = function(e) {
    #     shiny::showNotification(paste("Error reading regressors file:", e$message), type = "error")
    #     return(NULL)
    #   })
    # })

    # --- Reactive: Read Regressors File ---
    # regressors_data <- reactive({
    #   req(input$prophet_regressors_file)
    #   inFile <- input$prophet_regressors_file
    #   df_regressors_processed <- NULL
    #   tryCatch({
    #     df <- utils::read.csv(inFile$datapath, stringsAsFactors = FALSE)
    #     # Basic Validation (must have 'ds')
    #     req("ds" %in% names(df))
    #     # Convert ds to Date
    #     df$ds <- lubridate::as_date(df$ds)
    #     # Ensure at least one other column exists
    #     req(ncol(df) > 1)
    #     # Select first two columns, attempt conversion/rename
    #     df_regressors_processed <- tibble::tibble(
    #       ds = lubridate::as_date(df[[1]]), # Convert first column
    #       holiday = as.character(df[[2]])    # Convert second column
    #     )
    #     return(df_regressors_processed)
    #   }, error = function(e) {
    #     shiny::showNotification(paste("Error reading regressors file:", e$message), type = "error")
    #     return(NULL)
    #   })
    # })

    # --- Observer to Show/Hide Parameter Divs ---
    # Define which div corresponds to which model name (from checkbox choices)
    # model_param_panels <- list(
    #   ARIMA = ns("arima_accordion_panel"),
    #   Prophet = ns("prophet_accordion_panel"),
    #   XGBoost = ns("xgb_accordion_panel"),
    #   ETS = ns("ets_accordion_panel"),
    #   TBATS = ns("tbats_accordion_panel"),
    #   RF = ns("rf_accordion_panel"),
    #   GAM = ns("gam_accordion_panel")
    # )

    # --- UI Definitions for Accordion Panels ---
    # Define UI content for each model once. Use ns() for input IDs.
    # arima_panel_ui <- tagList(
    #   checkboxInput(ns("arima_auto"), "Use auto.arima? (Overrides manual settings)", value = TRUE),
    #   shinyjs::disabled( # Start disabled if auto is checked by default
    #     div(id = ns("arima_manual_params"),
    #         h4("Manual ARIMA Order"),
    #         fluidRow(column(4, numericInput(ns("arima_p"), "p (AR order)", min=0, step=1, value=1)),
    #                  column(4, numericInput(ns("arima_d"), "d (Differencing)", min=0, step=1, value=1)),
    #                  column(4, numericInput(ns("arima_q"), "q (MA order)", min=0, step=1, value=1))
    #         )
    #     )
    #   ),
    #   hr(), h4("Seasonal Component"),
    #   checkboxInput(ns("arima_seasonal"), "Include Seasonal Component (P, D, Q)?", value = TRUE), # Clarified label
    #   conditionalPanel(
    #   # JS Condition: !input['ID_auto'] && input['ID_seasonal']
    #   condition = paste0("!input['", ns("arima_auto"), "'] && input['", ns("arima_seasonal"), "']"),
    #   h4("Manual Seasonal Order (P, D, Q, Period)"),
    #   fluidRow(
    #     column(3, numericInput(ns("arima_P"), "Seasonal P", min=0, step=1, value=1)),
    #     column(3, numericInput(ns("arima_D"), "Seasonal D", min=0, step=1, value=1)),
    #     column(3, numericInput(ns("arima_Q"), "Seasonal Q", min=0, step=1, value=1)),
    #     column(3, numericInput(ns("arima_period"), "Seasonal Period", min=1, step=1, value=7)) # Removed placeholder previously
    #   )
    #   ) # End Conditional Panel for Manual Seasonal Inputs
    # )
    # ets_panel_ui <- tagList(
    #   checkboxInput(ns("ets_manual"), ...), hr(),
    #   # Remove conditionalPanels
    #   div(h5("Manual ETS Components:"), fluidRow(...), selectInput(ns("ets_damped"),...)),
    #   div(h5("Automatic ETS Selection:"), tags$p(...))
    # )
    # tbats_panel_ui <- tagList(p("TBATS models complex seasonality..."), ...)



    # prophet_panel_ui <- tagList(
    #   h5("Seasonality Components"), checkboxInput(ns("prophet_yearly"),...), # etc.
    #   hr(), h5("Growth Model"), selectInput(ns("prophet_growth"),...),
    #   # Remove conditionalPanel, always include capacity input
    #   numericInput(ns("prophet_capacity"), "Capacity (for logistic growth)", value = 1, min=0),
    #   hr(), h5("Hyperparameters"), numericInput(ns("prophet_changepoint_scale"),...),
    #   hr(), h5("Additional Features (Optional)"), fileInput(ns("prophet_holidays_file"),...), actionButton(ns("loadDefaultHolidays"),...),
    #   fileInput(ns("prophet_regressors_file"),...)
    # )
    # xgb_panel_ui <- tagList(
    #   h5("Tree Parameters"), fluidRow(...), fluidRow(...),
    #   hr(), h5("Regularization & Sampling"), fluidRow(...),
    #   hr(), helpText("Note: Time series features...")
    # )
    # gam_panel_ui <- tagList(h5("Configuration:"), selectInput(ns("gam_trend_type"), ...), checkboxInput(ns("gam_use_season_y"),...), ...)
    # rf_panel_ui <- tagList(h5("Configuration:"), fluidRow(...), fluidRow(...), hr(), helpText(...))

    # List mapping model names to their UI definitions
    # panel_ui_list <- list(
    #   ARIMA = arima_panel_ui
    #   # , Prophet = prophet_panel_ui, XGBoost = xgb_panel_ui,
    #   # ETS = ets_panel_ui, TBATS = tbats_panel_ui, RF = rf_panel_ui, GAM = gam_panel_ui
    # )
    # --- End UI Definitions ---


    # --- reactiveVal to track currently inserted panels ---
    # Initialize with default selection from UI checkboxGroupInput
    # currently_shown_panels_rv <- reactiveVal( isolate(input$selected_models %||% character(0)) )
    #

    # observeEvent(input$selected_models, {
    #   selected_now <- input$selected_models %||% character(0)
    #   # previously_shown <- currently_shown_panels_rv() %||% character(0)
    #   #
    #   # panels_to_remove <- setdiff(previously_shown, selected_now)
    #   # panels_to_insert <- setdiff(selected_now, previously_shown)
    #   #
    #   # message("Current Panels: ", paste(previously_shown, collapse=", "))
    #   message("Selected Models: ", paste(selected_now, collapse=", "))
    #   # message("Removing Panels: ", paste(panels_to_remove, collapse=", "))
    #   # message("Inserting Panels: ", paste(panels_to_insert, collapse=", "))
    #   #
    #   # # Remove panels first
    #   # for (model_name in panels_to_remove) {
    #   #   message("Removing panel: ", model_name)
    #   #   bslib::accordion_panel_remove(
    #   #     id = "model_params_accordion", # Target accordion ID (no ns() needed here)
    #   #     target = model_name          # Target panel using its 'value'
    #   #   )
    #   # }
    #   #
    #   # # icons_list = list(ARIMA="graph-up", Prophet="calendar-heart", XGBoost="tree", ETS="lightning-charge", TBATS="reception-4", RF="diagram-3", GAM="bezier")
    #   #
    #   # for (model_name in panels_to_insert) {
    #   #   message("Inserting panel: ", model_name)
    #   #   # Get the UI function/tagList for this model
    #   #   panel_content <- panel_ui_list[[model_name]]
    #   #   req(panel_content) # Ensure UI is defined for this model name
    #   #
    #   #   bslib::accordion_panel_insert(
    #   #     id = "model_params_accordion", # Target accordion ID (no ns() needed here)
    #   #     # Define the panel to insert
    #   #     panel = bslib::accordion_panel(
    #   #       title = paste(model_name, "Parameters"),
    #   #       value = model_name, # IMPORTANT: Set value to model name for removal targeting
    #   #       # icon = bslib::bs_icon(icons_list[[model_name]] %||% "gear"), # Add icon
    #   #       panel_content # Insert the UI controls
    #   #     ),
    #   #     # target = NULL, # Insert at the end by default
    #   #     # position = "after"
    #   #   )
    #   # }
    #
    #
    #
    #   # Loop through all possible models/divs
    #   # for (model_name in names(model_param_divs)) {
    #   #   div_id <- model_param_divs[[model_name]]
    #   #   if (model_name %in% selected) {
    #   #     shinyjs::show(id = div_id, anim = TRUE) # Show if selected
    #   #   } else {
    #   #     shinyjs::hide(id = div_id, anim = TRUE) # Hide if not selected
    #   #   }
    #   # }
    #   # message(print(names(model_param_divs)))
    #
    #   # lapply(names(model_param_panels), function(model_name){
    #   #   shinyjs::toggle(id = model_param_panels[[model_name]],
    #   #                   condition = model_name %in% selected,
    #   #                       animType = "fade",
    #   #                       anim = TRUE,
    #   #                       asis = TRUE
    #   #                   )
    #   #   # message(model_name,model_param_panels[[model_name]])
    #   # })
    #
    #   # for (model_name in names(model_param_panels)) {
    #   #   panel_id <- model_param_panels[[model_name]]
    #   #   # Use shinyjs::toggle based on selection
    #   #   shinyjs::toggle(
    #   #     id = panel_id, # Target the accordion panel's ID
    #   #     condition = model_name %in% selected,
    #   #     animType = "fade",
    #   #     anim = TRUE,
    #   #     asis = TRUE
    #   #   )
    #   #   message(print(paste(panel_id, model_name %in% selected)))
    #   # }
    #
    #   # for (model_name in names(model_param_divs)) {
    #   #   # Use shinyjs::toggle - condition is TRUE if model IS selected, FALSE otherwise
    #   #   shinyjs::toggle(
    #   #     id = model_param_divs[[model_name]],
    #   #     condition = model_name %in% selected,
    #   #     anim = TRUE,
    #   #     animType = "fade",
    #   #     asis = TRUE
    #   #   )
    #   #   # shinyjs::show(
    #   #   #   id = model_param_divs[[model_name]],
    #   #   #   anim = model_name %in% selected
    #   #   # )
    #
    #   # }
    # }, ignoreNULL = FALSE, ignoreInit = TRUE) # ignoreNULL=F runs when empty, ignoreInit=F runs on load
    # # --- End Show/Hide Observer ---


    # --- Return Reactive Values ---
    # Return a list where each element is a reactive expression
    # reading the corresponding input value.
    return(
      list(
        # selected_models = reactive({ input$selected_models }),

        use_arima = reactive({ input$use_arima }),
        use_prophet = reactive({ input$use_prophet }),
        use_xgboost = reactive({ input$use_xgboost }),
        use_ets = reactive({ input$use_ets }),
        use_tbats = reactive({ input$use_tbats }),
        use_rf = reactive({ input$use_rf }),
        use_gam = reactive({ input$use_gam }),

        # ARIMA
        arima_auto = reactive({ input$arima_auto }),
        arima_p = reactive({ input$arima_p }),
        arima_d = reactive({ input$arima_d }),
        arima_q = reactive({ input$arima_q }),
        arima_seasonal = reactive({ input$arima_seasonal }),
        arima_P = reactive({ input$arima_P }),
        arima_D = reactive({ input$arima_D }),
        arima_Q = reactive({ input$arima_Q }),
        arima_period = reactive({ input$arima_period }),

        # ETS
        ets_manual = reactive({ input$ets_manual }),
        ets_e = reactive({ input$ets_e }), # Component E ('Z', 'A', 'M')
        ets_t = reactive({ input$ets_t }), # Component T ('Z', 'N', 'A', 'M')
        ets_s = reactive({ input$ets_s }), # Component S ('Z', 'N', 'A', 'M')
        ets_damped_str = reactive({ input$ets_damped }), # Damped ('NULL', 'TRUE', 'FALSE') string

        # Prophet
        prophet_yearly = reactive({ input$prophet_yearly }),
        prophet_weekly = reactive({ input$prophet_weekly }),
        prophet_daily = reactive({ input$prophet_daily }),
        prophet_growth = reactive({ input$prophet_growth }),
        prophet_capacity = reactive({ input$prophet_capacity }),
        prophet_changepoint_scale = reactive({ input$prophet_changepoint_scale }),
        # Prophet Additions
        prophet_holidays_df = reactive({ processed_holidays_rv() }),
        prophet_regressors_df = reactive({ processed_regressors_rv() }),

        # XGBoost
        xgb_nrounds = reactive({ input$xgb_nrounds }),
        xgb_max_depth = reactive({ input$xgb_max_depth }),
        xgb_eta = reactive({ input$xgb_eta }),
        xgb_gamma = reactive({ input$xgb_gamma }),
        xgb_subsample = reactive({ input$xgb_subsample }),
        xgb_colsample = reactive({ input$xgb_colsample }),

        # GAM
        gam_trend_type = reactive({ input$gam_trend_type }), # 'smooth' or 'linear'
        gam_use_season_y = reactive({ input$gam_use_season_y }), # TRUE/FALSE
        gam_use_season_w = reactive({ input$gam_use_season_w }), # TRUE/FALSE

        # Random Forest
        rf_num_trees = reactive({ input$rf_num_trees }),
        rf_mtry = reactive({ input$rf_mtry }),
        rf_min_node_size = reactive({ input$rf_min_node_size }),

        # Common
        forecast_horizon = reactive({ input$forecastHorizon }),
        run_forecast_button = reactive({ input$runForecast }) # Reactive trigger for the button
        # active_tab = reactive({ input$modelTabs }) # <-- Add reactive for active tab
      )
    )

  })
}
