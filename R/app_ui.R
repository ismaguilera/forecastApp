# R/app_ui.R

#' The application User-Interface using bslib::page_navbar
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @import shiny.i18n
#' @import rintrojs
#' @import bslib
#' @import shinybusy
#' @noRd

i18n <- Translator$new(translation_json_path = app_sys("i18n",'translation.json'))

app_ui <- function(request) {
  tagList(
    # Required for rintrojs (if still using the tour)
    shinyjs::useShinyjs(), # Ensure this is present ONCE in the main UI
    shinybusy::add_busy_spinner(spin = "fading-circle", position = "top-right", margins = c(10, 20)),
    rintrojs::introjsUI(),
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Top-level container: page_navbar
    bslib::page_navbar(
      id = "navbar", # Add an ID to the navbar
      title = uiOutput("ui_page_title"), # Page title
      window_title = "Forecast App", # Browser tab title
      theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
      # theme = bslib::bs_theme(version = 5, bootswatch = "cosmo", bg = "#2c3e50", fg = "#ffffff"), # Apply theme (change "cerulean" if desired)
      # Collapsible navigation on smaller screens
      # collapsible = TRUE,
      # Inverse theme for navbar (optional)
      # inverse = TRUE,

      # --- Data Panel ---
      bslib::nav_panel(
        title = tagList(shiny::icon("table"), textOutput("ui_nav_data", inline = TRUE)),
        # Internal layout for this panel
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            div(id = "data_sidebar_div", # Wrap sidebar content in a div for easy disabling
                title = textOutput("ui_sidebar_data", inline = TRUE), # Dynamic sidebar title
                width = 350, # Adjust width as needed
                # Placeholder for Data Input Module UI
                # tags$p("Placeholder for Data Upload controls (e.g., mod_data_input_ui)"),
                # tags$p("Placeholder for Column Selection controls"),
                mod_data_input_ui("data_input_1"), # Module UI would go here

                hr(), # Separator
                h5(textOutput("ui_load_holidays",inline=TRUE)), # Section title
                # fileInput("global_holidays_file", "Upload Global Holidays File (CSV: ds, holiday)",
                fileInput("global_holidays_file", textOutput("ui_upload_global_holidays", inline = TRUE),
                          multiple = FALSE,
                          accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),
                # actionButton("load_default_holidays", "Load Default Holidays", icon = icon("calendar-check"), class="btn-sm") # Add button
                actionButton("load_default_holidays", textOutput("ui_load_holidays_default", inline = TRUE), icon = icon("calendar-check"), class="btn-sm") # Add button
            ) # End of data_sidebar_div
          ),
          # Main content for Data panel
          # bslib::card(
          #   bslib::card_header("Global Holidays Data (Preview)"),
          #   bslib::card_body(
          #     verbatimTextOutput("global_holidays_preview")
          #   )
          # ),
          bslib::accordion(
            id = "accordion_data", # Add id
            open = c("Preprocessing & Split","Time Series Decomposition"),
            bslib::accordion_panel(
              title = uiOutput("ui_accordion_preprocess_title", inline = TRUE), # "Preprocessing & Split"
              mod_preprocess_controls_ui("preprocess_controls_1")
            ),
            bslib::accordion_panel(
              title = uiOutput("ui_accordion_decomposition_title", inline = TRUE), # "Time Series Decomposition"
              full_screen = TRUE, # Added full_screen here
              mod_decomposition_plot_ui("decomposition_plot_1")
            )
          )

          # bslib::card(
          #   bslib::card_header("Preprocessing & Split"),
          #   bslib::card_body(
          #     min_height = 500,
          #     full_screen = TRUE,
          #     # Placeholder for Preprocessing Controls Module UI
          #     # tags$p("Placeholder for Aggregation, Train/Test Split controls (e.g., mod_preprocess_controls_ui)"),
          #     mod_preprocess_controls_ui("preprocess_controls_1") # Module UI would go here
          #   )
          # ),
          # bslib::card( # Card for decomposition plot
          #   bslib::card_header("Time Series Decomposition"),
          #   bslib::card_body(
          #     min_height = 500,
          #     full_screen = TRUE,
          #     mod_decomposition_plot_ui("decomposition_plot_1") # Call new module UI
          #   )
          # )
        ) # End layout_sidebar for Data panel
      ), # End Data nav_panel

      # --- Model Panel ---
      bslib::nav_panel(
        title = tagList(shiny::icon("gears"),textOutput("ui_nav_model", inline = TRUE)),
        div(id = "model_config_div", # Wrap model panel content in a div
            # Internal layout for this panel
            # bslib::layout_sidebar(
            #   sidebar = bslib::sidebar(
            #     title = "Models"
            #     # width = 350,
            #     # Placeholder for Model Selection/Config Module UI
            #     # tags$p("Placeholder for Model Configuration Tabs (e.g., mod_model_config_ui)"),
            #     # tags$p("Placeholder for Run Forecast Button"),
            #      # Module UI would go here
            #   ), # End sidebar
            # Main content for Model panel
            # bslib::card(
            # bslib::card_header("Models"),
            # bslib::card_body(
            # Placeholder - Config details might be primarily in sidebar
            # tags$p("Detailed model parameters appear in the sidebar.")
            mod_model_config_ui("model_config_1")
            # )
            # )
            # ) # End layout_sidebar for Model panel
        ) # End of model_config_div
      ), # End Model nav_panel

      # --- Forecast Results Panel ---
      bslib::nav_panel(
        title = tagList(shiny::icon("chart-line"),textOutput("ui_nav_forecast_results", inline = TRUE)),
        # Internal layout for this panel
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = uiOutput("ui_sidebar_model_summary_title", inline = TRUE), # "Model summary"
            width = 350,
            # Placeholder for Model Summary Module UI
            # tags$p("Placeholder for text summary of the run model (e.g., mod_model_summary_ui)"),
            mod_model_summary_ui("model_summary_1") # Module UI would go here
          ), # End sidebar
          # Main content for Results panel (arranged vertically)
          bslib::card(
            bslib::card_body(
              # Add padding or alignment if needed
              div(style = "display: flex; justify-content: flex-end; align-items: center;", # Align button right & vertically center
                  uiOutput("ui_download_forecast_button_placeholder", inline = TRUE),
                  # Spacing
                  tags$span(style="margin-left: 20px;"),
                  # Report Format Radio Buttons
                  radioButtons(
                    inputId = "reportFormat", # Use inputId for direct use in app_ui
                    label = NULL, # Keep it compact
                    choices = list("HTML" = "html", "PDF" = "pdf"),
                    selected = "html",
                    inline = TRUE
                  ),
                  # Spacing
                  tags$span(style="margin-left: 10px;"),
                  # New Download Report Button
                  uiOutput("ui_download_report_button_placeholder", inline = TRUE)
              )
            )
          ),
          bslib::navset_card_underline(
            title = uiOutput("ui_visualizations_title", inline = TRUE), # "Visualizations"
            # Panel with plot ----
            bslib::nav_panel(title = uiOutput("ui_plot_nav_title", inline = TRUE), h1="Forecast plot", mod_results_plot_ui("results_plot_1"), full_screen = TRUE),

            # Panel with summary ----
            bslib::nav_panel(title = uiOutput("ui_performance_nav_title", inline = TRUE), h1="Model performance metrics", mod_results_table_ui("results_table_1"), full_screen = TRUE),

            # Panel with table ----
            bslib::nav_panel(title = uiOutput("ui_extra_plots_nav_title", inline = TRUE), h1="Additional plots", mod_extra_plots_ui("extra_plots_1"), full_screen = TRUE)
          )

          # bslib::card(
          #   bslib::card_header("Forecast plot"),
          #   bslib::card_body(
          #     # Placeholder for Results Plot Module UI
          #     min_height = 400,
          #     # tags$p("Placeholder for main forecast plot (e.g., mod_results_plot_ui)"),
          #     mod_results_plot_ui("results_plot_1") # Module UI would go here
          #   )
          # ),
          # bslib::card(
          #   bslib::card_header("Model performance metrics"),
          #   bslib::card_body(
          #     # Placeholder for Metrics Table Module UI
          #     min_height = 250,
          #     # tags$p("Placeholder for metrics table (e.g., mod_results_table_ui)"),
          #     mod_results_table_ui("results_table_1") # Module UI would go here
          #   )
          # ),
          # bslib::card(
          #   bslib::card_header("Additional plots"),
          #   bslib::card_body(
          #     # Placeholder for Extra Plots Module UI
          #     min_height = 500,
          #     # tags$p("Placeholder for cumulative/yearly plots (e.g., mod_extra_plots_ui)"),
          #     mod_extra_plots_ui("extra_plots_1") # Module UI would go here
          #   )
          # )
        ) # End layout_sidebar for Results panel
      ), # End Results nav_panel

      # --- Validation Panel ---
      bslib::nav_panel( # This is now handled by the module UI
        title = tagList(shiny::icon("circle-check"),textOutput("ui_nav_validation", inline = TRUE)),
        mod_validation_ui("validation_1") 
      ),
       # Call the module UI here

      # --- Footer Definition ---
      # footer = tags$div(
      #   style = "text-align: center; padding: 15px; background-color: #f5f5f5; border-top: 1px solid #e3e3e3;",
      #   tags$p(tags$strong(tagList(shiny::icon("circle-info"), " About the app:")), "Vaccine Forecasting Application: This application allows forecasting of vaccine doses using multiple time series models."),
      #   tags$p(tags$strong("Features:"), "Loading data from CSV/Excel files, Configuration of multiple models (ARIMA, Prophet, XGBoost), Switching between daily and weekly time series, Interactive visualization of results, Validation of forecasts with real data."),
      #   tags$p(tags$strong("Developed with:"), "R and Shiny."),
      #   tags$p(tags$strong("Packages:"), tags$code("bslib, shiny, golem, dplyr, lubridate, plotly, forecast, prophet, xgboost, recipes, yardstick, readxl, DT, rintrojs, tidyr, tibble, slider, timetk, etc."))
      # )
      bslib::nav_panel(
        title = tagList(shiny::icon("circle-info"),textOutput("ui_nav_about", inline = TRUE)),
        # Internal layout for this panel
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = uiOutput("ui_about_app_sidebar_title", inline = TRUE),
            width = 350,
            # Placeholder for Validation Configuration?
            tags$p("This application allows forecasting of vaccine doses using multiple time series models."),
          ), # End sidebar

          bslib::card(
            full_screen = TRUE, # Added full_screen here
            bslib::card_header(tags$strong("Vaccine Forecasting Application:")),
            bslib::card_body(
              # Placeholder for validation outputs
              tags$p(tags$strong("Features:"),
                     tags$ul(tags$li("Loading data from CSV/Excel files,"),
                             tags$li("Configuration of multiple models (ARIMA, Prophet, XGBoost),"),
                             tags$li("Switching between daily and weekly time series,"),
                             tags$li("Interactive visualization of results,"),
                             tags$li("Validation of forecasts with real data."))),
              tags$p(tags$strong("Developed with:"), "R and Shiny."),
              tags$p(tags$strong("Packages:"), tags$code("bslib, shiny, golem, dplyr, lubridate, plotly, forecast, prophet, xgboost, recipes, yardstick, readxl, DT, rintrojs, tidyr, tibble, slider, timetk, etc."))
            ),
            footer =bslib::card_footer(
              class = "fs-6",
              "Copyright 2025 DESAL, MINSAL"
            )
          )
        ) # End layout_sidebar for Validation panel
      ),
      bslib::nav_spacer(), # Adds space before right-aligned items
      bslib::nav_item(
        selectInput(inputId ="selected_language",
                           label = NULL, # Fixed label for now
                           choices = c("English" = "en", "Español" = "es"),
                           selected = i18n$get_key_translation(), # Obtiene el idioma actual
                           width = "120px") # Adjust width as needed
      ),
      bslib::nav_item(
        uiOutput("ui_save_session_button_placeholder", inline = TRUE)
      ),
      bslib::nav_item(
        uiOutput("ui_load_session_button_placeholder", inline = TRUE)
      ),
      footer =bslib::card_footer(
        class = "fs-6",
        "Copyright 2025 DESAL, MINSAL. Versión 1.0"
      )
      # footer =bslib::card(
      #   bslib::card_header(tags$strong(tagList(shiny::icon("circle-info"), " About the app:"))),
      #   bslib::card_body(
      #     tags$p(tags$strong("Vaccine Forecasting Application:"),"This application allows forecasting of vaccine doses using multiple time series models."),
      #     tags$p(tags$strong("Features:"), "Loading data from CSV/Excel files, Configuration of multiple models (ARIMA, Prophet, XGBoost), Switching between daily and weekly time series, Interactive visualization of results, Validation of forecasts with real data."),
      #     tags$p(tags$strong("Developed with:"), "R and Shiny."),
      #     tags$p(tags$strong("Packages:"), tags$code("bslib, shiny, golem, dplyr, lubridate, plotly, forecast, prophet, xgboost, recipes, yardstick, readxl, DT, rintrojs, tidyr, tibble, slider, timetk, etc."))
      #   ),
      # )
      # --- End Footer ---

    ) # End page_navbar
  ) # End tagList
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @description Adds resources from the 'app/www' folder to the Shiny UI.
#' Includes standard golem favicon and bundle resources functionality.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # Ensure golem is listed in DESCRIPTION Imports

  # Add resources from the app/www folder
  add_resource_path(
    prefix = "www", # Standard prefix for www resources
    directoryPath = app_sys("app/www") # Finds the www folder using golem's app_sys
  )

  # Add other head tags like favicon and essential resources
  tags$head(
    favicon(), # Adds favicon based on golem defaults or settings
    bundle_resources( # Bundles resources from the specified path
      path = app_sys("app/www"),
      app_title = "forecastApp" # Title for the app
    )
    # Add other external resources here if needed, e.g.:
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # shinyalert::useShinyalert() # If using shinyalert
  )
}
