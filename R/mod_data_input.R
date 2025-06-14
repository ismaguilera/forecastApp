# R/mod_data_input.R

#' data_input UI Function
#' @description A shiny Module for handling data file input.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList fileInput selectInput actionButton
#' @importFrom DT DTOutput
mod_data_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fileInput(ns("fileUpload"), "Choose CSV or Excel File",
    fileInput(ns("fileUpload"), textOutput("ui_csv_file_upload", inline = TRUE),
              multiple = FALSE,
              accept = c(".csv",
                         ".xlsx",
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         "application/vnd.ms-excel")),
    # Wrap selectors in a conditional panel or disable initially - let's disable
    selectInput(ns("defaultDataSelect"), textOutput("ui_choose_default_dataset", inline = TRUE), choices = NULL), # Added selectInput
    actionButton(ns("loadDefault"), textOutput("ui_load_default_dataset", inline = TRUE), icon = icon("table"), class="btn-sm"), # Modified button text
    hr(), # Add separator
    # selectInput(ns("dateCol"), "Select Date Column", choices = NULL),
    # selectInput(ns("valueCol"), "Select Value Column", choices = NULL),
    # DT::DTOutput(ns("dataPreview"))
    tabsetPanel(
      id = ns("modelTabs"),
      type = "tabs",

      # --- ARIMA Tab ---
      tabPanel(textOutput("ui_select_columns"),
               br(),
               selectInput(ns("dateCol"),  textOutput("ui_select_date_col", inline = TRUE), choices = NULL),
               selectInput(ns("valueCol"), textOutput("ui_select_value_col", inline = TRUE), choices = NULL)
      ),
      # tabPanel("Select Columns",
      #          br(),
      #          selectInput(ns("dateCol"), "Select Date Column", choices = NULL),
      #          selectInput(ns("valueCol"), "Select Value Column", choices = NULL)
      # ),
      # tabPanel("Preview",
      tabPanel(textOutput("ui_preview", inline = TRUE),
               br(),
               DT::DTOutput(ns("dataPreview"))
      )
    )
  )
}

#' data_input Server Functions
#'
#' @description Server logic for the data input module. Handles file reading,
#'   validation, and populating column selectors.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @return A reactive list containing: `reactive_df`, `reactive_date_col`, `reactive_value_col`.
#'
#' @noRd
#'
#' @import shiny
#' @import readxl
#' @import dplyr
#' @import lubridate
#' @importFrom tools file_ext
#' @importFrom utils read.csv head
#' @importFrom magrittr %>%
#' @importFrom DT renderDT datatable DTOutput
#' @import data.table
mod_data_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- Populate Default Dataset Selector ---
    # List .csv files in inst/extdata
    available_default_files <- list.files(
      app_sys("extdata"), # Correctly uses golem's helper to find inst/extdata
      pattern = "\\.csv$", # Filter for .csv files
      full.names = FALSE   # Get only filenames, not full paths
    )
    # Update the selectInput choices
    updateSelectInput(session, "defaultDataSelect",
                      choices = available_default_files,
                      selected = if(length(available_default_files) > 0) available_default_files[[1]] else NULL)
    # --- End Populate Selector ---

    # Use a reactiveVal to store the data source trigger
    # It updates on file upload OR default button click
    # data_trigger <- reactiveVal(NULL) # No longer needed in this form

    # New reactiveVal to store the processed dataframe
    reactive_df_data <- reactiveVal(NULL)

    # --- Observer for File Upload ---
    observeEvent(input$fileUpload, {
      req(input$fileUpload)
      message("File upload triggered.") # Debug message

      shiny::withProgress(message = 'Loading uploaded file...', value = 0, {
        shiny::incProgress(0.1, detail = "Reading file...")
        trigger <- input$fileUpload
        ext <- tools::file_ext(trigger$name)

        df <- tryCatch({
          if (ext == "csv") {
            message("Reading CSV file from upload using data.table::fread.") # Debug message
            data.table::fread(trigger$datapath, stringsAsFactors = FALSE, data.table = FALSE)
          } else if (ext == "xlsx") {
            message("Reading Excel file from upload.") # Debug message
            readxl::read_excel(trigger$datapath)
          } else {
            stop(paste0("Unsupported file type: '.", ext, "'. Please upload a .csv or .xlsx file."))
          }
        }, error = function(e) {
          user_message <- paste0("Error reading file '", trigger$name, "'. Ensure it is a valid CSV or XLSX file, not corrupted, and the extension matches the content. Specific error: ", conditionMessage(e))
          shiny::showNotification(user_message, type = "error", duration = 15)
          message(user_message)
          return(NULL)
        })

        shiny::incProgress(0.7, detail = "Validating data structure...")
        if (!is.null(df)) {
          if (ncol(df) < 2) {
            shiny::showNotification("Data must have at least two columns (one for dates, one for values).", type = "error", duration = 10)
            reactive_df_data(NULL)
          } else {
            shiny::showNotification("File uploaded successfully!", type = "message", duration = 5)
            message(paste("Successfully read uploaded file. Dimensions:", nrow(df), "rows,", ncol(df), "columns."))
            reactive_df_data(df)
          }
        } else {
          reactive_df_data(NULL) # Set to NULL if reading failed (error already shown)
        }
        shiny::incProgress(1, detail = "Load complete.")
      }) # End withProgress
    })

    # --- Observer for Load Default Button ---
    observeEvent(input$loadDefault, {
      message("Load default button triggered.") # Debug message
      selected_file_name <- input$defaultDataSelect

      # Validate that a file is selected
      validate(
        need(!is.null(selected_file_name) && selected_file_name != "", "Please select a default dataset to load.")
      )
      message(paste("Selected default file:", selected_file_name)) # Debug message

      shiny::withProgress(message = 'Loading default dataset...', value = 0, {
        shiny::incProgress(0.1, detail = "Locating file...")
        # Construct path using app_sys and the selected filename
        default_file_path <- app_sys("extdata", selected_file_name)

        validate(
          need(file.exists(default_file_path), paste("Selected default data file not found at:", default_file_path))
        )
        shiny::incProgress(0.3, detail = "Reading file...")
        df <- tryCatch({
          message(paste("Reading selected default CSV file:", selected_file_name, "using data.table::fread")) # Debug message
          data.table::fread(default_file_path, stringsAsFactors = FALSE, data.table = FALSE)
        }, error = function(e) {
          user_message <- paste0("Error reading default dataset '", selected_file_name, "'. Ensure the file is a valid CSV and not corrupted. Specific error: ", conditionMessage(e))
          shiny::showNotification(user_message, type = "error", duration = 15)
          message(user_message)
          return(NULL)
        })

        shiny::incProgress(0.7, detail = "Validating data structure...")
        if (!is.null(df)) {
          if (ncol(df) < 2) {
            shiny::showNotification(paste0("Default dataset '", selected_file_name, "' must have at least two columns."), type = "error", duration = 10)
            reactive_df_data(NULL)
          } else {
            shiny::showNotification("Selected default dataset loaded successfully!", type = "message", duration = 5)
            message(paste("Successfully read selected default file. Dimensions:", nrow(df), "rows,", ncol(df), "columns."))
            reactive_df_data(df)
          }
        } else {
          reactive_df_data(NULL) # Set to NULL if reading failed (error already shown)
        }
        shiny::incProgress(1, detail = "Load complete.")
      }) # End withProgress
    })

    # --- Observer to update column selectors when data changes ---
    observeEvent(reactive_df_data(), {
      df <- reactive_df_data()
      message("reactive_df_data updated. Updating select inputs.") # Debug message
      if (!is.null(df) && ncol(df) > 0) {
        col_names <- colnames(df)
        potential_date_col <- grep("date|time", col_names, ignore.case = TRUE, value = TRUE)
        selected_date <- if(length(potential_date_col) > 0) potential_date_col[[1]] else col_names[[1]]
        numeric_cols <- df %>% dplyr::select_if(is.numeric) %>% colnames()
        potential_value_col <- setdiff(numeric_cols, selected_date)
        if (length(potential_value_col) == 0) {
          potential_value_col <- if(length(col_names)>1) setdiff(col_names, selected_date)[1] else NULL
        } else {
          potential_value_col <- potential_value_col[[1]]
        }
        selected_value <- if(!is.null(potential_value_col)) potential_value_col else if(length(col_names)>1) col_names[[2]] else col_names[[1]]
        updateSelectInput(session, "dateCol", choices = col_names, selected = selected_date)
        updateSelectInput(session, "valueCol", choices = col_names, selected = selected_value)
      } else {
        updateSelectInput(session, "dateCol", choices = character(0))
        updateSelectInput(session, "valueCol", choices = character(0))
      }
    })

    # --- Data Preview Table ---
    output$dataPreview <- DT::renderDT({
      df <- reactive_df_data()
      req(df)
      message("Rendering data preview table.") # Debug message
      DT::datatable(
        utils::head(df, 10),
        options = list(pageLength = 5, scrollX = TRUE, searching = FALSE, lengthChange = FALSE),
        rownames = FALSE, selection = 'none'
      )
    })

    # --- Return values ---
    return(
      list(
        reactive_df = reactive_df_data, # Return the reactiveVal holding the dataframe
        reactive_date_col = reactive({ input$dateCol }),
        reactive_value_col = reactive({ input$valueCol })
      )
    )
    # --- End server logic ---

  })
}
