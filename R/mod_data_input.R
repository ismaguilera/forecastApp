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
    fileInput(ns("fileUpload"), "Choose CSV or Excel File",
              multiple = FALSE,
              accept = c(".csv",
                         ".xlsx",
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         "application/vnd.ms-excel")),
    # Wrap selectors in a conditional panel or disable initially - let's disable
    actionButton(ns("loadDefault"), "Load Default Dataset", icon = icon("table"), class="btn-sm"), # Add button
    hr(), # Add separator
    # selectInput(ns("dateCol"), "Select Date Column", choices = NULL),
    # selectInput(ns("valueCol"), "Select Value Column", choices = NULL),
    # DT::DTOutput(ns("dataPreview"))
    tabsetPanel(
      id = ns("modelTabs"),
      type = "tabs",

      # --- ARIMA Tab ---
      tabPanel("Select Columns",
               br(),
               selectInput(ns("dateCol"), "Select Date Column", choices = NULL),
               selectInput(ns("valueCol"), "Select Value Column", choices = NULL)
      ),
      tabPanel("Preview",
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
mod_data_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Use a reactiveVal to store the data source trigger
    # It updates on file upload OR default button click
    # data_trigger <- reactiveVal(NULL) # No longer needed in this form

    # New reactiveVal to store the processed dataframe
    reactive_df_data <- reactiveVal(NULL)

    # --- Observer for File Upload ---
    observeEvent(input$fileUpload, {
      req(input$fileUpload)
      message("File upload triggered.") # Debug message

      trigger <- input$fileUpload
      ext <- tools::file_ext(trigger$name)

      df <- tryCatch({
        if (ext == "csv") {
          message("Reading CSV file from upload.") # Debug message
          utils::read.csv(trigger$datapath, stringsAsFactors = FALSE)
        } else if (ext == "xlsx") {
          message("Reading Excel file from upload.") # Debug message
          readxl::read_excel(trigger$datapath)
        } else {
          stop("Unsupported file type. Please upload a .csv or .xlsx file.")
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Error reading uploaded file:", e$message),
          type = "error",
          duration = 10
        )
        message(paste("Error reading uploaded file:", e$message)) # Debug message
        return(NULL)
      })

      if (!is.null(df)) {
        shiny::showNotification("File uploaded successfully!", type = "message", duration = 5)
        message(paste("Successfully read uploaded file. Dimensions:", nrow(df), "rows,", ncol(df), "columns.")) # Debug message
        reactive_df_data(df) # Update the reactiveVal with the dataframe
      } else {
        reactive_df_data(NULL) # Set to NULL if reading failed
      }
    })

    # --- Observer for Load Default Button ---
    observeEvent(input$loadDefault, {
      message("Load default button triggered.") # Debug message
      # --- Load Default Data ---
      default_file_name <- get_golem_config("default_data_file")
      # Check if config value exists
      validate(need(!is.null(default_file_name), "Default data file not specified in config."))
      # Construct path using app_sys
      default_file_path <- app_sys("extdata", default_file_name)

      validate(need(file.exists(default_file_path), paste("Default data file not found at:", default_file_path)))

      df <- tryCatch({
        # Assuming CSV for default, add checks/options if needed
        message("Reading default CSV file.") # Debug message
        utils::read.csv(default_file_path, stringsAsFactors = FALSE)
      }, error = function(e) {
        shiny::showNotification(paste("Error reading default file:", e$message), type = "error", duration = 10)
        message(paste("Error reading default file:", e$message)) # Debug message
        return(NULL)
      })
      if (!is.null(df)) {
        shiny::showNotification("Default dataset loaded successfully!", type = "message", duration = 5)
        message(paste("Successfully read default file. Dimensions:", nrow(df), "rows,", ncol(df), "columns.")) # Debug message
        reactive_df_data(df) # Update the reactiveVal with the dataframe
      } else {
        reactive_df_data(NULL) # Set to NULL if reading failed
      }
      # --- End Load Default ---
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
