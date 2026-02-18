library(shiny)
library(shinyFiles)
library(DT)
library(shinyjs)
library(dplyr)

# Load in function , for local testing add the app_folder/
source("createSAMMfile.R")

# App creation
ui <- fluidPage(
  shinyjs::useShinyjs(),  # Initialize shinyjs
  shiny::titlePanel("SAMM File Creation Tool"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("model", "Model", choices = c("glucose", "cpeptide")),
      shiny::fileInput("file", "Choose CSV File", accept = ".csv"),
      shiny::textOutput("output_dir_display"),  # Display selected output directory

      # Baseline fasted timepoint selection
      fluidRow(
        column(10, shiny::selectInput("fasted_option", "Baseline Fasted Timepoint",
                                      choices = c("No" = "no", "Yes" = "yes"),
                                      selected = NULL)),
        column(2, actionButton("fasted_option", label = "", icon = icon("info-circle"), style = "link", size = "small"))
      ),
      shiny::conditionalPanel(
        condition = "input.fasted_option == 'yes'",
        shiny::tagList(
          shiny::textInput("fasted_timepoint", "Enter fasted timepoint in minutes ie. -10", value = "-"),
          tags$script(HTML("
    $(document).on('input', '#fasted_timepoint', function() {
      let input = $(this).val();

      // Ensure input starts with '-'
      if (!input.startsWith('-')) {
        input = '-' + input.replace(/-/g, '');
      }

      $(this).val(input);
    });
  "))
        )),
      # Dropdown for number of points with custom options and clickable info button
      fluidRow(
        column(10, shiny::selectInput("npoints", "Number of Points",
                                      choices = c("5" = 5, "7" = 7, "5 (custom)" = "5_custom", "7 (custom)" = "7_custom"),
                                      selected = NULL)),
        column(2, actionButton("npoints_info", label = "", icon = icon("info-circle"), style = "link", size = "small"))
      ),

      # Conditional input for custom time points with an info button
      shiny::conditionalPanel(
        condition = "input.npoints == '5_custom' || input.npoints == '7_custom'",
        fluidRow(
          column(10, shiny::textInput("timepoints", "Custom Time Points (comma-separated)", value = "")),
          column(2, actionButton("timepoints_info", label = "", icon = icon("info-circle"), style = "link", size = "small"))
        ),
        shiny::actionButton("submit_timepoints", "Submit Timepoints", disabled = TRUE)  # Initially disabled
      ),

      # Model type  settings
      shiny::conditionalPanel(
        condition = "input.model == 'glucose'",
        shiny::numericInput("FSD", "FSD (default 0.02)", value = 0.02),
        shiny::numericInput("glucose_protocol", "Glucose Protocol (default to 1 for 75g glucose OGTT)", value = 1)
      ),
      shiny::conditionalPanel(
        condition = "input.model == 'cpeptide'",
        shiny::numericInput("GEN1", "GEN1 (default 2000)", value = 2000),
        shiny::numericInput("GEN2", "GEN2 (default 0.001)", value = 0.001),
        shiny::numericInput("GEN3", "GEN3 (default 2)", value = 2)
      ),

      shiny::actionButton("run", "Create SAMM Files", disabled = TRUE),  # Run button initially disabled
      shinyjs::hidden(shiny::downloadButton("download", "Download SAMM Files"))  # Add download button
    ),

    shiny::mainPanel(
      DT::DTOutput("uploaded_data")
    )
  )
)
server <- function(input, output, session) {
  # Create a reactive value to store log messages
  log_messages <- reactiveVal(character())

  # Enable file system access for the output directory selection
  shinyFiles::shinyDirChoose(input, "outputdir", roots = c(home = "~"), session = session)

  # Define a dedicated directory for output files within tempdir
  output_dir_path <- reactive({
    file.path(tempdir(), "SAMM_output_files")
  })

  # Ensure the output directory is clean before each run and when inputs change
  observe({
    dir.create(output_dir_path(), showWarnings = FALSE, recursive = TRUE)
    unlink(file.path(output_dir_path(), "*"), recursive = TRUE)
  }) %>% bindEvent(
    input$model, input$file, input$fasted_option, input$npoints, input$timepoints,
    input$FSD, input$glucose_protocol, input$GEN1, input$GEN2, input$GEN3
  )

  # Hide the download button when any relevant input changes
  observe({
    shinyjs::hide("download")
  }) %>% bindEvent(
    input$model, input$file, input$fasted_option, input$npoints, input$timepoints,
    input$FSD, input$glucose_protocol, input$GEN1, input$GEN2, input$GEN3
  )

  # Show information when number of points info button is clicked
  observeEvent(input$npoints_info, {
    showModal(modalDialog(
      title = "Number of Points Information",
      "Selecting '5' uses timepoints: 0, 30, 60, 90, 120. Selecting '7' uses timepoints: 0, 10, 20, 30, 60, 90, 120. Selecting custom enables input of your own time point values.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Show specifications when custom timepoints info button is clicked
  observeEvent(input$timepoints_info, {
    showModal(modalDialog(
      title = "Custom Time Points Specifications",
      "Please enter custom time points as a comma-separated list. The requirements are: For '5 (custom)' select exactly 5 time points. For '7 (custom)' select exactly 7 time points. The first time point must be '0'. Only numeric values are accepted.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Show info about baseline timepoint button is clicked
  observeEvent(input$fasted_option, {
    showModal(modalDialog(
      title = "Baseline fasted timepoint",
      "If you select 'Yes' you are indicating the existence of the measurement of a baseline fasted timepoint in your OGTT i.e., -10 minute time point = 10 minutes before glucose consumed.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Store uploaded data in a reactive variable
  uploaded_data <- reactive({
    req(input$file)
    rio::import(input$file$datapath)
  })

  output$uploaded_data <- DT::renderDT({
    req(uploaded_data())
  })

  # Monitor the timepoints input and enable "Submit Timepoints" only when it is non-empty and valid
  observe({
    if (input$npoints == "5_custom" || input$npoints == "7_custom") {
      timepoints <- as.numeric(unlist(strsplit(input$timepoints, ",")))
      expected_count <- ifelse(input$npoints == "5_custom", 5, 7)
      is_valid <- !is.null(timepoints) && length(timepoints) == expected_count && timepoints[1] == 0
      shinyjs::toggleState("submit_timepoints", condition = is_valid)
    }
  })

  # Enable "Run" button when all required inputs are filled and valid
  observe({
    all_inputs_filled <- !is.null(input$file)

    # Check for custom timepoints if applicable
    if (input$npoints == "5_custom" || input$npoints == "7_custom") {
      timepoints <- as.numeric(unlist(strsplit(input$timepoints, ",")))
      expected_count <- ifelse(input$npoints == "5_custom", 5, 7)
      timepoints_valid <- length(timepoints) == expected_count && timepoints[1] == 0
    } else {
      timepoints_valid <- TRUE
    }

    # Check model-specific inputs
    model_inputs_filled <- if (input$model == "glucose") {
      !is.null(input$FSD) && !is.null(input$glucose_protocol)
    } else {
      !is.null(input$GEN1) && !is.null(input$GEN2) && !is.null(input$GEN3)
    }

    # Enable "Run" button if all conditions are met
    shinyjs::toggleState("run", all_inputs_filled && timepoints_valid && model_inputs_filled)
  })

  # Run the SAMM file creation when the button is pressed
  observeEvent(input$run, {
    req(uploaded_data(), output_dir_path())

    # Custom time points as a numeric vector
    timepoints <- if (input$timepoints == "") NULL else as.numeric(unlist(strsplit(input$timepoints, ",")))

    # Capture log messages from createSAMMfile
    log_messages(character())  # Reset log messages before running
    result <- if (input$model == "glucose") {
      createSAMMfile(inputfile = input$file$datapath,
                     outputdir = output_dir_path(),
                     npoints = input$npoints,
                     fasted_timepoint = input$fasted_timepoint,
                     timepoints = timepoints,
                     model = input$model,
                     glucose_protocol = input$glucose_protocol,
                     FSD = input$FSD)
    } else {
      createSAMMfile(inputfile = input$file$datapath,
                     outputdir = output_dir_path(),
                     npoints = input$npoints,
                     fasted_timepoint = input$fasted_timepoint,
                     timepoints = timepoints,
                     model = input$model,
                     GEN1 = input$GEN1,
                     GEN2 = input$GEN2,
                     GEN3 = input$GEN3)
    }

    # Update log_messages with the result from createSAMMfile, or leave empty if no result
    log_messages(if (!is.null(result)) result else character())

    # Display log messages in the UI
    output$log_output <- renderText({
      if (length(log_messages()) == 0) return("No logs yet.")
      paste(log_messages(), collapse = "\n")
    })

    # Display notifications based on log messages
    if (length(log_messages()) > 0) {
      for (msg in log_messages()) {
        showNotification(msg, type = ifelse(grepl("error|missing", msg, ignore.case = TRUE), "error", "message"))
      }
    }

    # Final success notification if no errors
    if (all(!grepl("error|missing", log_messages(), ignore.case = TRUE))) {
      showNotification("SAMM file generation completed", type = "message")
      shinyjs::show("download")  # Show the download button
    }
  })

  # Download Handler to allow user to download the SAMM files as a ZIP
  output$download <- downloadHandler(
    filename = function() {
      paste0("SAMM_files_", input$model, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create a zip file of only the SAMM output files
      files <- list.files(output_dir_path(), full.names = TRUE)
      zip::zipr(zipfile = file, files = files, recurse = FALSE)
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)

#rsconnect::deployApp("app folder/", appName = "SAMMII_creation_tool")
