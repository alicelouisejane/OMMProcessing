library(shiny)
library(rio)
library(shinyjs)
library(DT)
library(zip)
library(ggplot2)
library(gtsummary)
library(gt)

# Source the extraction function
source("extractSAMMresults_forapp.R")

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs for showing/hiding elements
  titlePanel("SAMM Results Extractor Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_phi", "Upload Phi Results CSV File", accept = ".csv"),
      fileInput("file_si", "Upload SI Results CSV File", accept = ".csv"),
      actionButton("process", "Process Data"),
      hr(),
      hidden(downloadButton("download", "Download Cleaned Results")),  # Hide download button initially
      hr()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Minimal Model Results Dataset", DTOutput("cleaned_data")),
        tabPanel("Error messages and Parameter Summaries",
                 uiOutput("error_summary"),  # Descriptive text output
                 dataTableOutput("cv_param_summary_table")  # Summary table output
        ),
        tabPanel("Visualisations", plotOutput("viz"))
        )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) # 30MB FILE UPLOAD RESTRICTION CHANGE IF NEEDED

  # Reactive values to store the final data and different stages of data processing for sumamry tabs
  final_output_long <- reactiveVal(NULL)
  final_output <- reactiveVal(NULL)
  output_dir_path <- tempdir()  # Temporary directory to store files for download

  # Observe Process Button
  observeEvent(input$process, {
    # Hide download button at the start of processing
    shinyjs::hide("download")

    # Check if files are uploaded; show a notification if not
    if (is.null(input$file_phi) && is.null(input$file_si)) {
      showNotification("No data input. Please upload at least one file to proceed.", type = "error")
      return(NULL)  # Stop further execution if no files are uploaded
    }

    # Use the external function to process data
    result <- tryCatch({
      extractSAMMresults(input$file_phi$datapath, input$file_si$datapath)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })

    if (!is.null(result)) {
      final_output_long(result$final_output_long)  # Store the long data for further use
      final_output(result$final_output)  # Store the output data for download

      # Save processed data as CSV in the temporary directory
      cleaned_file_path <- file.path(output_dir_path, "cleaned_SAMM_results.csv")
      rio::export(result$final_output, cleaned_file_path)
      shinyjs::show("download")  # Show download button after processing
    }
  })

  # Render DataTable in window and show message if no data present
  output$cleaned_data <- renderDT({
    data <- final_output()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available. Please upload valid files and click 'Process Data'."),
                options = list(dom = 't')))  # Hide table controls
    } else {
      return(datatable(data,options = list(dom = 'lfrtip'))) # all table controls
    }
  })


  # Render statistics (basic summary of the cleaned data)
  output$stats <- renderPrint({
    req(final_output())
    summary(final_output())
  })


  # Render statistics with errors, warnings and default 0 values count
  output$error_summary <- renderUI({
    req(final_output())
    total_length<-nrow(final_output())
    phi_has_parameter_count= sum(final_output()$phi_has_parameter,na.rm = T)
    si_has_parameter_count= sum(final_output()$si_has_parameter,na.rm = T)
    phi_is_skipped_count= sum(final_output()$phi_is_skipped,na.rm = T)
    si_is_skipped_count= sum(final_output()$si_is_skipped,na.rm = T)
    gezi_zero_count <- sum(final_output()$SI2_value==0 | final_output()$SI2_value<final_output()$SI2_GE,na.rm = T)
    phid_zero_count <- sum(final_output()$phid_value==0,na.rm = T)
    phis_zero_count <- sum(final_output()$phis_value==0,na.rm = T)
    phib_zero_count <- sum(final_output()$phib_value==0,na.rm = T)
    phitotOB_zero_count <- sum(final_output()$phitotOB_value==0,na.rm = T)


    div(HTML(paste(
      "<p style='color:red; font-size:18px; font-weight:bold;'>Error Messages and Modelling problems:</p>",
      "<p style='font-size:14px;'>The C-peptide and Glucose models (i.e., The <b>minimal model</b>: the C-peptide model generates Phi parameters, the Glucose model generates SI parameters) may not be able to derive parameters from the model fit for some observations or fit the model entirely. The variables <i>si_has_parameter</i> and <i>phi_has_parameter</i> denote where parameters could not be generated after trying to fit the model, with the additional <i>si_is_skipped</i> and <i>phi_is_skipped</i> to denote where the model did not proceed with any fit (i.e., observation was skipped). The warning/error messages are stored in the respective <i>messages</i> variable.</p>",
      "<p style='font-size:14px; font-weight:bold;'>The total number of observations in this run is:", total_length, ".</p>",
      "<br>",

      "<p style='color:red;font-weight:bold;font-size:16px;'>Types of messages</p>",
      "<p style='color:black; font-size:14px;'> The messages that occur that prevent the minimal model from fitting are related to convergence issues, including:</p>",
      "<ul>",
      "<li>The optimizer was unable to find any combination of parameter values that would satisfy the convergence criteria.</li>",
      "<li>The maximum number of iterations was reached without convergence.</li>",
      "</ul>",
      "<p style='color:black; font-size:14px;'> Usually these messages will contain the word: <b>Error</b> <i>regardless of any other words</i>.</p>",
      "<br>",

      "<p style='color:black; font-size:14px;'> The messages that occur that allow the minimal model to fit but prevent the minimal model from generating parameters are related to modelling parameter limits requiring optimization, including:</p>",
      "<ul>",
      "<li>The following parameter limit(s) constrain further optimization (the modelling parameter(s) are then displayed after this message).</li>",
      "<li>The covariance matrix is unreliable and may indicate multiple solutions.</li>",
      "</ul>",
      "<p style='color:black; font-size:14px;'> Usually these messages will contain the word: <b>Warning</b> <i>without the word Error</i>.</p>",

      "<p style='color:red;font-weight:bold;font-size:16px;'>When a parameter is given a zero value</p>",
      "<p style='color:black; font-size:14px;'>Note that when the warning surrounding modelling parameter limit(s) contains: 'hit lower limit', the model defaults the a value of zero. This means that Phi (s,d,b or totalOB) and SI parameters will have a value of 0 by default.</p>",
      "<p style='color:red; font-size:14px; font-weight:bold;'>Number of observations with defaulted 0 value for Phi parameters:</p>",
      "<ul>",
      "<li> Phi Dynamic (Phid):", phid_zero_count, "</li>",
      "<li> Phi Static (Phis):", phis_zero_count, "</li>",
      "<li> Phi Basal (Phib):", phib_zero_count, "</li>",
      "<li> Phi Total (Phitotal):", phitotOB_zero_count, "</li>",
      "</ul>",
      "<br>",

      "<p style='color:red; font-size:14px; font-weight:bold;'>Number of observations with defaulted 0 value for SI parameter:</p>",
      "<ul>",
      "<li> Insulin Sensitivity (SI2):", gezi_zero_count, "</li>",
      "</ul>",
      "<p style='color:black; font-size:14px;'>For SI there is a minimum value that is set in the modelling parameters. The minimum value the model has used is denoted by variable <i>SI2_GE</i>.</p>",
      "<br>",

      "<p style='color:red;font-weight:bold;font-size:16px;'>Modelling errors where parameters cannot be generated</p>",
      "<p style='color:red; font-size:14px;'>Number of observations with Phi not generated:</p>",
      "<p style='color:black; font-size:14px;'>", total_length - phi_has_parameter_count, "</p>",
      "<p style='color:black; font-size:14px;'> Out of these, the number that the C-peptide model did not try and fit entirely was: ", phi_is_skipped_count, "</p>",

      "<br>",
      "<p style='color:red; font-size:14px;'>Number of observations with SI not generated:</p>",
      "<p style='color:black; font-size:14px;'>", total_length - si_has_parameter_count, "</p>",
      "<p style='color:black; font-size:14px;'> Out of these, the number that the Glucose model did not try and fit entirely was: ", si_is_skipped_count, "</p>",
      "<br>",

      "<p style='color:red;font-weight:bold; font-size:16px;'>Understanding the Coefficient of Variation in SAMM parameters:</p>",
      "<p>The coefficient of variation (CV) for each parameter is a normalized measure of uncertainty or variability in the estimated parameter, relative to the estimate itself. The CV expresses the relative uncertainty of that parameter estimate for a single model fit, indicating how much the estimate might vary if the model were fitted repeatedly with similar data.</p>",
      "<p>For a given parameter estimate (e.g., Phi total, Phi static), the CV reflects how precisely that parameter was derived in the context of the model fitting process for that individual. A high CV suggests that the parameter estimate for this individual might vary significantly if the model were re-fit under similar conditions, indicating low confidence in this estimate. This could mean that the parameter is less stable or able to be identified given the current model structure and data, with fluctuations due to data noise or modelling constraints.</p>",
      "<p>In contrast, a low CV for a parameter indicates that its estimate is relatively stable and that the model provides a consistent, reliable value for that parameter in this individual.</p>",
      "<p style='color:red; font-size:16px;'>Guidelines for interpreting the CV:</p>",
      "<p style='color:black; font-size:14px; font-weight:bold;'><b>When the value of a parameter is 0 (by default or not) the CV cannot be calculated</b></p>",
      "<p>A threshold for acceptable CV can vary depending on the setting. In biological/medical modeling, there is no strict rule for CV thresholds. However, general guidelines often suggest the following:</p>",
      "<ul>",
      "<li><b>Parameter reproducibility is excellent</b>: CV ≤ 10%</li>",
      "<li><b>Parameter reproducibility is good</b>: CV between 10–20%</li>",
      "<li><b>Parameter reproducibility is acceptable</b>: CV between 20–30%</li>",
      "<li><b>Parameter reproducibility is poor</b>: CV > 30%</li>",
      "</ul>",
      "<p>While not a strict threshold, it is recommended that parameters with a CV >30% be interpreted with caution.</p>",
      "<br>",
      "<p>The table below gives an overview of the distribution of each parameter itself as Median, Mean, and Range as well as the distribution of the CV for each parameter.</p>",
      "<br>"
    )))

  })

    # Reactive to calculate and pivot CV and parameter summaries
    cv_and_param_summaries_pivoted <- reactive({
      req(final_output_long())

      # Enhanced CV Summary
      cv_summary <- final_output_long() %>%
        mutate(parameter = recode(parameter,
                                  "SI2" = "Insulin Sensitivity",
                                  "phib" = "Phi Basal",
                                  "phid" = "Phi Dynamic",
                                  "phis" = "Phi Static",
                                  "phitotOB" = "Phi Total")) %>%
        group_by(parameter) %>%
        summarize(
          `CV (Median, IQR)` = paste0(
            round(median(cv, na.rm = TRUE), 2), " (",
            round(quantile(cv, 0.25, na.rm = TRUE), 2), ", ",
            round(quantile(cv, 0.75, na.rm = TRUE), 2), ")"
          ),
          `CV (Mean, SD)` = paste0(
            round(mean(cv, na.rm = TRUE), 2), " (",
            round(sd(cv, na.rm = TRUE), 2), ")"
          ),
          `CV Count ≤ 10%` = paste0(
            sum(cv <= 10, na.rm = TRUE), " (",
            round(sum(cv <= 10, na.rm = TRUE) / n() * 100, 1), "%)"
          ),
          `CV Count 10-20%` = paste0(
            sum(cv > 10 & cv <= 20, na.rm = TRUE), " (",
            round(sum(cv > 10 & cv <= 20, na.rm = TRUE) / n() * 100, 1), "%)"
          ),
          `CV Count 20-30%` = paste0(
            sum(cv > 20 & cv <= 30, na.rm = TRUE), " (",
            round(sum(cv > 20 & cv <= 30, na.rm = TRUE) / n() * 100, 1), "%)"
          ),
          `CV Count > 30%` = paste0(
            sum(cv > 30, na.rm = TRUE), " (",
            round(sum(cv > 30, na.rm = TRUE) / n() * 100, 1), "%)"
          ),
          `CV Count > 50%` = paste0(
            sum(cv > 50, na.rm = TRUE), " (",
            round(sum(cv > 50, na.rm = TRUE) / n() * 100, 1), "%)"
          ),
          `Median (IQR)` = paste0(
            round(median(value, na.rm = TRUE), 2), " (",
            round(quantile(value, 0.25, na.rm = TRUE), 2), ", ",
            round(quantile(value, 0.75, na.rm = TRUE), 2), ")"
          ),
          `Mean (SD)` = paste0(
            round(mean(value, na.rm = TRUE), 2), " (",
            round(sd(value, na.rm = TRUE), 2), ")"
          ),
          `Range` = paste0(
            round(min(value, na.rm = TRUE), 2), " - ",
            round(max(value, na.rm = TRUE), 2)
          )
        )

      # Pivot data to have parameters as columns
      cv_summary %>%
        pivot_longer(cols = -parameter, names_to = "Statistic", values_to = "Value") %>%
        pivot_wider(names_from = parameter, values_from = Value)
    })

    # Render the pivoted summary table in the UI
    output$cv_param_summary_table <- renderDataTable({
      cv_and_param_summaries_pivoted()
    }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)


  # Render histograms for each parameter
  output$viz <- renderPlot({
    req(final_output_long())
    ggplot2::ggplot(final_output_long(), aes(x = value)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      facet_wrap(~ parameter, scales = "free_x") +
      labs(title = "Distribution of Values for Each Parameter (raw values)", x = "Value", y = "Frequency") +
      theme_minimal()

  })

  # Download Handler to allow user to download the final data as a ZIP
  output$download <- downloadHandler(
    filename = function() {
      paste0("cleaned_SAMM_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create a zip file of the final data file
      files <- list.files(output_dir_path, full.names = TRUE, pattern = "cleaned_SAMM_results.csv")
      zip::zipr(zipfile = file, files = files, recurse = FALSE)
    },
    contentType = "application/zip"
  )
}

# Run the application
shinyApp(ui = ui, server = server)


#rsconnect::deployApp("app folder extract/", appName = "SAMMII_Results_Extractor_Tool")
