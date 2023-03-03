library(openxlsx)
library(fuzzyjoin)
library(shiny)
library(stringr)
library(stringdist)
library(magrittr)
library(dplyr)
library(shinythemes)
library(bslib)

ui <- fluidPage(

  theme = bs_theme(version = 4, bootswatch = "minty"),

  titlePanel(h3("QILT Projects Search Engine")),

  div(class = "row",
      div(class = "col-md-2",
          div(class = "sidebar",
              selectInput("project", "QILT Project",
                          choices = c("GOS", "SES", "ESS", "GOS-L")),
              textInput("year", "Collection Year", value = "2023"),
              textInput("subfolder_path", "Subfolder name(separated by commas)", value = "FEB"),
              textInput("file_name", "Keywords in File Name (separated by commas)", value = "Operational"),
              selectInput("file_extension", "File extension",
                          c("xlsx", "csv", "txt", "pdf", "docx"), selected = "xlsx"),
              selectInput("drive", "Drive (K/Z)", choices = c("K", "Z", "Both")),
              checkboxInput("match_all_name", "Match all keywords in file name", value = TRUE),
              checkboxInput("match_all_path", "Match all subfolder paths", value = FALSE),
              actionButton("submit", "Submit")
          )
      ),

      div(class = "col-md-10",
          mainPanel(
            # Display the list of matching files and sheets
            selectInput(inputId = "file_path", label = "Select a file:", choices = NULL),
            selectInput(inputId = "sheet_name", label = "Select a sheet:", choices = NULL),

            # Button to open the selected sheet
            # actionButton(inputId = "open_sheet", label = "Open Sheet"),
            # Render the data table output
            dataTableOutput("data_table")
          )
      )
  )
)



server <- function(input, output, session) {

  files <- reactive({
    # Retrieve the search parameters from the input fields
    project <- input$project
    year <- input$year
    drive <- input$drive
    subfolder_path <- strsplit(input$subfolder_path, ",\\s*")[[1]]
    file_name <- strsplit(input$file_name, ",\\s*")[[1]]
    file_extension <- input$file_extension
    match_all_name <- input$match_all_name
    match_all_path <- input$match_all_path

    # Find the matching files using the find_files function
    matching_files <- find_files(project, year, drive, subfolder_path, file_name, file_extension, match_all_name, match_all_path)

    # Return the matching files as a reactive value
    return(matching_files)
  })

  observeEvent(input$submit, {
    choices <- files()
    print(choices)
    updateSelectInput(session, "file_path", choices = choices)
  })


  sheet_names <- reactive({
    req(input$file_path)
    return(getSheetNames(input$file_path))
  })


  observe({
    choices <- sheet_names()
    updateSelectInput(session, "sheet_name", choices = choices)
  })

  data <- reactive({
    req(input$file_path, input$sheet_name)
    sheet_name <- input$sheet_name
    if (sheet_name != "") {
      return(read.xlsx(input$file_path, sheet = sheet_name, colNames = TRUE, rowNames = FALSE))
    }
  })

  output$data_table <- renderDataTable({
    data()
  })

  # Submit button observer
  observeEvent(input$submit, {
    # Retrieve the search parameters from the input fields
    project <- input$project
    year <- input$year
    drive <- input$drive
    subfolder_path <- strsplit(input$subfolder_path, ",\\s*")[[1]]
    file_name <- strsplit(input$file_name, ",\\s*")[[1]]
    file_extension <- input$file_extension
    match_all_name <- input$match_all_name
    match_all_path <- input$match_all_path

    # Find the matching files using the find_files function
    matching_files <- find_files(project, year, drive, subfolder_path, file_name, file_extension, match_all_name, match_all_path)

    # Update the file path select input with the matching files
    updateSelectInput(session, "file_path", choices = matching_files)
  })

}



shinyApp(ui = ui, server = server)
