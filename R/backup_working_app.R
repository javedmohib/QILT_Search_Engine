library(openxlsx)
library(shiny)
library(stringr)
library(magrittr)
library(dplyr)
library(bslib)
library(shinyjs)
library(officer)
library(docxtools)

ui <- fluidPage(
  useShinyjs(),

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
            shinyjs::hidden(selectInput(inputId = "sheet_name", label = "Select a sheet:", choices = NULL)),
            # Render the data table output
            dataTableOutput("data_table"),
            verbatimTextOutput("text_output")
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
    # Display all the matching files in the select input
    choices <- files()
    print(choices)
    updateSelectInput(session, "file_path", choices = choices)

    # Hide the "Select a sheet" input if the selected file is not an xlsx file
    if (tools::file_ext(choices[1]) != "xlsx") {
      shinyjs::hide("sheet_name")
    } else {
      shinyjs::show("sheet_name")
    }
  })

  observeEvent(input$file_path, {
    file_path <- input$file_path
    ext <- tools::file_ext(file_path)

    if (ext %in% c("xlsx", "xlsm")) {
      sheet_names <- getSheetNames(file_path)
      updateSelectInput(session, "sheet_name", choices = sheet_names)
      shinyjs::show("sheet_name")
    } else if (ext %in% c("txt", "csv")) {
      # Display the content of the file
      output$data_table <- renderDataTable({
        read.table(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
      })
      shinyjs::hide("sheet_name")
    } else if (ext == "pdf") {
      # Display the content of the file
      output$text_output <- renderText({
        pdftools::pdf_text(file_path)
      })
      shinyjs::hide("sheet_name")
    } else if (ext %in% c("doc", "docx")) {
      # Display the content of the file
      output$text_output <- renderText({

        doc_text <- read_docx(file_path)
        text <- docx_summary(doc_text)
        print(text)

      })
      shinyjs::hide("sheet_name")
    } else {
      # Show a message indicating that the selected file is not supported
      showModal(modalDialog("The selected file is not supported. Please select a file with extension .xlsx, .xlsm, .txt, .csv, .doc, .docx or .pdf."))
      shinyjs::hide("sheet_name")
    }
  })

  observeEvent(input$sheet_name, {
    sheet_name <- input$sheet_name
    file_path <- input$file_path

    if (!is.null(sheet_name) && sheet_name != "") {
      # Display the content of the selected sheet
      output$data_table <- renderDataTable({
        read.xlsx(file_path, sheet = sheet_name, colNames = TRUE, rowNames = FALSE)
      })
    }
  })
}


shinyApp(ui = ui, server = server)
