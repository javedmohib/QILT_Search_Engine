library(openxlsx)
library(shiny)
library(stringr)
library(magrittr)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(officer)
library(pdftools)


header <- dashboardHeader(title = "QILT Search Engine")

sidebar <- dashboardSidebar(
  selectInput("project", "QILT Project",
              choices = c("GOS", "SES", "ESS", "GOS-L")),
  textInput("year", "Collection Year", value = "2023"),
  textInput("subfolder_path", "Subfolder name(separated by commas)", value = "FEB"),
  textInput("file_name", "Keywords in File Name (separated by commas)", value = "Operational"),
  selectInput("file_extension", "File extension",
              c("xlsx", "csv", "txt", "pdf", "docx", "R", "All of above"), selected = "xlsx"),
  selectInput("drive", "Drive (K/Z)", choices = c("K", "Z", "Both")),
  checkboxInput("match_all_name", "Match all keywords in file name", value = TRUE),
  checkboxInput("match_all_path", "Match all subfolder paths", value = FALSE),
  checkboxInput("open_tinnR", "Open R script in Tinn-R editor", value = FALSE),
  actionButton("submit", "Submit")
)


body <- dashboardBody(
  shinyjs::useShinyjs(),

  fluidRow(
    column(12,
           div(style = "display: flex; justify-content: space-between;",
               div(style = "flex: 2.5;",
                   selectInput(inputId = "file_path", label = "Select a file:", choices = NULL, width = "100%")
               ),
               div(style = "flex: 1;",
                   shinyjs::hidden(selectInput(inputId = "sheet_name", label = "Select a sheet:", choices = NULL, width = "100%"))
               ),
               div(style = "display: flex: 1; justify-content: flex-end;",
                   actionButton(inputId = "copy_path_btn", label = "Copy Path"),
                   actionButton(inputId = "open_file_btn", label = "Open File"),
                   actionButton(inputId = "open_folder_btn", label = "Open Folder")
               )
           )
    )
  ),

  fluidRow(
    column(12,
           tabsetPanel(type = "tabs", id = "data_table", #selected = 3, # height = "700px", width = NULL,
                       tabPanel("File Content", dataTableOutput("table_output"), value=1),
                       tabPanel("File Content", verbatimTextOutput("text_output"), value = 2),
                       tabPanel("About",  shiny::includeMarkdown("README.md"), value = 3),

           )

    )
  )

)

ui <- dashboardPage(header, sidebar, body)

server <- shinyServer(function(input, output, session) {
  useShinyjs()
  files <- reactive({
    # Retrieve the search parameters from the input fields
    project <- input$project
    year <- input$year
    drive <- input$drive
    subfolder_path <- strsplit(input$subfolder_path, ",\\s*")[[1]]
    file_name <- strsplit(input$file_name, ",\\s*")[[1]]
    file_extension <- ifelse(input$file_extension %in% "All of above", "", input$file_extension)
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

    if (length(choices) == 0) {
      output$text_output <- renderText({
        "No file found based on search inputs provided"
      })
    } else {
      updateSelectInput(session, "file_path", choices = choices)
      # Check if the selected file is an xlsx file and show/hide the sheet_name input accordingly
      if (length(choices) > 0 && tools::file_ext(choices[1]) != "xlsx") {
        shinyjs::hide("sheet_name")
      } else {
        shinyjs::show("sheet_name")
      }
    }
  })



  observeEvent(input$copy_path_btn, {

    clipr::write_clip(input$file_path)

  })

  observeEvent(input$open_file_btn, {

    open_file(input$file_path, input$open_tinnR)
  })

  observeEvent(input$open_folder_btn, {

    shell.exec(dirname(input$file_path))
  })


  observeEvent(input$file_path, {
    file_path <- input$file_path
    ext <- tools::file_ext(file_path)

    if (ext %in% c("xlsx", "xlsm")) {
      sheet_names <- getSheetNames(file_path)
      updateSelectInput(session, "sheet_name", choices = sheet_names)
      shinyjs::show("sheet_name")
      shinyjs::show("table_tab")
      shinyjs::hide("text_tab")

    } else if (ext %in% c("txt", "csv")) {
      # Display the content of the file
      output$table_output <- renderDataTable({
        read.table(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
      })
      shinyjs::hide("sheet_name")
      shinyjs::show("table_tab")
      shinyjs::hide("text_tab")

    } else if (ext == "pdf") {
      # Display the content of the file
      output$text_output <- renderText({
        pdftools::pdf_text(file_path)
      })
      shinyjs::hide("sheet_name")
      shinyjs::hide("table_tab")
      shinyjs::show("text_tab")

    } else if (ext %in% c("doc", "docx")) {
      # Display the content of the file
      output$text_output <- renderText({
        doc_text <- read_docx(file_path)
        text <- docx_summary(doc_text)$text
        return(paste("<pre>", text, "</pre>", sep = ""))
      })
      shinyjs::hide("sheet_name")
      shinyjs::hide("table_tab")
      shinyjs::show("text_tab")

    } else if (ext %in% c("R", "rmd")) {
      output$text_output <- renderPrint({
        script_content <- readLines(file_path)
        cat(paste(script_content, collapse = "\n"))
      })
      shinyjs::hide("sheet_name")
      shinyjs::hide("table_tab")
      shinyjs::show("text_tab")

    } else if (ext %in% "") {
      output$text_output <- renderText({
        print("Text is displayed here only when a .doc or .pdf file is selected")
      })
      shinyjs::hide("sheet_name")
      shinyjs::hide("table_tab")
      shinyjs::show("text_tab")

    } else {
      # Show a message indicating that the selected file is not supported
      showModal(modalDialog("The selected file is not supported. Please select a file with extension .xlsx, .xlsm, .txt, .csv, .doc, .docx or .pdf."))
      shinyjs::hide("sheet_name")
      shinyjs::hide("table_tab")
      shinyjs::hide("text_tab")
    }
  })


  observeEvent(input$sheet_name, {
    sheet_name <- input$sheet_name
    file_path <- input$file_path

    if (!is.null(sheet_name) && sheet_name != "") {
      # Display the content of the selected sheet
      output$table_output <- renderDataTable({
        read.xlsx(file_path, sheet = sheet_name, colNames = TRUE, rowNames = FALSE)
      })
    }
  })
}

)



shinyApp(ui = ui, server = server)
