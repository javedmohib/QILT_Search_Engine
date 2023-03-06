library(openxlsx)
library(shiny)
library(stringr)
library(magrittr)
library(dplyr)
library(bslib)
library(shinydashboard)
library(shinyjs)
library(officer)
library(pdftools)

find_files <- function (project, year, drive = "", subfolder_path = "",
                        file_name = "", file_extension = "", match_all_name = TRUE,
                        match_all_path = FALSE)
{
  if (tolower(project) == "gosl")
    project <- "GOS-L"
  if (tolower(drive) == "k") {
    folder_paths <- glue::glue("K:/QILT/{project}/{year}")
  }
  else if (tolower(drive) == "z") {
    folder_paths <- glue::glue("z:/Consulting/Jobs/QILT/{project}/{year}")
  }
  else {
    1
    folder_paths <- c(glue::glue("K:/QILT/{project}/{year}"),
                      glue::glue("z:/Consulting/Jobs/QILT/{project}/{year}"))
  }
  matching_files <- character(0)
  for (folder_path in folder_paths) {

    all_files <- dir(folder_path, recursive = TRUE)

    all_files <- all_files[!grepl("^[\\.\\$~]", basename(all_files)) &
                             grepl(paste0(".", file_extension), all_files, ignore.case = TRUE) &
                             !grepl("zip$", all_files)]
    for (file in all_files) {
      if (((match_all_name && all(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))) ||
           (!match_all_name && any(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))))) {
        if (all(subfolder_path == "")) {
          matching_files <- c(matching_files, file.path(folder_path,
                                                        file))
        }
        else {
          matching_subfolder_path <- sub("/[^/]+$",
                                         "", file.path(folder_path, file))
          if ((!match_all_path && any(sapply(subfolder_path,
                                             function(arg) grepl(arg, matching_subfolder_path,
                                                                 ignore.case = TRUE)))) || (match_all_path &&
                                                                                            all(sapply(subfolder_path, function(arg) grepl(arg,
                                                                                                                                           matching_subfolder_path, ignore.case = TRUE))))) {
            matching_files <- c(matching_files, file.path(folder_path,
                                                          file))
          }
        }
      }
    }
  }

  return(matching_files)

}



header <- dashboardHeader(title = "QILT Projects Search Engine")

sidebar <- dashboardSidebar(
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


body <- dashboardBody(
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
                   actionButton(inputId = "open_file_btn", label = "Open File")
               )
           )
    )
  ),

  fluidRow(
    column(12,
           tabBox(id = "data_table", height = "700px", width = NULL,
                  tabPanel("Table", dataTableOutput("table_output")),
                  tabPanel("Text", verbatimTextOutput("text_output"))
           )
    )
  )
)




ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  useShinyjs()
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

  observeEvent(input$copy_path_btn, {

    clipr::write_clip(input$file_path)
  })

  observeEvent(input$open_file_btn, {

    shell.exec(input$file_path)
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
      output$table_output <- renderDataTable({
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
    } else if (ext %in% ""){

      output$text_output <- renderText({

        print("Please fill keywords and press submit to view the files")

      })

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
      output$table_output <- renderDataTable({
        read.xlsx(file_path, sheet = sheet_name, colNames = TRUE, rowNames = FALSE)
      })
    }
  })
}



shinyApp(ui = ui, server = server)
