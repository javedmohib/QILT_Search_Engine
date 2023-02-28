library(openxlsx)
library(fuzzyjoin)
library(shiny)
library(stringr)
library(stringdist)
library(magrittr)
library(dplyr)
library(shinythemes)
library(bslib)

app1 <- shinyApp(
  ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                  titlePanel(h3("Variables Search Engine")),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("survey", "Survey", choices = c("QILT", "Life in Australia"), selected = "GOS"),
                      selectInput("project", "Project:", choices = c("GOS", "SES", "GOS-L", "ESS"), selected = "GOS"),
                      selectInput("spec", "Specification:", choices = c("POP", "OP", "MOS", "Other", "ALL"), selected = "POP"),
                      textInput("search_variable", "Search Variable:", value = "")
                    ),
                    mainPanel(
                      tableOutput("result_table")
                    )
                  )
  ),

  server <- function(input, output) {

    observe({
      # Require that all inputs are present
      req(input$search_variable, input$project, input$spec)

      # Find the file name that corresponds to the user's inputs
      file_name <- list.files(file.path("lookup", input$project, input$spec))
      print(file_name)

      # If no file is found, return an error message
      if (length(file_name) == 0) {
        return(message("Error: file not found."))
      } else {
        # Get the full path to the file
        file_path <- file.path("lookup", input$project, input$spec, file_name)
        print(file_path)

        # Read the variable data from the first sheet of the file
        variable_df <- openxlsx::read.xlsx(file_path, 1) %>%
          dplyr::rename_all(tolower) %>%
          select(any_of(c("variable", "label"))) %>%
          mutate(variable = tolower(variable))
        dim(variable_df)

        # Read the value data from the second sheet of the file
        if(!input$spec %in% "OP"){
          value_df <- read.xlsx(file_path, 2) %>%
            dplyr::rename_all(tolower) %>%
            select(any_of(c("variable", "value", "label"))) %>%
            mutate(variable = tolower(variable))
          dim(value_df)
        } else{
          value_df <- NULL
        }
      }

      # Create a reactive function that filters the result data based on the user's inputs
      result_filtered <- reactive({
        # Convert the search variable to lowercase
        search_variable <- tolower(input$search_variable)
        result_variable <- variable_df[variable_df$variable %in% search_variable, ]

        # If a matching variable is found, find any corresponding value data
        if (nrow(result_variable) > 0) {
          # Combine the matching results from both data frames into a list
          result_value <- value_df[value_df$variable %in% search_variable, ]
          if (nrow(result_value) > 0) {
            result_filtered <- list(
              Variable = as.list(result_variable),
              Value = result_value)
            # result_filtered <- bind_rows(result_variable, result_value)
          } else {
            # If no match is found in the values data frame, return the results from the variables data frame
            result_filtered <- result_variable %>% rename("Variable" = "variable", "Label" = "label")
          }
        } else {
          # If no match is found in the variables data frame
          # Calculate the string distance between the search input and each variable in the variables data frame
          variable_df$distance <- stringdist(variable_df$variable, search_variable, method = "jw", p = 0.1)
          # retrieves the variable with the minimum string distance
          nearest_match_variable <- variable_df$variable[which (variable_df$distance == min(variable_df$distance))]
          result_value <- value_df[value_df$variable %in% nearest_match_variable, ]

          if (nrow(result_value) > 0) {
            result_variable <- variable_df[variable_df$variable %in% nearest_match_variable, ]
            result_filtered <- bind_rows(result_variable, result_value)

          } else {
            result_variable <- variable_df[variable_df$variable %in% nearest_match_variable, ]
            result_filtered <- result_variable

          }

        }
      })

      output$result_table <- renderTable({

        result_filtered()
      })
    })
  }
)


app2 <- shinyApp(
  ui <- fluidPage(

    theme = bs_theme(version = 4, bootswatch = "minty"),

    titlePanel(h3("Files Search Engine")),

    sidebarLayout(
      sidebarPanel(
        selectInput("project", "Select project:",
                    choices = c("GOS", "SES", "ESS", "GOS-L")),
        textInput("year", "Enter collection year:", value = "2023"),
        textInput("subfolder_path", "Enter sub-folder name of file path (separated by commas):", value = "feb"),
        textInput("file_name", "Enter one or more keywords from file name (separated by commas):", value = "spec"),
        selectInput("file_extension", "Select file extension:",
                    c("xlsx", "csv", "txt", "pdf", "docx"), selected = "xlsx"),
        selectInput("drive", "Drive to search for", choices = c("Both", "Z", "K")),
        checkboxInput("match_all_name", "Match all keywords in file name:", value = TRUE),
        checkboxInput("match_all_path", "Match all subfolder paths:", value = TRUE),
        actionButton("submit", "Submit")
      ),

      mainPanel(
        verbatimTextOutput("matching_files")
      )
    )
  ),


  server <- function(input, output) {
    observeEvent(input$submit, {
      subfolder_path <- unlist(strsplit(input$subfolder_path, ",\\s*"))
      file_name <- unlist(strsplit(input$file_name, ",\\s*"))
      drive = unlist(strsplit(input$drive, ",\\s*"))
      matching_files <- find_files(project = input$project,
                                   year = input$year,
                                   subfolder_path = subfolder_path,
                                   file_name = file_name,
                                   file_extension = input$file_extension,
                                   drive = drive,
                                   match_all_name = input$match_all_name,
                                   match_all_path = input$match_all_path)

      output$matching_files <- renderText({
        if(length(matching_files) == 0){
          "No such file found"
        } else{
          paste(matching_files, collapse = "\n")
        }

      })
    })

  }
)

# Combine the two apps into one app with two tabs
ui <- shinyUI(
  navbarPage("", #h2("QILT Search Engine"),
             # position = c("fixed-top"),
             tabPanel(h3("Varables"), app1),
             tabPanel(h3("Files"), app2))
)

server <- function(input, output) {}

# Run the app
shinyApp(ui = ui, server = server)


