library(openxlsx)
library(fuzzyjoin)
library(shiny)
library(stringr)
library(stringdist)
library(magrittr)
library(dplyr)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("SRC Variable Search Engine"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("survey", "Survey", choices = c("QILT", "Life in Australia"), selected = "GOS"),
                    selectInput("project", "Project:", choices = c("GOS", "SES", "GOS-L", "ESS"), selected = "GOS"),
                    selectInput("spec", "Specification:", choices = c("POP", "OP", "MOS", "Other", "ALL"), selected = "POP"),
                    textInput("search_variable", "Search Variable:", value = "GOSID")
                  ),
                  mainPanel(
                    tableOutput("result_table")
                  )
                )
)

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
      value_df <- read.xlsx(file_path, 2) %>%
        dplyr::rename_all(tolower) %>%
        select(any_of(c("variable", "value", "label"))) %>%
        mutate(variable = tolower(variable))
      dim(value_df)
    }

    # Create a reactive function that filters the result data based on the user's inputs
    result_filtered <- reactive({
      # Convert the search variable to lowercase
      search_variable <- tolower(input$search_variable)
      result_var <- variable_df[variable_df$variable %in% search_variable, ]

      # If a matching variable is found, find any corresponding value data
      if (nrow(result_var) > 0) {
        # Combine the matching results from both data frames into a list
        result_value <- value_df[value_df$variable %in% search_variable, ]
        if (nrow(result_value) > 0) {
          result_filtered <- list(
            Variable = as.list(result_var),
            Value = result_value)
        } else {
          # If no match is found in the values data frame, return the results from the variables data frame
          result_filtered <- result_var
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

shinyApp(ui, server)

