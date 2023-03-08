# QILT Search Engine

The QILT Search Engine is an R Shiny app that allows users to search for files in their local system and displays the content of the file. It also allows the user to copy the file path, oen the file or open the directory where the file is stored. The app is designed to be user-friendly and customizable, allowing users to search for files based on a range of parameters.

### **Features**

-   Select a QILT project to search (GOS, SES, ESS, or GOS-L) - For now only allows to search for a QILT project

-   Enter a collection year 

-   Enter keywords to search for in the file name and subfolder path

-   Choose a file extension (xlsx, csv, txt, pdf, or docx)

-   Select a drive to search (K, Z, or Both)

-   Choose whether to match all keywords in the file name and/or subfolder path

-   View a list of matching files and select one to display

-   Select a specific tab from the file to display

-   View the content of the selected tab in a data table

### **Dependencies**

The app requires the following R packages to be installed:

-   openxlsx

-   shiny

-   stringr

-   magrittr

-   dplyr

-   bslib

-   shinydashboard

-   shinyjs

-   officer

-   pdftools

### **License**

This app is licensed under the MIT License.
