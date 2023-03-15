# QILT Search Engine

The QILT Search Engine is an R Shiny app that allows users to search for files in their local system and displays the content of the file. It also allows the user to copy the file path, open the file or open the directory where the file is stored. The app also allows user to open R scripts either in Tinn-R editor or directly .Rproj from working directory. The app is designed to be user-friendly and customizable, allowing users to search for files based on a range of parameters.

### **Features**

-   Select a QILT project to search (GOS, SES, ESS, or GOS-L) - For now only allows to search for a QILT project

-   Enter a collection year 

-   Enter keywords to search for in the file name and subfolder path - The app splits words for blank, _ or -. So if file name is sample_spec, feed two words separated by space like sample, spec

-   Choose a file extension (xlsx, csv, txt, pdf, docx, R or All of Above)

-   Select a drive to search (K, Z, or Both)

-   Choose whether to match all keywords in the file name and/or subfolder path - If TRUE only gives the files that have all the strings given in file name and/or file folder

-   Open R script in Tinn-R editor - Select the check box if you want to open R files in Tinn-R editor

-   Submit - Press the button to View a list of matching files 

-   Select a specific file from the drop down list to display


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
