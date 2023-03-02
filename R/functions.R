#' To remove duplicate columns
#'
#' @param dfA datframe
#'
#' @return A dataframe
#' @export
#'
#' @examples
remove_duplicate_cols <- function(df) {
  cols_to_keep <- rep(TRUE, ncol(df))
  for (i in 1:(ncol(df) - 1)) {
    for (j in (i+1):ncol(df)) {
      if (all(df[, i] == df[, j])) {
        cols_to_keep[j] <- FALSE
      }
    }
  }
  df[, cols_to_keep, drop = FALSE]
}

#' To find a file in QILT project folders
#' @param project Specify the QILT project (GOS, SES, ESS, GOS-L)
#' @param year Specify the collection year
#' @param subfolder_path Sub-folder name of the file path in any order
#' @param file_extension File extension for example xlsx or txt
#' @param file_name One or more keywords from the name of the file
#' @param match_all_name TRUE if all the values in file_name must be there in the output file.
#' @param match_all_path If TRUE only gives results when all values in subfolder_path match
# find_files <- function(project, year, drive = "", subfolder_path = "", file_name = "", file_extension = "", match_all_name = TRUE, match_all_path = FALSE){
#   if(tolower(project) == "gosl") project <- "GOS-L"
#   if(tolower(drive) == "k"){
#     folder_paths <- glue::glue("K:/QILT/{project}/{year}")
#   } else if(tolower(drive) == "z"){
#     folder_paths <- glue::glue("z:/Consulting/Jobs/QILT/{project}/{year}")
#   } else {
#     folder_paths <- c(
#       glue::glue("K:/QILT/{project}/{year}"),
#       glue::glue("z:/Consulting/Jobs/QILT/{project}/{year}")
#     )
#   }
#
#   matching_files <- data.frame(file_path = character(),
#                                sheet_name = character(),
#                                stringsAsFactors = FALSE)
#
#   for (folder_path in folder_paths) {
#     all_files <- dir(folder_path, recursive = TRUE)
#     # remove the files that start with "., ~, $"  (i.e. hidden files) and exclude zip files
#     all_files <- all_files[!grepl("^[\\.\\$~]", all_files) &
#                              !grepl("\\.zip$", all_files, ignore.case = TRUE) &
#                              grepl(paste0(".", file_extension), all_files, ignore.case = TRUE)] # select only those with desired extension
#
#     for (file in all_files){
#       if (((match_all_name && all(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))) || # Runs when match_all_name = TRUE
#            (!match_all_name && any(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))))){ # Runs when match_all_name = FALSE
#         if (all(subfolder_path == "")){
#           sheets <- tryCatch({
#             sheets <- getSheetNames(file.path(folder_path, file))
#             sheets
#           }, error = function(e) {
#             character(0)
#           })
#           if (length(sheets) > 0) {
#             matching_files <- ifelse(nrow(matching_files) == 0,
#                                      data.frame(file_path = file.path(folder_path, file),
#                                                 sheet_name = sheets,
#                                                 stringsAsFactors = FALSE),
#                                      rbind(matching_files,
#                                            data.frame(file_path = file.path(folder_path, file),
#                                                       sheet_name = sheets,
#                                                       stringsAsFactors = FALSE)))
#           }
#
#         } else {
#           matching_subfolder_path <- sub("/[^/]+$", "", file.path(folder_path, file))
#           if ((!match_all_path && any(sapply(subfolder_path, function(arg) grepl(arg, matching_subfolder_path, ignore.case = TRUE)))) ||
#               (match_all_path && all(sapply(subfolder_path, function(arg) grepl(arg, matching_subfolder_path, ignore.case = TRUE))))){
#             sheets <- tryCatch({
#               sheets <- openxlsx::getSheetNames(file.path(folder_path, file))
#               sheets
#             }, error = function(e) {
#               character(0)
#             })
#             if (length(sheets) > 0
#             ){
#               matching_files <- rbind(matching_files,
#                                       data.frame(file_path = file.path(folder_path, file),
#                                                  sheet_name = sheets,
#                                                  stringsAsFactors = FALSE))
#             }
#           }
#         }
#       }
#     }
#   }
#
#   return(unique(matching_files$file_path))
# }

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

