#' Find The Newest File.
#'
#' Look for a file in the working directory or a subfolder and return a character string with the newest version of the file matching the queried character string.
#'
#' @param look_for Character string with the name, or part of the name, of the file to look for
#'
#' @param folder Specify the subfolder to look for the file for
#'
#' @export

# newest file of given name
newest_file <- function(look_for = NA, folder = NA) {

  if (is.na(look_for)) {
    stop("Please specifiy a character string to look_for")
  }

  # check whether subfolder is given and define folder to scan through for files
  if (is.na(folder)) {
    designation <- here::here()
  } else {

    # remove any / in case I forgot that I don't need them
    folder <- gsub(x = folder, pattern = "/", replacement = "")
    designation <- paste0(here::here(), "/", folder, "/")
  }

  # list files matching look_for
  files <- list.files(designation)

  # check if there are any files in the designated folder
  if(identical(files, character(0))) stop("No files found!")

  # search for files matching query
  files.wanted <- files[grep(look_for, files)]

  # get information about last modification of files
  files.detailed <- file.info(files.wanted)

  # sort by modification date
  files.ordered <- files.detailed[with(files.detailed, order(as.POSIXct(mtime))), ]

  # get the newest file from the list
  files.newest <- rownames(utils::tail(files.detailed, n = 1))

  # return the newest file
  return(files.newest)
}
