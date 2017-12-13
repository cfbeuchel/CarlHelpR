save_csv_carl <- function(file = NA, file_name = NA, subfolder = NA, create_subfolder = F) {

  # check if data.table is
  if(!is.na(all(file)) | data.table::is.data.table(file) == F){

    # try to coerce file into data.table
    try(data.table::setDT(file), silent = T)

    # if file is still not a data.table, stop
    if (!data.table::is.data.table(file)) {
      stop("Please specifiy a file that can be converted into a data.table to save as file!")
    }
  }

  # check whether file name was given
  if (is.na(file_name) | !is.character(file_name)) {
    stop("Please specifiy a character string with a name for the file_name!")
  }

  # check if subfolder is a character string
  if (!is.character(subfolder)) {
    stop("Please specifiy a character string with a name of an existing subfolder to save the file in!")
  }

  # check whether subfolder is given
  if (is.na(subfolder)) {
    save_designation <- here::here()
  } else {

    # remove any / in case I forgot that I don't need them
    subfolder <- gsub(x = subfolder, pattern = "/", replacement = "")
    save_designation <- paste0(here::here(), "/", subfolder, "/")
  }

  # check if given directory exists
  if (!dir.exists(save_designation) & create_subfolder == F) {
    stop("Directory or subfolder does not exist. Set create_subfolder == T to create a subdirectory of your working directory")

    # create subdirectory when create_subfolder == T
  } else if (!dir.exists(save_designation) & create_subfolder == T) {
    dir.create(save_designation)
  }

  # save file as .csv
  write.table(x = file,
              file = paste0(save_designation,
                            format(Sys.time(), '%y%m%d'),
                            "_", file_name, ".csv"),
              sep = "\t", row.names = F)
}