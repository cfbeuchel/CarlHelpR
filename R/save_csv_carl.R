#' Custom save to .csv function
#'
#' This function saves data.tables or data.frames as .csv in the root working directory or a specified subfolder.
#' Additionally the current date is automatically included in the file name.
#'
#' @param file The data.table or data.frame to be saved.
#'
#' @param file_name Character string that specifies the name the saved file should have
#' The date of creation and the .csv ending are added automatically
#'
#' @param subfolder A character string without "/" giving the subfolder the file shall be saved in.
#'
#' @param create_subfolder Given a subfolder, setting this to TRUE will create a new directory with the name given in subfolder and will stop if set to FALSE
#'
#' @param sep the field separator string. Values within each row of x are separated by this string.
#'
#' @param quote a logical value (TRUE or FALSE) or a numeric vector. If TRUE, any character or factor columns will be surrounded by double quotes. If a numeric vector, its elements are taken as the indices of columns to quote. In both cases, row and column names are quoted if they are written. If FALSE, nothing is quoted.
#'
#' @export

save_csv_carl <- function(file = NA, file_name = NA, subfolder = NA, create_subfolder = F, sep = ",", quote = F) {

  # check if data.table is
  if(!all(is.na(file)) | data.table::is.data.table(file) == F){

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
  if (!is.character(subfolder) & !is.na(subfolder)) {
    stop("Please specifiy a character string with a name of an existing subfolder to save the file in!")
  }

  # check whether subfolder is given
  if (is.na(subfolder)) {
    save_designation <- here::here()
  } else {

    # remove any / in case I forgot that I don't need them
    subfolder <- gsub(x = subfolder, pattern = "\\|/", replacement = "")
    save_designation <- here::here(subfolder)
  }

  # check if given directory exists
  if (!dir.exists(save_designation) & create_subfolder == F) {
    stop("Directory or subfolder does not exist. Set create_subfolder == T to create a subdirectory of your working directory")

    # create subdirectory when create_subfolder == T
  } else if (!dir.exists(save_designation) & create_subfolder == T) {
    dir.create(save_designation)
  }

  # set the complete file_name
  complete_file_name <- paste0(format(Sys.time(), '%y%m%d'), "_", file_name, ".csv")

  # save in wd
  if (here::here() == save_designation) {

    # print message
    message("File will be saved as: ", here::here(complete_file_name))

    # save file as .csv
    utils::write.table(x = file,
                       file = here::here(complete_file_name),
                       sep = sep,
                       row.names = F,
                       quote = quote)

    # in case of subfolder
  } else if(here::here() != save_designation & !is.na(subfolder))

    # print message
    message("File will be saved as: ", here::here(subfolder, complete_file_name))

    # save file as .csv
    utils::write.table(x = file,
                       file = here::here(subfolder, complete_file_name),
                       sep = sep,
                       row.names = F,
                       quote = quote)
}
