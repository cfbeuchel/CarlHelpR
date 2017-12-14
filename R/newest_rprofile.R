#' Newest Rprofile by Holger Kirsten.
#'
#' This function looks for and loads the newest .RProfile file by Holger when executed in the IMISE network environment.
#'
#' The loaded RProfile contains helper functions.
#'
#' @export

# newest rprofile
newest_rprofile <-
  function() {
    designation <- "/mnt/ifs1_projekte/genstat/07_programme/rtools/RProfile_hk/"
    files <- list.files(designation)
    RProfiles <- files[grep("Rprofile_hk_", files)]
    newest_RProfile <- utils::tail(sort(RProfiles), n = 1)
    suppressPackageStartupMessages(source(paste0(designation, newest_RProfile)))
  }
