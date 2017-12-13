# newest rprofile
newest_rprofile <-
  function() {
    designation <- "/mnt/ifs1_projekte/genstat/07_programme/rtools/RProfile_hk/"
    files <- list.files(designation)
    RProfiles <- files[grep("Rprofile_hk_", files)]
    newest_RProfile <- tail(sort(RProfiles), n = 1)
    suppressPackageStartupMessages(source(paste0(designation, newest_RProfile)))
  }
