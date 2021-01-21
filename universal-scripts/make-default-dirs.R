if (!"here" %in% rownames(utils::installed.packages())) {
  message("Package `here` is not installed. Installing...")
  invisible(install.packages("here"))
} 

start_project <- function(variables) {
  require(here)
  if ('here' %in% installed.packages()) {
    message("Build directories")
  } else{
    invisible(install.packages('here'))
  }
  
  subdirs <-
    c("analysis_data",
      "command_files",
      "command_files/functions",
      "documents",
      "original_data")
  sourcedir <- "~/Rmain/etc/universal_scripts/"
  files <- c("01_install-packages.R", "02_custom-functions.R", "03_load-data.R")
  destination <- "command_files"
  
  for (i in seq_along(subdirs)) {
    if (dir.exists(here::here(subdirs[i]))) {
      message(paste(subdirs[i], "already exists."))
      Sys.sleep(0.1)
    } else {
      dir.create(here::here(subdirs[i]), showWarnings = FALSE)
      Sys.sleep(0.1)
    }
  }
  message("Copy standard files over")
  file.copy(from = paste0(sourcedir, files), 
            to = here::here(destination, files))
  
  file.edit(here::here(destination, files))
  
  message("Finished")
}

start_project()
rm(list = ls())
