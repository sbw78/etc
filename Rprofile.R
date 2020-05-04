# Heavily borrowed from Kevin Ushey (github.com/kevinushey/etc)

invisible(local({
  
  # set TZ if unset ----
  if (is.na(Sys.getenv("TZ", unset = NA)))
    Sys.setenv(TZ = "America/New_York")

  # only run in interactive mode ----
  if (!interactive())
    return()
  
  # create .Rprofile env ----
  .__Rprofile.env__. <- attach(NULL, name = "local:rprofile")
  
  # helpers for setting things in .__Rprofile.env__. ----
  set <- function(name, value)
    assign(name, value, envir = .__Rprofile.env__.)
  
  # Functions ----
  set(".Start.time", as.numeric(Sys.time()))
  
  set("ht", function(d) rbind(head(d,10),tail(d,10)))
  
  set("hh", function(d) d[1:5,1:5])
  
  set("last", function(x){
    x[length(x)]
  })
  
  set("install_load_packages", function() {
    packages <- get("packages", envir = globalenv())
    installed_packages <- packages %in% rownames(installed.packages())
    
    if (any(installed_packages == FALSE)) {
      install.packages(packages[!installed_packages])
    } else {
      message("\n ...Packages were already installed.\n")
    }
    
    attached <- search()
    attached_packages <- attached[grepl("package", attached)]
    need_to_attach <- packages[which(!packages %in% gsub("package", "", attached_packages))]
    
    if (length(need_to_attach > 0)) {
      invisible(lapply(packages, require, character.only = TRUE))
    } else {
      message("\n ...Packages were already loaded.\n")
    }
    rm(packages)
  })
  
  # Source custom functions file ----
  sys.source("~/Rmain/functions/custom-functions.R", envir = .__Rprofile.env__.)
  
  # Set CRAN repo ----
  r <- getOption("repos")
  
  r["CRAN"] <- "http://cran.rstudio.com"
  
  options(repos = r)
  # Options ----
  options(
    # no fancy quotes (makes copy + paste a pain)
    useFancyQuotes = FALSE,
    
    # warn on partial matches
    # warnPartialMatchArgs = TRUE ## too disruptive
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    
    # warn right away
    warn = 1,
    
    # Scientific notation kicks in after 10 digits
    scipen = 10
    
    max.print = 100
  )
  
  # auto-completion of package names in `require`, `library` ----
  utils::rc.settings(ipck = TRUE)
  

  # clean up extra attached envs ----
  addTaskCallback(function(...) {
    count <- sum(search() == "local:rprofile")
    if (count == 0)
      return(FALSE)
    
    for (i in seq_len(count - 1))
      detach("local:rprofile")
    
    return(FALSE)
  })
  
  # display startup message(s)
  msg <- if (length(.libPaths()) > 1)
    "Using libraries at paths:\n"
  else
    "Using library at path:\n"
  libs <- paste("-", .libPaths(), collapse = "\n")
  message(msg, libs, sep = "")
  
}))