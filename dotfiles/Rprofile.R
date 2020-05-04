# Heavily borrowed from Kevin Ushey (github.com/kevinushey/etc)

# TEST SCRIPT

invisible(local({
  
  NAME <- as.integer(c(66, 114, 121, 97, 110, 32, 87, 101, 115, 116))
  EMAIL <- as.integer(c(115, 98, 119, 55, 56, 64, 99, 111, 114, 110, 101,
                       108, 108, 46, 101, 100, 117))
  
  # Set devtools options
  options("devtools.desc" = list(
    Author = intToUtf8(NAME),
    Maintainer = paste0(intToUtf8(NAME), " <", intToUtf8(EMAIL), ">"),
    License = "MIT + file LICENSE",
    Version = "0.0.1"
  ))
  options("devtools.name" = intToUtf8(NAME))
  
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

  # Source custom functions file ----
  sys.source("~/Rmain/dotfiles/functions/custom-functions.R", envir = .__Rprofile.env__.)
  
  # Set CRAN repo ----
  r <- getOption("repos")
  
  r["CRAN"] <- "https://cran.rstudio.com"
  
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
    scipen = 10,
    
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
  
  # display startup message(s) ----
  version <- "0.0.1"
  version <- paste("Using .RProfile version ", version, "\n", sep = "")
  
  msg <- if (length(.libPaths()) > 1)
    "\nUsing libraries at paths:\n"
  else
    "\nUsing library at path:\n"
  
  libs <- paste("-", .libPaths(), collapse = "\n")
  workdir <- paste("Working directory: ", getwd(), "\n", sep = "")
  functions <- paste("\nCustom functions installed:", paste("-", ls(pos = "local:rprofile"), collapse = "\n"), sep = "\n")
  message(version, msg, libs, workdir, functions, sep = "")
  
}))

