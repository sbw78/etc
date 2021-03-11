# Heavily borrowed from Kevin Ushey (github.com/kevinushey/etc)

# READY FOR USE

invisible(
  local(
    {
      # Set name and email safely ----
      FIRSTNAME <- as.integer(c(83, 46, 32, 66, 114, 121, 97, 110))
      LASTNAME <- as.integer(c(87, 101, 115, 116))
      NAME <- as.integer(c(83, 46, 32, 66, 114, 121, 97, 110, 32, 87, 101, 115, 116))
      EMAIL <- as.integer(c(115, 98, 119, 55, 56, 64, 99, 111, 114, 110, 101,
                            108, 108, 46, 101, 100, 117))
      
      # Set devtools options ----
      options(
        "devtools.desc" = list(
          Author = intToUtf8(NAME),
          Maintainer = paste0(intToUtf8(NAME), " <", intToUtf8(EMAIL), ">"),
          License = "MIT + file LICENSE",
          Version = "0.0.0.9000"
        )
      )
      options("devtools.name" = intToUtf8(NAME))
      
      # Set usethis github options ----
      options(
        usethis.full_name = intToUtf8(NAME),
        usethis.protocol = "ssh",
        usethis.description = list(
          "Authors@R" = utils::person(
            intToUtf8(FIRSTNAME), intToUtf8(LASTNAME),
            email = intToUtf8(EMAIL),
            role = c("aut", "cre", "dtc"),
            comment = c(ORCID = "0000-0003-3327-0499")
          )
        )
      )
      
      # set TZ if unset ----
      if (is.na(Sys.getenv("TZ", unset = NA))) {
        Sys.setenv(TZ = "America/New_York")
      }
      # only run in interactive mode ----
      if (!interactive()) {
        return()
      }
        
      # create .Rprofile env ----
      .__Rprofile.env__. <- attach(NULL, name = "local:rprofile")
      
      # helpers for setting things in .__Rprofile.env__. ----
      # set <- function(name, value)
      #   assign(name, value, envir = .__Rprofile.env__.)
      
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
      addTaskCallback(
        function(...) {
          count <- sum(search() == "local:rprofile")
          if (count == 0)
            return(FALSE)
          
          for (i in seq_len(count - 1))
            detach("local:rprofile")
          
          return(FALSE)
        }
      )
      
      # display startup message(s) ----
      version <- "0.0.2"
      version <- paste("Using .RProfile version ", version, "\n", sep = "")
      
      msg <- if (length(.libPaths()) > 1) {
        "\nUsing libraries at paths:\n"
      } else {
        "\nUsing library at path:\n"
      }
      
      libs <- paste("-", .libPaths(), collapse = "\n")
      
      message(version, msg, libs, sep = "")
    }
  )
)

