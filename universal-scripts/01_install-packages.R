# These are the packages I use most frequently for my work and load by default.
# Before finishing a project, remove any unused packages from the list so that
# only needed packages are installed.

packages <- c("conflicted",
              "emmeans",
              "haven",
              "janitor",
              "kableExtra",
              "knitr",
              "labelled",
              "lme4",
              "lmerTest",
              "lmtest",
              "lubridate",
              "psych",
              "purrr",
              "RColorBrewer",
              "rmdformats",
              "stargazer",
              "tidytext",
              "tidyverse",
              "tinytex",
              "viridis")

# Github packages =====
# Correlation matrices calculated using fancyCorr package available at
# https://github.com/mcbeem/fancyCorr

# To install, run this code: 
# remotes::install_github("sbw78/fancyCorr")

# Reports =====
# remotes::install_github("easystats/report")

# List github packages here. `package` contains the package name as it is called
# in `library()` or `require()`, `repo` contains the repository address in the
# format username/repo

github_packages <- list(package = c("fancyCorr",       "reports"),
                        repo    = c("sbw78/fancyCorr", "easystats/report"))

# This function will load github and CRAN packages
invisible(install_load_packages())

# Set preferred packages for function conflicts
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("map", "purrr")
conflicted::conflict_prefer("annotate", "ggplot2")