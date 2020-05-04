install_load_packages <- function() {
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
}

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
              "RColorBrewer",
              "rmdformats",
              "stargazer",
              "tidytext",
              "tidyverse",
              "tinytex",
              "viridis")

# Install packages =====
pkgs <-
  sort(
    c("conflicted",
      "countrycode",
      "emmeans",
      "ggstatsplot",
      "haven",
      "htmltab",
      "janitor",
      "kableExtra",
      "knitr",
      "labelled",
      "lme4",
      "lmerTest",
      "lmtest",
      "lubridate",
      "maps",
      "performance",
      "plotly",
      "psych",
      "RColorBrewer",
      "rlist",
      "rmdformats",
      "rvest",
      "see",
      "stargazer",
      "stringi",
      "tidycensus",
      "tidytext",
      "tidyverse",
      "tinytex",
      "tm",
      "viridis",
      "wbstats"))

miss_pkgs <- pkgs[!pkgs %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0) {
  install.packages(miss_pkgs)
} else {
  rm(miss_pkgs)
}

# Load all the packages
invisible(lapply(pkgs, library, character.only = TRUE))

# Remove the objects that are no longer required 
rm(pkgs)

# Github packages =====
# Correlation matrices calculated using fancyCorr package available at https://github.com/mcbeem/fancyCorr
# To install, run this code: 
# remotes::install_github("https://github.com/mcbeem/fancyCorr")
library(fancyCorr)

# Reports =====
# remotes::install_github("easystats/report")
library(reports)

conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")
conflict_prefer("annotate", "ggplot2")