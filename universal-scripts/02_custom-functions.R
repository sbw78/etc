#   ____________________________________________________________________________
#   Custom functions                                                        ####
# 
# These are functions that I use in many of my analyses, across studies. These
# scripts should not need much modification, except for performance tweaks as I
# improve as a programmer. 
# 
# DO NOT PUT FUNCTIONS USED FOR ONLY ONE STUDY IN THIS FILE.

# Set session start time -------------------------------------------------------
.Start.time <- as.numeric(Sys.time())

# Get first 10 and last 10 rows ------------------------------------------------
ht <- function(d) {
  rbind(utils::head(d,10), utils::tail(d,10))
}

# Get first 5 each of rows, columns --------------------------------------------
hh <- function(d) {
  d[1:5,1:5]
}

# Get last element of a vector or list -----------------------------------------
last <- function(x) {
  x[length(x)]
}

# Standard error ---------------------------------------------------------------
se <- function(x, na.rm = FALSE) {
  stdev <- stats::sd(x, na.rm = na.rm)
  if (na.rm) {
    n <- length(x[!is.na(x)])
  } else {
    n <- length(x)
  }
  stdev / sqrt(n)
}

# Install packages -------------------------------------------------------------
install_load_packages <- function() {
  packages <- get("packages", envir = globalenv())
  if(exists("github_packages", envir = globalenv())) {
    github_packages <- get("github_packages", envir = globalenv())
  } else {
    github_packages <- NULL
  }
  installed_packages <- packages %in% rownames(utils::installed.packages())
  installed_github_packages <- 
    github_packages$package %in% rownames(utils::installed.packages())
  
  if (any(installed_packages == FALSE)) {
    utils::install.packages(packages[!installed_packages])
  } else {
    message("\n ...CRAN packages were already installed.\n")
  }
  
  if (any(installed_github_packages == FALSE)) {
    remotes::install_github(github_packages$repo[!installed_github_packages])
  } else {
    message("\n ...Github packages were already installed.\n")
  }
  
  attached <- search()
  attached_packages <- attached[grepl("package", attached)]
  need_to_attach <- 
    c(packages[which(!packages %in% gsub("package", "", attached_packages))])
  if (!is.null(github_packages)) {
    need_to_attach <- 
      append(need_to_attach,
             github_packages$package[which(!github_packages$package %in% gsub(
               "package", "", attached_packages))])
    packages <- c(packages, github_packages$package)
  } 
  
  if (length(need_to_attach > 0)) {
    invisible(lapply(packages, require, character.only = TRUE))
  } else {
    message("\n ...Packages were already loaded.\n")
  }
  rm(packages, github_packages, envir = globalenv())
} 

# Create regex of vector -------------------------------------------------------
regex_build <- function(list, modifier = "single") {
  if (modifier == "single") {
    custom_left <- "\\b"
    custom_right <- "\\b"
  } else if (modifier == "multi") {
    custom_left <- "\\s*?\\b"
    custom_right <- "\\b\\s*?"
  } else {
    stop("You must define modifier as either 'single' or 'multi'.")
  }
  out <- lapply(list, FUN = function(x) {paste0(custom_left, x, custom_right)})
  # out <- purrr::map(list, ~ paste0(custom_left, .x, custom_right))
  Reduce(function(x, y) {paste(x, y, sep = "|")}, out)
  # reduce(out, ~ paste(.x, .y, sep = "|"))
}

# Format p-value ---------------------------------------------------------------
format_pval <- function(p, digits = 3) {
  pvalue_chr <- as.character(round(p, digits = digits))
  if (digits == 3)
    pformat <- ifelse(p < 0.001, "< .001", pvalue_chr)
  else if (digits == 2)
    pformat <- ifelse(p < 0.01, "< .01", pvalue_chr)
  pformat
}

# Custom kable function for tables ---------------------------------------------
custom_kable <- function(data,
                         digits = 3,
                         caption = NA,
                         col.names = NA,
                         row.names = NA,
                         format = NULL,
                         escape = TRUE,
                         format.args = list()) {
  
  out_table <- knitr::kable(data,
                            escape = escape,
                            format = format,
                            booktabs = TRUE,
                            digits = digits,
                            format.args = format.args,
                            caption = caption,
                            col.names = col.names,
                            row.names = row.names)
  
  kableExtra::kable_styling(out_table, latex_options = c("HOLD_position"),
                            position = "left",
                            full_width = F)
}

# Create dataframe with bootstrapped 95% confidence intervals ------------------
# (default 1000 samples) 
ci_boot_df <- function(.data, .summary_var, ..., ci = 0.95, groups_col = FALSE, 
                       B = 1000) {
  require(dplyr)
  # check function args ----
  df <- .data
  if (missing(.summary_var)) {
    stop("No numeric variable defined")
  }
  if (!is.logical(groups_col)) {
    groups_col <- FALSE
  }
  
  if(!missing(...)) {
    group_vars <- dplyr::enquos(..., .named = TRUE)
    groups_nm <- paste0(names(group_vars), collapse = ":")
    df <- dplyr::group_by(df, !!!group_vars)
  }
  if (is.null(B)) {
    B <- 1000
  }
  # Create function variables ----
  summary_var <- dplyr::enquo(.summary_var)
  ci_quo <- dplyr::enquo(ci)
  summary_nm <- paste0("mean_", dplyr::quo_name(summary_var))
  
  # Build output df ----
  boot_df <- dplyr::summarize(df,
                              ci_out = Hmisc::smean.cl.boot(!!summary_var, 
                                                            conf.int = ci
                                                            , B = 1000),
                              conf = c(paste0("mean"), "ci_low", "ci_high"),
                              sd = stats::sd(!!summary_var, na.rm = TRUE),
                              n = dplyr::n())
  boot_df <- dplyr::ungroup(boot_df)
  boot_df <- tidyr::pivot_wider(boot_df, names_from = conf, values_from = ci_out)
  boot_df <- dplyr::relocate(boot_df, sd, .after = ci_high)
  
  # create groups_col ----
  if (groups_col) {
    tidyr::unite(data = boot_df, col = "group",
          c(!!!group_vars), remove = TRUE)
  } else {
    boot_df
  }
}


# Build (optional grouping) dataframe with 95% CI ------------------------------
ci_df <- function(.data, .summary_var, ..., ci = 0.95, groups_col = FALSE) {
  require(dplyr)
  # check function args ----
  df <- .data
  if (missing(.summary_var)) {
    stop("No numeric variable defined")
  }
  
  if (!is.logical(groups_col)) {
    groups_col <- FALSE
  }
  
  if (!missing(...)) {
    group_vars <- dplyr::enquos(..., .named = TRUE)
    groups_nm <- paste0(names(group_vars), collapse = ":")
    df <- dplyr::group_by(df, !!!group_vars)
  }
  # create function variables ----
  summary_var <- dplyr::enquo(.summary_var)
  ci_quo <- dplyr::enquo(ci)
  summary_nm <- paste0("mean_", dplyr::quo_name(summary_var))
  
  # build final df ----
  conf_df <- dplyr::summarize(df,
                              ci_out = Hmisc::smean.cl.normal(!!summary_var, 
                                                              conf.int = ci),
                              conf = c(paste0(summary_nm), "ci_low", "ci_high"),
                              sd = stats::sd(!!summary_var, na.rm = TRUE),
                              n = dplyr::n())
  conf_df <- dplyr::ungroup(conf_df)
  conf_df <- tidyr::pivot_wider(conf_df, names_from = conf, values_from = ci_out)
  conf_df <- dplyr::relocate(conf_df, sd, .after = ci_high)
  
  # create groups_col ---- 
  if (groups_col == TRUE) {
    dplyr::unite(data = conf_df, col = "group", 
          c(!!!group_vars), remove = TRUE)
  } else {
    conf_df
  }
}

# Forest Plot ------------------------------------------------------------------
# x = measure_var
# xmin = ci_low
# xmax = ci_high
# y = group_var
forestplot_new <- function(d, .measure_var, .group_var,
                           xlab = NULL, ylab = NULL, xintercept = NULL){
  require(ggplot2)
  
  # check for inputs ====
  if (missing(.measure_var)) {
    stop("Must provide argument for '.measure_var'")
  } else if (missing(.group_var)) {
    stop("Must provide at least one argument for '.group_var'")
  }
  
  # get function args ----
  measure_var <- dplyr::enquo(.measure_var)
  group_var <- dplyr::enquo(.group_var)
  
  # build plot  ----
  p <- ggplot2::ggplot(
    d, aes(x = !!measure_var, y = !!group_var, 
           xmin = ci_low, xmax = ci_high)) + # uses default colnames from ci_df
    ggplot2::geom_pointrange(shape = 18) + 
    ggplot2::geom_vline(lty=2, xintercept = xintercept) +
    ggplot2::ylab(ylab) + 
    ggplot2::xlab(xlab)
  
  return(p)
}

