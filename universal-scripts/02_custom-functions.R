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
  rbind(head(d,10),tail(d,10))
}

# Get first 5 each of rows, columns --------------------------------------------
hh <- function(d) {
  d[1:5,1:5]
}

# Get last element of a vector or list -----------------------------------------
last <- function(x) {
  x[length(x)]
}

# Install packages -------------------------------------------------------------
install_load_packages <- function() {
  packages <- get("packages", envir = globalenv())
  github_packages <- get("github_packages", envir = globalenv())

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
    c(packages[which(!packages %in% gsub("package", "", attached_packages))],
      github_packages$package[which(!github_packages$package %in% gsub(
        "package", "", attached_packages))])
  packages <- c(packages, github_packages$package)
  
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
  map(list, ~ paste0(custom_left, .x, custom_right)) %>% 
    reduce(~ paste(.x, .y, sep = "|"))
  }

# Format p-value ---------------------------------------------------------------
format_pval <- function(p, digits = 3) {
  pvalue_chr <- p %>% round(digits = digits) %>% as.character()
  if (digits == 3)
    pformat <- ifelse(p < 0.001, "< .001", pvalue_chr)
  else if (digits == 2)
    pformat <- ifelse(p < 0.01, "< .01", pvalue_chr)
  pformat
}

# Custom kable function for tables ---------------------------------------------
custom_kable <- function(data, digits = 3, caption = NA, col.names = NA) {
  knitr::kable(data, 
               booktabs = TRUE,
               digits = digits, 
               caption = caption, 
               col.names = col.names) %>% 
    kableExtra::kable_styling(latex_options = c("HOLD_position"), 
                              position = "left",
                              full_width = F)
}

# Create dataframe with bootstrapped 95% confidence intervals ------------------
# (default 1000 samples) 
ci_boot_df <- function(x, var = "value", B = NULL) {
  if (is.null(B)) {
    B <- 1000
  } else {
    B <- B
  }
  boot_df <- rbind(Hmisc::smean.cl.boot(x[[var]], !!B)) %>% 
    data.frame %>% 
    setNames(c("mean", "conf.low", "conf.high"))
  return(boot_df)
}

# Build (optional grouping) dataframe with 95% CI ====
ci_df <- function(.data, .summary_var, ..., ci = 0.95, groups_col = FALSE) {
  # check function args =====
  if (missing(.summary_var)) {
    stop("No numeric variable defined")
  }
  
  if (!is.logical(groups_col)) {
    groups_col <- FALSE
  }
  
  if (!missing(...)) {
    group_vars <- enquos(..., .named = TRUE)
    groups_nm <- paste0(names(group_vars), collapse = ":")
    df <- df %>% group_by(!!!group_vars)
  }
  # create function variables =====
  summary_var <- enquo(.summary_var)
  ci_quo <- enquo(ci)
  summary_nm <- paste0("mean_", quo_name(summary_var))
  
  # build final df =====
  conf_df <- summarize(df,
                       ci_out = Hmisc::smean.cl.normal(!!summary_var, 
                                                       conf.int = ci),
                       conf = c(paste0(summary_nm), "ci_low", "ci_high"),
                       n = n()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = conf, values_from = ci_out)
  
  # create groups_col ===== 
  if (groups_col == TRUE) {
    return(unite(data = conf_df, col = "group", 
                 c(!!!group_vars), remove = FALSE))
  } else {
    return(conf_df)
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
  
  # get function args =====
  measure_var <- enquo(.measure_var)
  group_var <- enquo(.group_var)
  
  # build plot  =====
  p <- ggplot(
    d, aes(x = !!measure_var, y = !!group_var, 
           xmin = ci_low, xmax = ci_high)) + # uses default colnames from ci_df
    geom_pointrange(shape = 18) + 
    geom_vline(lty=2, xintercept = xintercept) +
    ylab(ylab) + 
    xlab(xlab)
  
  return(p)
}