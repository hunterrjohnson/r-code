# Function for "not in"
'%!in%' <- function(x,y){
  !('%in%'(x,y))
}

# Quick proportion for binary variables
bin_prop <- function(x, na_opt = FALSE){
  if (na_opt == FALSE) {
    table(x)[2] / sum(table(x))
  }
  else {
    table(x)[2] / sum(table(x, useNA = 'ifany'))
  }
}

# Functions to get missing count/proportion for each column of a data frame
null_count <- function(var, zero_opt = NULL) {
  if(is.null(zero_opt)) {
    sum(var == '' | toupper(var) %in% c('NULL'), na.rm=T) + sum(is.na(var))
  } else {
    sum(var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm=T) + sum(is.na(var))
  }
}

null_prop <- function(var, zero_opt = NULL) {
  if(is.null(zero_opt)) {
    ( sum(var == '' | toupper(var) %in% c('NULL'), na.rm=T) +
        + sum(is.na(var)) ) / length(var)
  } else {
    ( sum(var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm=T) +
        + sum(is.na(var)) ) / length(var)
  }
}

# Function to check missingness in data frame and report in new data frame with optional plot
check_missing <- function(dat, incl_plot = FALSE, parallel = FALSE) {

  if(parallel == TRUE){
    require(future.apply)
    # Count missing observations
    dat_na <- setDT(dat)[, future_lapply(.SD, null_count)] %>% t() %>%
      round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'N_MISSING')
  } else {
    # Count missing observations
    dat_na <- setDT(dat)[, lapply(.SD, null_count)] %>% t() %>%
      round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'N_MISSING')
  }
  
  # Get number of rows
  dat_na$N_ROWS <- nrow(dat)
  
  # Get proportion missing
  dat_na$PROP_MISSING <- round(dat_na$N_MISSING / nrow(dat), 3)
  
  # Convert rownames to first column
  dat_na <- tibble::rownames_to_column(dat_na, 'VARIABLE')
  
  # Assign to global environment
  assign(paste0(deparse(substitute(dat)), '_NA'), dat_na, envir = .GlobalEnv)
  
  # Create plot and assign to global environment
  if (isTRUE(incl_plot)) {
    missing_plot <<- ggplot(dat_na, aes(x = VARIABLE, y = PROP_MISSING)) +
      theme_minimal() +
      geom_bar(stat = 'identity', fill = '#4f5b66') +
      labs(x = 'Variable Name', y = 'Proportion Missing') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1))
  }
  
}

# Functions to get non-missing count/proportion for each column of a data frame
complete_count <- function(var, zero_opt = NULL) {
  if(is.null(zero_opt)) {
    length(var) - ( sum(var == '' | toupper(var) %in% c('NULL'), na.rm=T) + sum(is.na(var)) )
  } else {
    length(var) - ( sum(var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm=T) + sum(is.na(var)) )
  }
}

complete_prop <- function(var, zero_opt = NULL) {
  if(is.null(zero_opt)) {
    1 - ( ( sum(var == '' | toupper(var) %in% c('NULL'), na.rm=T) +
            + sum(is.na(var)) ) / length(var) )
  } else {
    1 - ( ( sum(var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm=T) +
              + sum(is.na(var)) ) / length(var) )
  }
}

# Function to check completeness in data frame and report in new data frame with optional plot
check_complete = function(dat, incl_plot = FALSE, parallel = FALSE) {
  
  if(parallel == TRUE){
    require(future.apply)
    # Count non-missing observations
    dat_complete <- setDT(dat)[, future_lapply(.SD, complete_count)] %>% t() %>%
      round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'N_COMPLETE')
  } else {
    # Count non-missing observations
    dat_complete <- setDT(dat)[, lapply(.SD, complete_count)] %>% t() %>%
      round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'N_COMPLETE')
  }
  
  # Get number of rows
  dat_complete$N_ROWS <- nrow(dat)
  
  # Get proportion complete
  dat_complete$PROP_COMPLETE <- round(dat_complete$N_COMPLETE / nrow(dat), 3)
  
  # Convert rownames to first column
  dat_complete <- tibble::rownames_to_column(dat_complete, 'VARIABLE')
  
  # Assign to global environment
  assign(paste0(deparse(substitute(dat)), '_COMPLETE'), dat_complete, envir = .GlobalEnv)
  
  # Create plot and assign to global environment
  if (isTRUE(incl_plot)) {
    complete_plot <<- ggplot(dat_complete, aes(x = VARIABLE, y = PROP_COMPLETE)) +
      theme_minimal() +
      geom_bar(stat = 'identity', fill = '#4f5b66') +
      labs(x = 'Variable Name', y = 'Proportion Non-Missing') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1))
  }
  
}

# Test (4 NAs, 3 non-missing)
# df = data.frame('A' = c(1, 2, 'A', '', 'NULL', NA, ''))
# check_complete(df)
# check_missing(df)

# Function to compare two data sets (e.g. old vs new)
compare_basic = function(dat1, dat2, id_var = NULL) {
  
  # Get data frame names as strings
  name1 = deparse(substitute(dat1))
  name2 = deparse(substitute(dat2))
  
  # Get ID name if supplied
  if(!is.null(id_var)) {id_name = deparse(substitute(id_var))}
  
  ### COLUMNS
  cat('*** COMPARE COLUMNS ***', '\n')
  
  # Check that number of columns is consistent
  cat('Number of columns in ', name1, ': ', ncol(dat1), '\n')
  cat('Number of columns in ', name2, ': ', ncol(dat2), '\n')
  
  # Check that column names are consistent
  if (ncol(dat1) == ncol(dat2) & length(intersect(names(dat1), names(dat2))) == ncol(dat1)) {
    cat('All columns have same names', '\n')
  } else {
    cat( paste0('Columns in ', name1, ' and not in ', name2, ':\n'),
         setdiff(names(dat1), names(dat2)), '\n')
    cat( paste0('Columns in ', name2, ' and not in ', name1, ':\n'),
         setdiff(names(dat2), names(dat1)), '\n')
  }
  
  ### ROWS
  cat('\n','*** COMPARE ROWS ***', '\n')
  
  # Check difference in number of rows
  cat('Number of rows in ', name1, ': ', nrow(dat1), '\n')
  cat('Number of rows in ', name2, ': ', nrow(dat2), '\n')
  cat('Difference between ', name1, 'and ', name2, ': ', nrow(dat1) - nrow(dat2), '\n')
  
  ### IDS
  cat('\n','*** COMPARE IDS ***', '\n')
  
  # Check difference in number of unique IDs
  if(!is.null(id_var)) {
    cat('ID Variable:', id_name, '\n')
    cat('Number of unique IDs in', name1, ': ', uniqueN(dat1[, get(id_var)]), '\n')
    cat('Number of unique IDs in', name2, ': ', uniqueN(dat2[, get(id_var)]), '\n')
    cat('Difference between ', name1, 'and ', name2, ': ', uniqueN(dat1[, get(id_var)]) - uniqueN(dat2[, get(id_var)]), '\n')
  }

}

# Function to check date ranges and panel balance (if applicable)
check_dates = function(dat, date_var, id_var = NULL) {
  
  # Get ID and date variable names
  id_var_name = deparse(substitute(id_var))
  date_var_name = deparse(substitute(date_var))
  cat('Date Variable: ', date_var_name, '\n')
  
  # Check min and max date
  min_date = min(dat[, get(date_var)])
  max_date = max(dat[, get(date_var)])
  cat('Date Range: ', as.character(min_date), 'to', as.character(max_date), '\n')
  
  # Check balanced panel
  if(!is.null(id_var)) {
    dat = plm::pdata.frame(dat, index = c(id_var, date_var))
    cat('Panel is balanced: ', plm::is.pbalanced(temp), '\n')
  } else {cat('Set "id_var" to check for panel balance.')}
  
}

# Remove non-UTF-8 characters from data frame or data table
char_cols <- unlist(lapply(dat, is.character))
char_cols <- names(data.frame(dat)[, char_cols])
dat[, (char_cols) := lapply(.SD, iconv, from = "ASCII", to = "UTF-8", sub = ''), .SDcols = char_cols]

# Miscellaneous NA conversions
na_blank = function(x) ifelse(is.na(x), '', x)
blank_na = function(x) ifelse(x == '', NA, x)
na_zero = function(x) ifelse(is.na(x), 0, x)
zero_na = function(x) ifelse(x == 0, NA, x)
inf_na = function(x) ifelse(!is.finite(x), NA, x)
na_dash = function(x) ifelse(is.na(x), '-', x)

# Check which rows are different (for dataframes that should be identical)
different_rows = function(df1, df2) {
  
  # Create list of rows
  rows = list()
  
  # Identify identical rows
  for (i in 1:nrow(df1)) {
    if(identical(df1[i, ], df2[i, ])) {
      rows[[i]] = TRUE
    } else {
      rows[[i]] = FALSE
    }
  }
  
  # Create data table of rows
  rows = data.table(identical = unlist(rows))
  rows = tibble::rownames_to_column(rows, 'row_number')
  
  # Filter to non-identical rows
  rows = rows[!(identical)]
  
  # Select rows that differ in df1 and df2
  nonidentical1 <<- df1[as.numeric(rows$row_number), ]
  nonidentical2 <<- df2[as.numeric(rows$row_number), ]
  
}

# Basic summary of numeric variables
numeric_summary = function(dat) {
  
  # Identify numeric columns
  num_cols = names(dat)[sapply(dat, is.numeric)]
  
  # Get summary stats
  mean_vals = dat[, lapply(.SD, mean, na.rm = TRUE), .SDcols = num_cols] %>%
    t() %>% data.frame() %>% tibble::rownames_to_column(., "VARIABLE") %>% setnames(., '.', 'MEAN')
  sd_vals = dat[, lapply(.SD, sd, na.rm = TRUE), .SDcols = num_cols] %>%
    t() %>% data.frame() %>% tibble::rownames_to_column(., "VARIABLE") %>% setnames(., '.', 'SD')
  min_vals = dat[, lapply(.SD, min, na.rm = TRUE), .SDcols = num_cols] %>%
    t() %>% data.frame() %>% tibble::rownames_to_column(., "VARIABLE") %>% setnames(., '.', 'MIN')
  med_vals = dat[, lapply(.SD, median, na.rm = TRUE), .SDcols = num_cols] %>%
    t() %>% data.frame() %>% tibble::rownames_to_column(., "VARIABLE") %>% setnames(., '.', 'MEDIAN')
  max_vals = dat[, lapply(.SD, max, na.rm = TRUE), .SDcols = num_cols] %>%
    t() %>% data.frame() %>% tibble::rownames_to_column(., "VARIABLE") %>% setnames(., '.', 'MAX')

  # Arrange summary stats values
  sumstats <<- cbind(mean_vals,
                     "SD" = sd_vals[, 2],
                     "MIN" = min_vals[, 2],
                     "MED" = med_vals[, 2],
                     "MAX" = max_vals[, 2])
  
}

# Simple way to anonymize multiple columns
anonymize_data = function(data, cols) {
  generate_group_ids <- function(col) {as.integer(as.factor(col))}
  data[, (cols) := lapply(.SD, generate_group_ids), .SDcols = cols]
}

# Function to scrape text from PDF
scrape_zips = function(url) {
  
  # Extract text from PDF
  pdf_text = pdf_tools::pdf_text(url)
  
  # Identify matches (e.g. 5-digit zip codes) using regular expressions
  matches = regmatches(pdf_text, gregexpr("\\b\\d{5}\\b", pdf_text))
  
  # Convert to numeric
  zips = unique(as.numeric(unlist(numbers)))
  
}
