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
    sum(is.na(var) | var == '' | toupper(var) %in% c('NULL'), na.rm=T)
  } else {
    sum(is.na(var) | var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm=T)
  }
}

null_prop <- function(var, zero_opt = NULL) {
  if(is.null(zero_opt)) {
    sum(is.na(var) | var == '' | toupper(var) %in% c('NULL'), na.rm=T) / length(var)
  } else {
    sum(is.na(var) | var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm=T) / length(var)
  }
}

# Function to check missingness in data frame and report in new data frame with optional plot
check_missing <- function(dat, incl_plot = FALSE) {
  
  # Count missing observations
  dat_na <- setDT(dat)[, lapply(.SD, null_count)] %>% t() %>%
    round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'N_MISSING')
  
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
