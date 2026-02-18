# Useful Functions

#===============================================================================
# Generic

# Function for "not in"
'%!in%' <- function(x,y){
  !('%in%'(x,y))
}

#===============================================================================
# NA Conversions

na_blank = function(x) ifelse(is.na(x), '', x)
blank_na = function(x) ifelse(x == '', NA, x)
na_zero = function(x) ifelse(is.na(x), 0, x)
zero_na = function(x) ifelse(x == 0, NA, x)
inf_na = function(x) ifelse(!is.finite(x), NA, x)
na_dash = function(x) ifelse(is.na(x), '-', x)

#===============================================================================
# Get Example of Data

# Function to get example values from each column
get_example = function(dat) {
  dat_example = data.frame(EXAMPLE = apply(dat, 2, function(x) x[which.max(!is.na(x) & x != "" & x != "NULL")])) %>%
    tibble::rownames_to_column(., 'VARIABLE')
  return(dat_example)  
}

#===============================================================================
# Check Completeness of Data

check_complete <- function(dat, incl_plot = FALSE, parallel = FALSE) {
  
  # Internal function to count complete observations
  complete_count <- function(var, zero_opt = NULL) {
    if(is.null(zero_opt)) {
      length(var) - (sum(var == '' | toupper(var) %in% c('NULL'), na.rm = TRUE) + sum(is.na(var)))
    } else {
      length(var) - (sum(var == '' | toupper(var) %in% c('NULL') | var == 0, na.rm = TRUE) + sum(is.na(var)))
    }
  }
  
  # Convert to data.table
  dat <- data.table::setDT(dat)
  
  # Compute counts
  if (parallel) {
    require(future.apply)
    dat_complete <- dat[, future_lapply(.SD, complete_count)] %>%
      t() %>% round(3) %>% data.frame() %>% setNames('N_COMPLETE')
  } else {
    dat_complete <- dat[, lapply(.SD, complete_count)] %>%
      t() %>% round(3) %>% data.frame() %>% setNames('N_COMPLETE')
  }
  
  # Add row count and proportion complete
  dat_complete$N_ROWS <- nrow(dat)
  dat_complete$PROP_COMPLETE <- round(dat_complete$N_COMPLETE / nrow(dat), 3)
  
  # Convert rownames to column
  dat_complete <- tibble::rownames_to_column(dat_complete, 'VARIABLE')
  
  # Optional plot
  if (isTRUE(incl_plot)) {
    complete_plot <- ggplot(dat_complete, aes(x = VARIABLE, y = PROP_COMPLETE)) +
      geom_bar(stat = 'identity', fill = '#4f5b66') +
      labs(x = 'Variable Name', y = 'Proportion Non-Missing') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1))
    
    return(list(summary = dat_complete, plot = complete_plot))
  }
  
  return(dat_complete)
  
}

#===============================================================================
# Count Unique Observations

check_unique = function(dat) {
  dat_unique = dat %>% summarise_all(uniqueN) %>%
    t() %>% data.frame() %>%
    tibble::rownames_to_column(., 'VARIABLE') %>%
    setnames(., '.', 'N_UNIQUE')
  return(dat_unique)
}

#===============================================================================
# Count Duplicate Observations

check_duplicated = function(dat) {
  dat_dups = dat %>% summarise_all(function(x) sum(duplicated(na.omit(x)))) %>%
    t() %>% data.frame() %>%
    tibble::rownames_to_column(., 'VARIABLE') %>%
    setnames(., '.', 'N_DUPLICATED')
  return(dat_dups)
}

#===============================================================================
# Basic Summary Statistics
numeric_summary = function(dat, except_cols = NULL) {
  
  # Set as data.table
  dat = setDT(dat)
  
  # Identify numeric columns
  num_cols = setdiff(names(dat)[sapply(dat, is.numeric)], except_cols)
  
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
  sumstats <- cbind(mean_vals,
                     "SD" = sd_vals[, 2],
                     "MIN" = min_vals[, 2],
                     "MED" = med_vals[, 2],
                     "MAX" = max_vals[, 2])
  
  # Round output
  num_cols = c("MEAN", "SD", "MIN", "MED", "MAX")
  sumstats[num_cols] = lapply(sumstats[num_cols], round, 3)
  
  return(sumstats)
  
}

#===============================================================================
# Review Data with Single Function

reviewData = function(data, parallel_complete = NULL) {
  data_example = get_example(data)
  data_complete = check_complete(data, parallel = parallel_complete)
  data_unique = check_unique(data)
  data_duplicated = check_duplicated(data)
  data_numeric = numeric_summary(data)
  data_summary <- data_example |>
    dplyr::left_join(data_unique, by = "VARIABLE") |>
    dplyr::left_join(data_duplicated, by = "VARIABLE") |>
    dplyr::left_join(data_numeric, by = "VARIABLE")
  return(data_summary)
}

#===============================================================================
# Check Date Ranges

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

#===============================================================================
# Anonymize Multiple Columns

anonymize_data = function(data, cols) {
  generate_group_ids <- function(col) {as.integer(as.factor(col))}
  data[, (cols) := lapply(.SD, generate_group_ids), .SDcols = cols]
}

#===============================================================================
# Summarize Boolean Columns

bool_summary = function(datatable, id_var, except_cols = NULL) {
  
  # Identify Boolean columns
  bool_cols = setdiff(names(datatable)[sapply(datatable, is.logical)], except_cols)
  
  # Reshape wide to long
  meltedDT = melt(datatable, id.vars = id_var, measure.vars = bool_cols, variable.name = "VARIABLE")
  
  # Count true and false
  summaryDT = dcast(meltedDT, VARIABLE ~ value, length)
  setnames(summaryDT, c("TRUE", "FALSE"), c("TRUE_COUNT", "FALSE_COUNT"))
  
  # Calculate totals and proportions
  summaryDT[, TOTAL_COUNT := TRUE_COUNT + FALSE_COUNT]
  summaryDT[, PROP_TRUE := round(TRUE_COUNT / TOTAL_COUNT, 3)]
  summaryDT[, PROP_FALSE := round(FALSE_COUNT / TOTAL_COUNT, 3)]
  
  # Select
  summaryDT = summaryDT %>% select(VARIABLE, TOTAL_COUNT, TRUE_COUNT, FALSE_COUNT, PROP_TRUE, PROP_FALSE)
  
  return(summaryDT)
  
}

#===============================================================================
# Remove Non-UTF-8

clean_char_utf8 = function(dat) {
  dat <- data.table::as.data.table(dat)
  char_cols <- names(dat)[sapply(dat, is.character)]
  dat[, (char_cols) := lapply(.SD, iconv, from = "ASCII", to = "UTF-8", sub = ''), .SDcols = char_cols]
  return(dat)
}
