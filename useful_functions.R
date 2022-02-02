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
null_count <- function(var) {
  sum(is.na(var) | var == '' | var %in% c('NULL','null','Null'), na.rm=T)
}

null_prop <- function(var) {
  sum(is.na(var) | var == '' | var %in% c('NULL','null','Null'), na.rm=T) / length(var)
}

# Function to check missingness in data frame and report in new data frame with optional plot
check_missing <- function(dat, incl_plot = FALSE) {
  
  # Count missing observations
  dat_na_count <- dat %>% summarise_all(null_count) %>% t() %>% 
    round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'N_MISSING')
  
  # Count missing observations
  dat_na_prop <- dat %>% summarise_all(null_prop) %>% t() %>% 
    round(digits = 3) %>% data.frame() %>% setnames(old = '.', new = 'PROP_MISSING')
  
  # Join into one data frame
  dat_na <- cbind(dat_na_count, dat_na_prop) %>% data.frame
  
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
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  
}
