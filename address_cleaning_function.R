clean_address = function(addr_dat, id_var, expand_range = FALSE, remove_units = FALSE, remove_paren = FALSE, remove_direc = FALSE) {
  
  require(postmastr)
  
  #=============================================================================
  # PRE-CLEANING 
  
  # Convert NAs to blanks
  postmastr_dat = setDT(addr_dat)[, lapply(.SD, na_blank)]
  
  # Convert to character
  postmastr_dat <- postmastr_dat[, lapply(.SD, as.character)]
  
  # Change addresses to upper case
  postmastr_dat$Street <- toupper(postmastr_dat$Street)
  
  # Remove periods (e.g. "Mt." or "Blvd.")
  postmastr_dat$Street <- gsub('\\.', '', postmastr_dat$Street)
  
  #=============================================================================
  # REMOVE UNITS
  
  # Note: removing units can be tricky and may require doing manually
  
  if(remove_units == TRUE) {
    
    # Create flag to remove any apartment units from addresses
    postmastr_dat$drop <- ifelse(grepl("(\\s)(?i)(APT|UNIT|STE|SUITE)(\\s)(.*)", postmastr_dat$Street), 1, 0) # need to be followed by a space
    postmastr_dat$drop <- ifelse(grepl("(\\s)(?i)(#|SPC|BLDG|BLDNG)(.*)", postmastr_dat$Street), 1, postmastr_dat$drop) # don't need to be followed by a space
    postmastr_dat$drop <- ifelse(grepl("(\\s)(?i)(LOWR|UPPR)(.*)", postmastr_dat$Street), 1, postmastr_dat$drop) # rare weird case
    postmastr_dat$drop <- ifelse(grepl("(\\s)(?i)(HSE)(.*)", postmastr_dat$Street), 1, postmastr_dat$drop) # rare weird case
    postmastr_dat$drop <- ifelse(grepl("(\\s)(/)(.*)", postmastr_dat$Street), 1, postmastr_dat$drop) # rare weird case
    cat('Units Removed From:', sum(postmastr_dat$drop), '(', round(sum(postmastr_dat$drop) / nrow(postmastr_dat) * 100, 2), '%)\n')
    
    # Remove apartment units from addresses
    postmastr_dat$Street <- gsub("(\\s)(?i)(APT|UNIT|STE|SUITE)(\\s)(.*)", "", postmastr_dat$Street)
    postmastr_dat$Street <- gsub("(\\s)(?i)(#|SPC|BLDG|BLDNG)(.*)", "", postmastr_dat$Street)
    postmastr_dat$Street <- gsub("(\\s)(?i)(LOWR|UPPR)(.*)", "", postmastr_dat$Street)
    postmastr_dat$Street <- gsub("(\\s)(?i)(HSE)(.*)", "", postmastr_dat$Street)
    postmastr_dat$Street <- gsub("(\\s)(/)(.*)", "", postmastr_dat$Street)
  
  }
  
  #=============================================================================
  # REMOVE PARENTHESES
  
  if(remove_paren == TRUE) {
    
    postmastr_dat$drop <- ifelse(grepl("[ ]\\(.*", postmastr_dat$Street), 1, 0) # for checking parentheses
    cat('Parentheses Removed From:', sum(postmastr_dat$drop), '(', round(sum(postmastr_dat$drop) / nrow(postmastr_dat) * 100, 2), '%)\n')
    postmastr_dat$Street <- gsub("[ ]\\(.*", "", postmastr_dat$Street)
    
  }
  
  #=============================================================================
  # REMOVE REAR/FRONT
  
  if(remove_direc == TRUE) {
    
    # Remove rear/front from ends of addresses
    postmastr_dat$drop <- ifelse(grepl("(REAR|FRNT)(.*)$", postmastr_dat$Street), 1, 0) # for checking rear / front
    cat('REAR/FRONT Removed From:', sum(postmastr_dat$drop), '(', round(sum(postmastr_dat$drop) / nrow(postmastr_dat) * 100, 2), '%)\n')
    postmastr_dat$Street <- gsub("(REAR|FRNT)(.*)$", "", postmastr_dat$Street)
    postmastr_dat$drop <- NULL
    
  }
  
  #=============================================================================
  # EXPAND RANGED ADDRESSES
  
  if(expand_range == TRUE) {
    
    ranged_dat = postmastr_dat[grepl('^\\d+\\s?-\\s?\\d+', Street)]
    ranged_dat$Street <- gsub(" - ", "-", ranged_dat$Street) # Fixes some ranged addresses with spaces
    get_range = function(x) regmatches(x, regexpr('^\\d+\\s?-\\s?\\d+', x))
    ranged_dat[, range := vapply(Street, get_range, FUN.VALUE = character(1))]
    ranged_dat[, c('start', 'end') := tstrsplit(range, '-')]
    
    # Filter out suspicious ranges (e.g. too large or truncated)
    ranged_dat[, trunc_end := (nchar(end) < nchar(start))]
    ranged_dat[(trunc_end), end := correct_end(start, end)]
    ranged_dat[, c('start', 'end') := lapply(.SD, as.numeric), .SDcols = c('start', 'end')]
    ranged_dat = ranged_dat[!((trunc_end) & end < start)]
    ranged_dat = ranged_dat[!(abs(start - end) > 300)] # Choice of "too large" is subjective; here, > 300 is chosen
    
    # Create range expand function
    expandRange = function(dat) {
      startRange <- min(dat$start, dat$end)
      endRange <- max(dat$start, dat$end)
      data.table(varID = dat$varID, 
                 Street = dat$Street, 
                 zip = dat$zip, 
                 num = c(startRange:endRange)[c(TRUE, FALSE)]) %>%
        .[, .(meterID, Street = replace_range(num, Street), zip)]
    }
    
    # Split ranged addresses
    ranged_dat = split(ranged_dat, by = c('varID','zip')) %>%
      lapply(expandRange) %>%
      rbindlist
    
    # Join ranged addresses back to non-ranged addresses
    postmastr_dat = rbind(postmastr_dat[!(varID %in% ranged_dat$varID)], ranged_dat) %>%
      .[, .(varID, Street, zip = zip)] %>%
      unique
    
  }
  
  #=============================================================================
  # CREATE DICTIONARIES
  
  # Create directional dictionary
  dirs <- pm_dictionary(type = "directional", case = "upper", locale = "us")
  
  # Append additional suffixes based on comparison w/ old SmartyStreets output
  extra_suffixes <- pm_append(type = "suffix",
                              input = c("CYN","CMN","CMNS","CRST","CURV","CRT","XRD"),
                              output = c("Cyn","Cmn","Cmns","Crst","Curv","Crd","Xrd"), locale = "us")
  
  # Create suffix dictionary
  suffixes <- pm_dictionary(type = "suffix", case = "upper", locale = "us", append = extra_suffixes)
  
  #===============================================================================
  # PARSE AND STANDARDIZE
  
  # Identify addresses
  postmastr_dat <- pm_identify(postmastr_dat, var = 'Street')
  
  # Parse
  parsed_addr_dat <- postmastr_dat %>%
    pm_parse(input = 'short', address = 'Street', output = 'short', keep_parsed = 'yes',
             dir_dict = dirs, suffix_dict = suffixes) %>%
    setnames(.,
             c('pm.house','pm.street','pm.streetSuf','pm.preDir'),
             c('num','stre','suffix','pre')) %>%
    select(id_var, num, pre, stre, suffix, zip) %>%
    arrange(id_var, num, pre, stre, suffix, zip)
  
  # Standardize directional street names
  parsed_addr_dat <- parsed_addr_dat %>%
    mutate(stre = ifelse(stre == 'East', 'E', stre),
           stre = ifelse(stre == 'West', 'W', stre),
           stre = ifelse(stre == 'North', 'N', stre),
           stre = ifelse(stre == 'South', 'S', stre))
  
  # Remove suffixes from numeric street names (e.g. "33rd" to "33")
  parsed_addr_dat <- parsed_addr_dat %>%
    mutate( stre = ifelse(stringr::str_detect(stre, "\\d+(?=st|nd|rd|th)"),
                          stringr::str_extract(stre, "\\d+(?=st|nd|rd|th)"),
                          stre) )
  
  # Fix "1/2" addresses
  parsed_addr_dat <- parsed_addr_dat %>%
    mutate(num = ifelse(grepl( "1 At 2", stre), paste0(num, ' 1/2'), num),
           stre = gsub("1 At 2", "", stre))
  
  # Extract numbers before hyphen when hyphenated
  parsed_addr_dat <- parsed_addr_dat %>%
    mutate(num = ifelse(grepl("^[0-9]+[A-Z]?\\-", num), # any combo of digits, optional letters, include hyphen
                        stringr::str_extract_all(num, "^[0-9]+[^-]+(?=-)"), # everything starting w/ digits, before hyphen
                        num))
  
  # Replace addresses with numeric suffixes with NAs
  parsed_addr_dat$num <- gsub("(\\d)+(?i)(st|nd|rd|th)\\b", "", parsed_addr_dat$num)
  
  # Trim whitespace
  parsed_addr_dat = setDT(parsed_addr_dat)[, lapply(.SD, trimws)]
  
  #=============================================================================
  # RETURN RESULTS
  
  parsed_addr_dat <<- parsed_addr_dat
  
}

# Example
clean_address(addr_dat = test,
              id_var = 'property_id',
              expand_range = FALSE,
              remove_units = TRUE,
              remove_paren = TRUE,
              remove_direc = TRUE)
              
              
              
              
