# Replace excessive whitespace with single space including in between strings
gsub("\\s+", " ", str_trim(df$col_1))

# Extract everything before first underscore not including underscore
as.character(stringr::str_extract_all(df$col_1, "^[^_]+(?=_)"))

# Create flag to remove any apartment units from addresses
df$drop <- ifelse(grepl("(?i)(APT|UNIT|#|STE|SPC|BLDG)\\s?[a-zA-Z0-9]+", df$address), 1, 0)
df$drop <- ifelse(grepl("(\\s)(?i)(APT|UNIT|STE|SUITE)(\\s)(.*)", df$Street), 1, 0) # need to be followed by a space
df$drop <- ifelse(grepl("(\\s)(?i)(#|SPC|BLDG|BLDNG)(.*)", df$Street), 1, df$drop) # don't need to be followed by a space
df$drop <- ifelse(grepl("(\\s)(?i)(LOWR|UPPR)(.*)", df$Street), 1, df$drop) # rare weird case in meter metadata
df$drop <- ifelse(grepl("(\\s)(?i)(HSE)(.*)", df$Street), 1, df$drop) # rare weird case in meter metadata
df$drop <- ifelse(grepl("(\\s)(/)(.*)", df$Street), 1, df$drop) # rare weird case in meter metadata
df$drop <- ifelse(grepl("[ ]\\(.*", df$Street), 1, 0) # for checking parentheses
df$drop <- ifelse(grepl("(\\-)(\\s?)[a-zA-Z]+(.*)", df$Street), 1, 0) # for checking hyphens
df$drop <- ifelse(grepl("(\\-)$", df$Street), 1, df$drop)
df$drop <- ifelse(grepl("^\\d+\\s?-\\s?\\d+", df$Street), 1, df$drop) # ranged street addresses
df$drop <- ifelse(grepl("(REAR|FRNT)(.*)$", df$Street), 1, df$drop) # rear/front at beginning/end of addresses
