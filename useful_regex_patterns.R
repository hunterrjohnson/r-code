# Replace excessive whitespace with single space including in between strings
gsub("\\s+", " ", str_trim(df$col_1))

# Extract everything before first underscore not including underscore
as.character(stringr::str_extract_all(df$col_1, "^[^_]+(?=_)"))

# Create flag to remove any apartment units from addresses
df$drop <- ifelse(grepl("(?i)(APT|UNIT|#|STE)\\s?[a-zA-Z0-9]+", df$address), 1, 0)
