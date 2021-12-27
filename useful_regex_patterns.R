# Replace excessive whitespace with single space including in between strings
gsub("\\s+", " ", str_trim(df$col_1))
