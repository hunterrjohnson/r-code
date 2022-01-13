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
