# Get ACS Data

lapply(c('acs','dplyr','data.table'), library, character.only = TRUE)

'%!in%' <- function(x,y){
  !('%in%'(x,y))
}

setwd('')

#===============================================================================

# Here is the table to lookup ACS 5-year codes used in this script:
# https://api.census.gov/data/2019/acs/acs5/variables.html

api.key.install('') # Census API key
cal_geo <- geo.make(state = 'NY', county = '*', tract = '*', block.group = '*') # geo.make required to retrieve data

# Note: getting S1101 ACS group variables from the API is apparently not possible
# S1101_C01_002E must be obtained from direct website download

# Retrieve block group data for all NY counties for 2019 from ACS 5-year survey
# - B19013: Median household income
cal_bgdata <- acs.fetch(endyear = 2019, span = 5, geography = cal_geo, table.number = 'B19013')

# Clean up data
dat <- data.frame(cal_bgdata@estimate)
dat <- dat %>% mutate(state = cal_bgdata@geography$state,
                      county = cal_bgdata@geography$county,
                      tract = cal_bgdata@geography$tract,
                      blockgroup = cal_bgdata@geography$blockgroup)

# Label columns for clarity
dat = expss::apply_labels(dat, B19013_001 = 'median_hh_inc')

fwrite(dat, 'acs_data.csv')



