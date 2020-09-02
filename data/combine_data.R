library(fst) # loads data really fast
library(dplyr)
library(data.table)

# This combine_data.R script loads all data files (crash, person, vehicle in the crash_databases folder)
# that is a FST, combines into a single long data.table format and saves as a FST
# This does so by grabbing all files with 'crash' (or person/vehicle) and FST in the name


# Function to import all data of type "databasetype" and is a FST # https://gist.github.com/aammd/9ae2f5cce9afd799bafb
# Then SAVES a fst of the combined data in data/
import_all_databases <- function(databasetype) {
  temp <- list.files(path = "crash_databases/", pattern = paste0(databasetype, ".fst$")) # look for NAME.fst pattern
  read_fst2 <- function(path) read_fst(paste0("crash_databases/", path))
  combined_data <- lapply(temp, read_fst2) %>% rbindlist()
  # combined_data
  write_fst(combined_data, paste0("data/all_", databasetype))
}

# Note: Creates a newtime variable - time of 0 and 999 is NA
import_all_databases("crash")

# Note: Creates an age_group variable
import_all_databases("person")

import_all_databases("vehicle")

# speedflag, teenflag, olderflag ("Y"), all other crash flags
import_all_databases("crsh_flags")

# microbenchmark::microbenchmark(
#   import_all_databases("crash"), # 237
#   read_fst("all_crash") # 195
#   )
