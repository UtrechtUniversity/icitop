## Sourcing external scripts
# Written by Dorien Huijser
# Last edit 2023-12-08
# NB: THIS SCRIPT IS NOT EXECUTABLE, JUST FOR ILLUSTRATION PURPOSES

# R allows for the sourcing of external scripts.
# The advantage of this is that you do not need to write 
# really long scripts. You can cut your processing steps
# into several R scripts or R functions
# and then use 1 "master" R script in which you
# "call on" all the other scripts in a certain order.
#
# For example, you could have the following scripts:
#
# scripts/readdata.R
# scripts/renamedata.R
# scripts/excludedata.R
#
# And then in your "master" script (e.g., one master script per study)
# do something like:

# Source the functions: 
# this will make the functions available in your Environment
source("scripts/readdata.R")
source("scripts/renamedata.R")
source("scripts/excludedata.R")

# Read the data
data <- readdata("path/to/your/raw_data.sav",
                 any_other_inputs_you_specified_in_the_script)

# Rename the data
config_file <- read.csv("path/to/config/file.csv")
renamed_data <- renamedata(data,
                           config = config_file,
                           any_other_inputs_you_specified_in_the_script)

# Exclude specified data
excluded_renamed_data <- excludedata(renamed_data,
                                     config = config_file,
                                     any_other_inputs_you_specified_in_the_script)

# Note that it is also possible to include multiple functions
# in your external scripts (e.g., readdata.R could contain 
# multiple functions). Once sourced, all of the functions in that 
# R script should become available in your Environment for use.