#################################################################################
##############################  CONCATENATION  ##################################
#################################################################################

#################################################################################
#########                        Load R Packages                        #########
#################################################################################

# Import the library required for reading SPSS files
library(foreign)

# Import the library required for reshaping data frames
library(plyr)
library(dplyr)
library(reshape2)


#################################################################################
###########                        Load Data                        #############
#################################################################################

# Set up a file path to the directory holding the txt files to be converted as 
# well as a file path to a directory to which the txt files will written
input_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_sig\\" # w
output_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_sig\\" # w

# Get a list of all long csv files (datatable) that need to be converted to wide txt files
files <- list.files(path=input_file_path)


#################################################################################
###########             Creating the aggregated data set            #############
#################################################################################

# Creating the data set to be evaluated ("agg_countries")
for (i in files) {
  
  ## Setting the local directory to file
  locdir <- paste(input_file_path, i, sep = "")
  
  ## If the merged dataset does not already exist, create it
  if (!exists("agg_countries")) {
    agg_countries <- read.csv(locdir, header = TRUE, sep = ";" , dec = ",")
  }
  ## If the merged dataset already exists, append to it 
  else {
    agg_countries_temp <- read.csv(locdir, header = TRUE, sep = ";" , dec = ",")
    agg_countries <- rbind(agg_countries, agg_countries_temp)
    rm(agg_countries_temp)
  }
  rm(locdir)
}
print("Aggregate Countries has been created")
cat("\n")

# Save aggregated file to input file path
agg_saved <- paste(input_file_path , "sig_agg_countries.csv" , sep = '')
# write.csv2(agg_countries, agg_saved)
write.table(agg_countries, agg_saved, dec = "," , sep = "\t" , row.names = FALSE)
files <- list.files(path=input_file_path)
rm(agg_countries)
print("Aggregate Countries have been saved")
cat("\n")
cat("\n")


# Free the imported libraries from memory
detach("package:foreign")
detach("package:plyr")
detach("package:dplyr")
detach("package:reshape2")
rm(agg_saved, i, input_file_path, output_file_path)

################################################################################################################
# Paste the code below into r studio. Ensure correct file path is used.
# source("C:\\R\\NBN_ad_hoc_analyses\\R_scripts\\150903 NBN_file_concatenation.r")
################################################################################################################
