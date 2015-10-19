#################################################################################
##################################  AD HOC  #####################################
#################################################################################

#################################################################################
#########                        Load R Packages                        #########
#################################################################################

# Import the library required for reading SPSS files
library(foreign)

# Import the library required for reshaping data frames
# library(plyr)
library(dplyr)
library(reshape2)


#################################################################################
###########                        Load Data                        #############
#################################################################################

# Set up a file path to the directory holding the txt files to be converted as 
# well as a file path to a directory to which the txt files will written
input_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_input\\" # w
output_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_output\\" # w

# Get a list of all long csv files (datatable) that need to be converted to wide txt files
files0 <- list.files(path=input_file_path)


#################################################################################
###########             Creating the aggregated data set            #############
#################################################################################

# Creating the data set to be evaluated ("agg_countries")
for (i in files0) {
  
  ## Setting the local directory to file
  locdir <- paste(input_file_path, i, sep = "")
  
  ## If the merged dataset does not already exist, create it
  if (!exists("agg_countries")) {
    agg_countries <- read.table(locdir, header = TRUE, sep = "\t")
  }
  ## If the merged dataset already exists, append to it 
  else {
    agg_countries_temp <- read.table(locdir, header = TRUE, sep = "\t")
    agg_countries <- rbind(agg_countries, agg_countries_temp)
    rm(agg_countries_temp)
  }
  rm(locdir)
}
print("Aggregate Countries has been created")
cat("\n")

# Save aggregated file to input file path
agg_saved <- paste(input_file_path , "agg_countries.csv" , sep = '')
write.csv2(agg_countries, agg_saved)
write.table(agg_countries, agg_saved, dec = "," , sep = "\t" , row.names = FALSE)
files <- list.files(path=input_file_path)
rm(agg_countries)
print("Aggregate Countries have been saved")
cat("\n")
cat("\n")

#################################################################################
###########                Reshape Data Long to Wide                #############
#################################################################################

# A function that does the csv long to csv wide file conversion
long_to_wide <- function(the_file_name){
    
    cat("Restructuring ", the_file_name, "\n")
    # Concatenation to make the sav and txt file paths that will be used for the 
    # reading and writing process that will be executed within the for loop below
    file_name <- paste(input_file_path , the_file_name , sep = '')
    file_saved <- paste(output_file_path , 'Long_' , the_file_name  , sep = '')
    
    # Read the data into R
    data_full <- read.csv(file_name, header = TRUE, sep = "\t")
    #print(names(data_full))
    
    # Select the varnames of the closed questions
    varnames_Q3 <- unique(grep("Q3", data_full$QUESTION_ID, value=TRUE))
    varnames_Q2c <- "Q2c"
    varnames_EMEVAL <- unique(grep("EMEVAL", data_full$QUESTION_ID, value=TRUE))
    varnames <- c(varnames_Q3, varnames_Q2c, varnames_EMEVAL)
    sort(varnames)
    
    # Keep only closed questions in the closed data set
    data_closed <- data_full[(data_full$QUESTION_ID %in% varnames), ]
    
    
    # Replacing "," by "."
    data_closed$WEIGHT_MARKETSHARE <- gsub(",",".",data_closed$WEIGHT_MARKETSHARE)
    data_closed$WEIGHT_FUNNEL <- gsub(",",".",data_closed$WEIGHT_FUNNEL)
    
    data_closed$FUNNELSTAGE_ID_VW <- gsub(",",".",data_closed$FUNNELSTAGE_ID_VW)
    data_closed$FUNNELSTAGE_ID_MAKE_ID <- gsub(",",".",data_closed$FUNNELSTAGE_ID_MAKE_ID)
    
    data_closed$ANSWER <- gsub(",",".",data_closed$ANSWER)
    
    
    # Define variable types of the data set
    data_closed$WEIGHT_MARKETSHARE <- as.numeric(data_closed$WEIGHT_MARKETSHARE)
    data_closed$WEIGHT_FUNNEL <- as.numeric(data_closed$WEIGHT_FUNNEL)
    
    data_closed$FUNNELSTAGE_ID_VW <- as.integer(data_closed$FUNNELSTAGE_ID_VW)
    data_closed$FUNNELSTAGE_ID_MAKE_ID <- as.integer(data_closed$FUNNELSTAGE_ID_MAKE_ID)
    
    data_closed$ANSWER <- as.numeric(data_closed$ANSWER)
    
    data_closed$COUNTRY_ID <- factor(data_closed$COUNTRY_ID)
    
    data_closed$ANSWER[data_closed$ANSWER == "99"] <- NaN
    
    data_closed$name <- paste(data_closed$CASE_ID, data_closed$OBJECT_ID, sep = "")
    
    # Reshape data_closed from long to wide
    data_closed_wide <- dcast(data_closed, CASE_ID + OBJECT_ID + WEIGHT_MARKETSHARE + WEIGHT_FUNNEL + FUNNELSTAGE_ID_VW + FUNNELSTAGE_ID_MAKE_ID + COUNTRY_ID ~ QUESTION_ID, value.var="ANSWER")
    
    # Write the data to a csv filede
    write.csv2(data_closed_wide, file_saved)
    cat(the_file_name, " has been saved." , "\n")
    cat("\n")
}

# A for loop which executes the sav_to_txt converter function for each country 
for (csv_file in files) {
  long_to_wide(csv_file)
}

# Free the imported libraries from memory
detach("package:foreign")
#detach("package:plyr")
detach("package:dplyr")
detach("package:reshape2")
rm(agg_saved, csv_file, files, files0, i, input_file_path, output_file_path , long_to_wide)

################################################################################################################
# Paste the code below into r studio. Ensure correct file path is used.
# source("C:\\R\\NBN_ad_hoc_analyses\\R_scripts\\150903 NBN_ad_hoc_analyses.r")
################################################################################################################
