#################################################################################
############################# Calculating TopX Values ###########################
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
input_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_FSnew\\" # w
output_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_tX\\" # w

# Get a list of all long csv files (datatable) that need to be converted to wide txt files
files <- list.files(path=input_file_path)

## A function to calculate topX values
tX_values <- function(the_file_name){ 
  
  cat("Calculating topX values (top1 to top9) for ", the_file_name, "\n")
  # Concatenation to make the sav and txt file paths that will be used for the 
  # reading and writing process that will be executed within the for loop below
  file_name <- paste(input_file_path , the_file_name , sep = '')
  file_saved <- paste(output_file_path , 'TopX_' , the_file_name  , sep = '')
  
  # Read the data into R
  data_tX <- read.csv(file_name, header = TRUE, sep = "\t" , dec = ",")
  #print(names(data_full))
  
  # Select the variables for which the tX values are calculated
  neglect <- c("CASE_ID", "OBJECT_ID", "WEIGHT_MARKETSHARE", "WEIGHT_FUNNEL", "FUNNELSTAGE_ID_VW", "FUNNELSTAGE_ID_MAKE_ID", "COUNTRY_ID" , "vlookup_id" , "ConSelection")
  variables <- names(data_tX)
  variables <- variables[!variables %in% neglect]

  # Calculate the tX values
  for (i in variables) {
    data_tX[[paste0(i, "_t1")]] <- ifelse(data_tX[[i]] > 9, c("1"), c("0"))
    data_tX[[paste0(i, "_t2")]] <- ifelse(data_tX[[i]] > 8, c("1"), c("0"))
    data_tX[[paste0(i, "_t3")]] <- ifelse(data_tX[[i]] > 7, c("1"), c("0"))
    data_tX[[paste0(i, "_t4")]] <- ifelse(data_tX[[i]] > 6, c("1"), c("0"))
    data_tX[[paste0(i, "_t5")]] <- ifelse(data_tX[[i]] > 5, c("1"), c("0"))
    data_tX[[paste0(i, "_t6")]] <- ifelse(data_tX[[i]] > 4, c("1"), c("0"))
    data_tX[[paste0(i, "_t7")]] <- ifelse(data_tX[[i]] > 3, c("1"), c("0"))
    data_tX[[paste0(i, "_t8")]] <- ifelse(data_tX[[i]] > 2, c("1"), c("0"))
    data_tX[[paste0(i, "_t9")]] <- ifelse(data_tX[[i]] > 1, c("1"), c("0"))
  }

  # write.csv2(data_closed_wide, file_saved)
  write.table(data_tX, file_saved, dec = "," , sep = "\t" , row.names = FALSE)
  cat(the_file_name, " has been saved." , "\n")
  cat("\n")
}

# A for loop which executes the sav_to_txt converter function for each country 
for (csv_file in files) {
  tX_values(csv_file)
}

# Free the imported libraries from memory
detach("package:foreign")
detach("package:plyr")
detach("package:dplyr")
detach("package:reshape2")
rm(files , input_file_path , output_file_path , csv_file)
  
################################################################################################################
# Paste the code below into r studio. Ensure correct file path is used.
# source("C:\\R\\NBN_ad_hoc_analyses\\R_scripts\\150915 NBN_topX_Values.r")
################################################################################################################
  