#################################################################################
############################### FS Specification ###############################
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
input_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_output\\" # w
datatable_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_input\\" # w
output_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_FSnew\\" # w


# Get a list of all long csv files (datatable) that need to be converted to wide txt files
files <- list.files(path=datatable_file_path)
files_wide <- list.files(path=input_file_path)

# A function that changes the Consideration Brands from Top3 to Top5
FS_NEW <- function(the_file_name , i = 5) {

    file_name <- paste(datatable_file_path , the_file_name , sep = '')
    
    df.F_answers <- read.table(file_name, header = TRUE, sep = "\t" , dec = ",")
    df.F_answers$ANSWER <- as.numeric(gsub(",0" , "" , df.F_answers$ANSWER))
    
    cat("Creating new consideration funnel steps for " , the_file_name, "." , "\n" , sep = "")
    cat("Consideration Cut-off value: Top " , i , sep = "")
    cat("\n")
    
    # Creating the lookup-data set df.1
    df.1 <- df.F_answers[(df.F_answers$QUESTION_ID == "Q2b"), ]
    levels(droplevels(df.1$QUESTION_ID))
    df.1$TABLECREATIONDATE <- NULL
    
    df.1$FUNNELSTAGE_ID_MAKE_ID[df.1$FUNNELSTAGE_ID_MAKE_ID >= 1] <- 1
    df.1$FUNNELSTAGE_ID_VW[df.1$FUNNELSTAGE_ID_VW >= 1] <- 1
    
    df.1$vlookup_id <- paste(df.1$CASE_ID, df.1$OBJECT_ID, sep = "_")
    col_idx <- grep("vlookup_id", names(df.1))
    df.1 <- df.1[, c(col_idx, (1:ncol(df.1))[-col_idx])]
    
    
    # 
    g <- df.F_answers
    g$vlookup_id <- paste(g$CASE_ID, g$OBJECT_ID, sep = "_") 
    x <- g[grep("Consideration_Make_" , df.F_answers$QUESTION_ID) , ] 
    y <- g[grep("First_Choice_Make" , df.F_answers$QUESTION_ID) , ]
    
    df.2 <- rbind(x, y)
    rm(x , y , g , df.F_answers)
    
    df.2$OBJECT_ID <- paste("10", df.2$ANSWER, sep = "_")
    df.2$vlookup_id <- paste(df.2$CASE_ID, df.2$OBJECT_ID, sep = "_")
    levels(droplevels(df.2$QUESTION_ID))
    df.2$TABLECREATIONDATE <- NULL
    
    df.2$vlookup_id <- paste(df.2$CASE_ID, df.2$OBJECT_ID, sep = "_")
    col_idx <- grep("vlookup_id", names(df.2))
    df.2 <- df.2[, c(col_idx, (1:ncol(df.2))[-col_idx])]
    
    df.2 <- df.2[ , c("vlookup_id" , "QUESTION_ID") ]
    df.2$Consid <- df.2$QUESTION_ID
    df.2$QUESTION_ID <- NULL
    
    rm(col_idx)
    
    
    # 
    df.1 <- join(df.1, df.2, by=c('vlookup_id'))
    
    conselect<- c(levels(droplevels(df.2$Consid)))
    conselect <- conselect[conselect != "First_Choice_Make"]
    conselect <- sort(conselect)
    conselect <- conselect[1:i]
    
    rm(df.2)
    
    df.1$FUNNELSTAGE_ID_MAKE_ID[df.1$Consid %in% conselect] <- 2
    df.1$FUNNELSTAGE_ID_MAKE_ID[df.1$Consid == "First_Choice_Make"] <- 3
    
    df.1 <- df.1[ , c("vlookup_id" , "FUNNELSTAGE_ID_MAKE_ID") ]
    
    df.1$FUNNELSTAGE_ID_MAKE_ID_NEW <- df.1$FUNNELSTAGE_ID_MAKE_ID
    df.1$FUNNELSTAGE_ID_MAKE_ID <- NULL
    
    
    file_name <- paste(input_file_path , "Wide_" , the_file_name , sep = '')
    
    df.3 <- read.csv(file_name, header = TRUE, sep = "\t" , dec = ",")
    df.3$vlookup_id <- paste(df.3$CASE_ID, df.3$OBJECT_ID, sep = "_")
    
    col_idx <- grep("vlookup_id", names(df.3))
    df.3 <- df.3[, c(col_idx, (1:ncol(df.3))[-col_idx])]
    rm(col_idx)
    
    
    df.3 <- join(df.3, df.1, by=c('vlookup_id'))
    df.3$FUNNELSTAGE_ID_MAKE_ID <- df.3$FUNNELSTAGE_ID_MAKE_ID_NEW
    df.3$FUNNELSTAGE_ID_MAKE_ID_NEW <- NULL
    
    df.3$ConSelection <- i
    
    col_idx <- grep("ConSelection", names(df.3))
    df.3 <- df.3[, c(col_idx, (1:ncol(df.3))[-col_idx])]
    rm(col_idx)
    
    # write.csv2(data_closed_wide, file_saved)
    file_saved <- paste(output_file_path , "CON_top" , i , "_Wide_", the_file_name , sep = '')
    write.table(df.3, file_saved, dec = "," , sep = "\t" , row.names = FALSE)
    cat("CON_top" , i , "_Wide_" , the_file_name, " has been saved." , sep = "")
    cat("\n")
    cat("\n")
}

# A for loop which executes the sav_to_txt converter function for each country 
for (csv_file in files) {
  FS_NEW(csv_file , 1)
  FS_NEW(csv_file , 2)
  FS_NEW(csv_file , 3)
  FS_NEW(csv_file , 4)
  FS_NEW(csv_file , 5)
}


# Free the imported libraries from memory
detach("package:foreign")
detach("package:plyr")
detach("package:dplyr")
detach("package:reshape2")
rm(input_file_path , datatable_file_path , output_file_path , csv_file , files , files_wide)
  
################################################################################################################
# Paste the code below into r studio. Ensure correct file path is used.
# source("C:\\R\\NBN_ad_hoc_analyses\\R_scripts\\150915 NBN_FS_specification_test_22.r")
################################################################################################################
  