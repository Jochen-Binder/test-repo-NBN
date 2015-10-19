#################################################################################
########################### Calculating Significances ###########################
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
input_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_tX\\" # w
output_file_path <- "C:\\R\\NBN_ad_hoc_analyses\\csv_sig\\" # w

# Get a list of all long csv files (datatable) that need to be converted to wide txt files
files <- list.files(path=input_file_path)


#################################################################################
######                       Statistical Testing                          #######
#################################################################################

sig <- data.frame(Var_Name = character(), Statistic = numeric(), df = numeric(), P.Value = numeric(), FunnelStep = character(), ConSelection = factor (), Country = factor())


Chi2_testing <- function(the_file_name , OBJECT = "CC" , FWEIGHT = "ON"){ 
  

  cat("Calculating split Significances for", the_file_name, "\n")
  # Concatenation to make the sav and txt file paths that will be used for the 
  # reading and writing process that will be executed within the for loop below
  file_name <- paste(input_file_path , the_file_name , sep = '')
  
  
  if (OBJECT == "CC") {
    cat("Significance testing is calculated for all core competitor brands" , "\n")
    
    # Read the data into R
    data_sig <- read.csv(file_name, header = TRUE, sep = "\t" , dec = ",")
    object <- "CC"
  }
  
  else if (OBJECT == "VW"){
    cat("Significance testing is caculated for Volkswagen only." , "\n")
    # Read the data into R
    data_sig <- read.csv(file_name, header = TRUE, sep = "\t" , dec = ",")
    data_sig <- data_sig[data_sig$OBJECT_ID == "10_44" , ]
    object <- "VW"
    F_Weight <- "F_Weight"
  }
  
  if (FWEIGHT == "ON") {
    # Nothing is being done
    F_Weight <- "F_Weight"
  }
  
  else if (FWEIGHT == "OFF"){
    cat("Warning: Funnel Weights turned off." , "\n")
    # Set all Funnel Weights equal to "1"
    data_sig$WEIGHT_FUNNEL <- 1
    F_Weight <- ""
  }
  
  file_saved <- paste(output_file_path , 'SigTest_' , object , "_" , the_file_name  , sep = '')
  
  # varnames <- grep("t1", names(data_sig), value=TRUE)
  countries <- c(levels(factor(data_sig$COUNTRY_ID)))#, "ALL")  
  topbox <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9")
  funnelstep <- c("CON", "FC")
  ConSelection <- c(levels(factor(data_sig$ConSelection)))#, "ALL") 
  

  for (k in funnelstep) {
  for (c in countries){
  for (t in topbox) {
  # if(c != "ALL") data_sig <- data_sig[data_sig$COUNTRY_ID == c, ]
  # else data_sig <- data_I
  varnames <- grep(t, names(data_sig), value=TRUE)
  for (i in varnames) {
  if(k == "CON") {
    sum_0_1 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="0" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 1], na.rm = TRUE), digits = 0)
    sum_1_1 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="1" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 1], na.rm = TRUE), digits = 0)
    sum_0_2 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="0" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 2], na.rm = TRUE), digits = 0)
    sum_1_2 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="1" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 2], na.rm = TRUE), digits = 0)
    one <- c(sum_1_1, sum_1_2)
    two <- c(sum_0_1, sum_0_2)
    m <- c(one, two)
    input <- matrix(m, ncol=2, byrow = FALSE)
    colnames(input) <- c("1","0")
    rownames(input) <- c("Group1", "Group2")
  }
  else {
    sum_0_2 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="0" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 2], na.rm = TRUE), digits = 0)
    sum_1_2 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="1" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 2], na.rm = TRUE), digits = 0)
    sum_0_3 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="0" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 3], na.rm = TRUE), digits = 0)
    sum_1_3 <- round(sum(data_sig$WEIGHT_FUNNEL[data_sig[[i]]=="1" & data_sig$FUNNELSTAGE_ID_MAKE_ID == 3], na.rm = TRUE), digits = 0)
    one <- c(sum_1_2, sum_1_3)
    two <- c(sum_0_2, sum_0_3)
    m <- c(one, two)
    input <- matrix(m, ncol=2, byrow = FALSE)
    colnames(input) <- c("1","0")
    rownames(input) <- c("Group1", "Group2")
  }
  
  
  Chi2 <- tryCatch({
                      prop.test(input, correct = TRUE)
                    }, error = function(e) {
                      statistic <- NA
                      parameter <- NA
                      p.value <- NA
                      Chi2 <- data.frame(statistic, parameter, p.value) 
                    }, silent = T)
  
  Statistic <- as.numeric(Chi2$statistic)
  Statistic <- data.frame(Statistic)
  df <- Chi2$parameter
  df <- data.frame(df)
  P.Value <- as.numeric(Chi2$p.value)
  P.Value <- data.frame(P.Value)
  
  
  if(k == "CON"){
  FunnelStep <- "CON"
  FunnelStep <- data.frame(FunnelStep)
  } else {
  FunnelStep <- "FC"
  FunnelStep <- data.frame(FunnelStep)
  }
  
  Var_Name <- gsub(paste("_" , t , sep = "") , "" , i)
  Var_Name <- data.frame(Var_Name)
  
  TopX <- t
  TopX <- data.frame(TopX)
  
  Country <- c
  Country <- data.frame(Country)
  
  ConSelection <- data.frame(ConSelection)
  
  Group1_TOP <- input["Group1", "1"]
  Group1_TOP <- data.frame(Group1_TOP)
  
  Group1_BOTTOM <- input["Group1", "0"]
  Group1_BOTTOM <- data.frame(Group1_BOTTOM)
  
  Group2_TOP <- input["Group2", "1"]
  Group2_TOP <- data.frame(Group2_TOP)
  
  Group2_BOTTOM <- input["Group2", "0"]
  Group2_BOTTOM <- data.frame(Group2_BOTTOM)
  
  Object <- object
  Object <- data.frame(Object)
  
  F_Weight <- data.frame(F_Weight)
  
  sig <- rbind(sig, data.frame(Var_Name , F_Weight , Object , t , Group1_TOP , Group1_BOTTOM , Group2_TOP , Group2_BOTTOM , Statistic , df , P.Value, ConSelection , FunnelStep, Country))
  }}}}

  cat("Saving", the_file_name, "\n")
  
  write.csv2(sig, file_saved)
  cat(the_file_name, "has been saved" , "\n")
  cat("\n")
}

# A for loop which executes the sav_to_txt converter function for each country 
for (csv_file in files) {
  Chi2_testing(csv_file , "CC")
}

for (csv_file in files) {
  Chi2_testing(csv_file , "VW")
}

# Free the imported libraries from memorywa
detach("package:foreign")
detach("package:plyr")
detach("package:dplyr")
detach("package:reshape2")
rm(files , input_file_path , output_file_path , csv_file, sig)

################################################################################################################
# Paste the code below into r studio. Ensure correct file path is used.
# source("C:\\R\\NBN_ad_hoc_analyses\\R_scripts\\150922 NBN_sinificance_testing.r")
################################################################################################################