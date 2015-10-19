#################################################################################
##########################  Significance Analysis  ##############################
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
files <- files[1]

locdir <- paste(input_file_path, files, sep = "")

data <- read.csv(locdir, header = TRUE, sep = "\t" , dec = ",")


#################################################################################
############                       ANALYSIS                       ###############
#################################################################################

files %>% group_by()

data$ident <- NA
data$ident[data$P.Value < 0.05] <- 1
data$ident[data$P.Value >= 0.05] <- 0

data %>% group_by(Object , t , FunnelStep) %>% summarise(avg = mean(ident))

test <- data %>% group_by(Object , t , FunnelStep , ConSelection , Country) %>% summarise(avg = mean(ident))


#################################################################################
############                Calculating Effect Sizes              ###############
#################################################################################

data$Cohens_h <- 2 * abs((asin((data$Group1_TOP/(data$Group1_TOP + data$Group1_BOTTOM))^0.5) - asin((data$Group2_TOP/(data$Group2_TOP + data$Group2_BOTTOM))^0.5)))

data$Corr <- abs(data$Group1_TOP/(data$Group1_TOP + data$Group1_BOTTOM) - data$Group2_TOP/(data$Group2_TOP + data$Group2_BOTTOM))

data$Phi <- (data$Statistic/(data$Group1_TOP + data$Group1_BOTTOM + data$Group2_TOP + data$Group2_BOTTOM)^0.5)

data$Phi_check <- abs(((data$Group1_TOP * data$Group2_BOTTOM) - (data$Group2_TOP * data$Group1_BOTTOM))/((data$Group1_TOP * data$Group2_BOTTOM * data$Group2_TOP * data$Group1_BOTTOM)^0.5))