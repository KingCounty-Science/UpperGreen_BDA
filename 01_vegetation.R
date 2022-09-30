#=== === === === === === === ===
# Started September 30, 2022
# Rebekah Stiling, rstiling@kingcounty.gov
# This script explores a preliminary set of vegetation data
#=== === === === === === === ===

# Load relevant packages
library(tidyverse)  # for data wrangling, analysis, and plotting
library(readxl) #To read the excel file


# Review the sheet names in order to select the correct one.  
excel_sheets("data/Vegetation Monitoring Data_testcopy.xlsx")

# read in the worksheet with the isotope data
spcover <- read_excel("data/Vegetation Monitoring Data_testcopy.xlsx", sheet = "DATA_SPCOVER")

head(spcover) #check info.

# Get some summary values