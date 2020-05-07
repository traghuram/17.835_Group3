setwd("C:/Users/Taran/Documents/Personal/Courses/MIT Final Pset")
#install.packages("httr")
#install.packages("jsonlite")
library("httr")
library("readxl")
library("jsonlite")
library(dplyr)





################# Import the basic company data #####################

# Import file
Data.company_info <- read.csv("BasicCompanyDataAsOneFile-2020-04-01.csv", header=TRUE)

# Define columns to keep
keep.columns.company <- colnames(Data.company_info)[c(1:33)]

# Keep those columns
Data.company_info <- Data.company_info[,keep.columns.company]






###################### Merging basic company data with Open Tender Data ###################


## Read in all files (condense code later) - create a folder in your working directory and paste the files in there
digiwhist_2009 <- read.csv("digiwhist_csv/data-uk-2009.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2010 <- read.csv("digiwhist_csv/data-uk-2010.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2011 <- read.csv("digiwhist_csv/data-uk-2011.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2012 <- read.csv("digiwhist_csv/data-uk-2012.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2013 <- read.csv("digiwhist_csv/data-uk-2013.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2014 <- read.csv("digiwhist_csv/data-uk-2014.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2015 <- read.csv("digiwhist_csv/data-uk-2015.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2016 <- read.csv("digiwhist_csv/data-uk-2016.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2017 <- read.csv("digiwhist_csv/data-uk-2017.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2018 <- read.csv("digiwhist_csv/data-uk-2018.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2019 <- read.csv("digiwhist_csv/data-uk-2019.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_200x <- read.csv("digiwhist_csv/data-uk-200x.csv", sep = ";", stringsAsFactors = FALSE)

digiwhist_all <- rbind(digiwhist_2009,
      digiwhist_2010,
      digiwhist_2011,
      digiwhist_2012,
      digiwhist_2013,
      digiwhist_2014,
      digiwhist_2015,
      digiwhist_2016,
      digiwhist_2017,
      digiwhist_2018,
      digiwhist_2019,
      digiwhist_200x)

rm(digiwhist_2009,
      digiwhist_2010,
      digiwhist_2011,
      digiwhist_2012,
      digiwhist_2013,
      digiwhist_2014,
      digiwhist_2015,
      digiwhist_2016,
      digiwhist_2017,
      digiwhist_2018,
      digiwhist_2019,
      digiwhist_200x)


### Pull in and Merge digiwhist data

# Create unique list of names and countries
digiwhist_all_names <- data.frame(unique(digiwhist_all[,c("bidder_name", "bidder_country")]))
names(digiwhist_all_names) <- c("unique_bidder_name", "bidder_country")


### Match companies in basic company file to winners

# Extract names from Basic company data
Data.company_info.names <- data.frame(Data.company_info[,c("CompanyName", "CompanyNumber")])

# Remove quotes from both files
Data.company_info.names$CompanyNameClean <- gsub('"', "", Data.company_info.names$CompanyName)
digiwhist_all_names$clean <- gsub('"', "", digiwhist_all_names$unique_bidder_name)

# Make strings lower case
Data.company_info.names$CompanyNameClean <- tolower(Data.company_info.names$CompanyNameClean)
digiwhist_all_names$clean <- tolower(digiwhist_all_names$clean)

# Remove ltd and limited and trim
Data.company_info.names$CompanyNameClean <- gsub(" ltd","",Data.company_info.names$CompanyNameClean)
Data.company_info.names$CompanyNameClean <- gsub(" limited","",Data.company_info.names$CompanyNameClean)
Data.company_info.names$CompanyNameClean <- trimws(Data.company_info.names$CompanyNameClean)

digiwhist_all_names$clean <- gsub(" ltd","",digiwhist_all_names$clean)
digiwhist_all_names$clean <- gsub(" limited","",digiwhist_all_names$clean)
digiwhist_all_names$clean <- trimws(digiwhist_all_names$clean)

# Match and check match %
merged_all <- merge(digiwhist_all_names, Data.company_info.names, by.x = "clean", by.y = "CompanyNameClean", all.x = TRUE)
1 - sum(is.na(merged_all$CompanyNumber))/length(merged_all$CompanyNumber)


# Remove pvt, llp (not private since that's a real word) and trim
Data.company_info.names$CompanyNameClean <- gsub(" llp","",Data.company_info.names$CompanyNameClean)
Data.company_info.names$CompanyNameClean <- gsub(" pvt","",Data.company_info.names$CompanyNameClean)
Data.company_info.names$CompanyNameClean <- trimws(Data.company_info.names$CompanyNameClean)

digiwhist_all_names$clean <- gsub(" llp","",digiwhist_all_names$clean)
digiwhist_all_names$clean <- gsub(" pvt","",digiwhist_all_names$clean)
digiwhist_all_names$clean <- trimws(digiwhist_all_names$clean)


# Check match % again
merged_all <- merge(digiwhist_all_names, Data.company_info.names, by.x = "clean", by.y = "CompanyNameClean", all.x = TRUE)
1 - sum(is.na(merged_all$CompanyNumber))/length(merged_all$CompanyNumber)

# Check match % among UK bidders
1 - sum((merged_all$bidder_country == "UK" | merged_all$bidder_country == "GB") & is.na(merged_all$CompanyNumber))/
  sum(merged_all$bidder_country == "UK" | merged_all$bidder_country == "GB")


# Remove any identifiers in parens or stem beginning (just for remaining) - to do if time


## Checks on merge

# Any many:1 matches? How many are unconsolidated rows (multiple company numbers across rows? Likely)

# Checking rows for certain companies
#charmatch("selfr",Data.company_info.names$CompanyNameClean)
#Data.company_info.names[grep("selfr", Data.company_info.names$CompanyNameClean),]



### Create final open tender dataset - one line per tender, with all company attributes

# Identify basic data columns needed for merge
keep.columns.company <- colnames(Data.company_info)[c(1:15, 27,33)]


# Bring company numbers from merged file into full open tender dataset
digiwhist_all <- merge(digiwhist_all, merged_all, by.x = "bidder_name", by.y = "unique_bidder_name", all.x = TRUE)

# Bring basic data columns into open tender dataset
merged_basic_opentender <- merge(digiwhist_all, Data.company_info[,keep.columns.company],
                                 by.x = "CompanyNumber", by.y = "CompanyNumber", all.x = TRUE)








####################### Merging PSC and basic companies datasets ############################

json_dframe_1 <- stream_in(file("psc-snapshot-2020-04-19_1of16.txt")) #
json_dframe_2 <- stream_in(file("psc-snapshot-2020-04-19_2of16.txt")) #
json_dframe_3 <- stream_in(file("psc-snapshot-2020-04-19_3of16.txt")) #
json_dframe_4 <- stream_in(file("psc-snapshot-2020-04-19_4of16.txt")) #
json_dframe_5 <- stream_in(file("psc-snapshot-2020-04-19_5of16.txt")) #
json_dframe_6 <- stream_in(file("psc-snapshot-2020-04-19_6of16.txt")) #
json_dframe_7 <- stream_in(file("psc-snapshot-2020-04-19_7of16.txt")) #
json_dframe_8 <- stream_in(file("psc-snapshot-2020-04-19_8of16.txt")) #
json_dframe_9 <- stream_in(file("psc-snapshot-2020-04-19_9of16.txt")) #
json_dframe_10 <- stream_in(file("psc-snapshot-2020-04-19_10of16.txt")) #
json_dframe_11 <- stream_in(file("psc-snapshot-2020-04-19_11of16.txt")) #
json_dframe_12 <- stream_in(file("psc-snapshot-2020-04-19_12of16.txt")) #
json_dframe_13 <- stream_in(file("psc-snapshot-2020-04-19_13of16.txt")) #
json_dframe_14 <- stream_in(file("psc-snapshot-2020-04-19_14of16.txt")) #
json_dframe_15 <- stream_in(file("psc-snapshot-2020-04-19_15of16.txt")) #
json_dframe_16 <- stream_in(file("psc-snapshot-2020-04-19_16of16.txt")) #

# Flatten the JSON files
json_dframe_1 <- flatten(json_dframe_1, recursive = T)
json_dframe_2 <- flatten(json_dframe_2, recursive = T)
json_dframe_3 <- flatten(json_dframe_3, recursive = T)
json_dframe_4 <- flatten(json_dframe_4, recursive = T)
json_dframe_5 <- flatten(json_dframe_5, recursive = T)
json_dframe_6 <- flatten(json_dframe_6, recursive = T)
json_dframe_7 <- flatten(json_dframe_7, recursive = T)
json_dframe_8 <- flatten(json_dframe_8, recursive = T)
json_dframe_9 <- flatten(json_dframe_9, recursive = T)
json_dframe_10 <- flatten(json_dframe_10, recursive = T)
json_dframe_11 <- flatten(json_dframe_11, recursive = T)
json_dframe_12 <- flatten(json_dframe_12, recursive = T)
json_dframe_13 <- flatten(json_dframe_13, recursive = T)
json_dframe_14 <- flatten(json_dframe_14, recursive = T)
json_dframe_15 <- flatten(json_dframe_15, recursive = T)
json_dframe_16 <- flatten(json_dframe_16, recursive = T)


# Choose columns to retain in merge and Add non-existent columns to 16th file
keep.columns <- colnames(json_dframe_1)[c(1,3,5:8, 10:17,19:20, 23:25)]

json_dframe_16[keep.columns] <- NA

# Bind dataframes together
json_all_dframe <- rbind(json_dframe_1[,keep.columns],json_dframe_2[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_3[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_4[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_5[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_6[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_7[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_8[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_9[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_10[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_11[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_12[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_13[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_14[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_15[,keep.columns])
json_all_dframe <- rbind(json_all_dframe[,keep.columns],json_dframe_16[,keep.columns])

# Remove individual dataframes
rm(list=rm(list = ls(pattern = "^json_dframe")))

# Export a list of company numbers and names to have for later
write.csv(json_all_dframe[,c(1,3,4)],"PSC_companies_ppl.csv", row.names = FALSE)






########################### Merge PSC and basic company data ################################

## Merge with other data

# Set seed to make partition reproducable
# set.seed(02139)

# Create a list of sample rows and test merge
# test_rows <- sample(nrow(json_all_dframe), 1000, replace = FALSE)
# test <- merge(json_all_dframe[test_rows,], Data.company_info, by.x = "company_number", by.y = "CompanyNumber")
# View(head(test))
# rm(test)


## Just Merge a few columns
Data.company_info <- Data.company_info[,c(1,2,9,27)]
json_all_dframe <- json_all_dframe[,c(1:6, 16)]


# Actual merge
Data.PSC.merged <- merge(json_all_dframe, Data.company_info, by.x = "company_number", by.y = "CompanyNumber", all.x = TRUE)
# Note - this does not pull in PSCs for companies not in the companies file - not enough memory for operation


# Remove the excess files
rm(json_all_dframe)
rm(Data.company_info)

## Export a file for later
write.csv(Data.PSC.merged[,c(1:5,7:10)],"PSC_Merged_Data.csv", row.names = FALSE)





########################YOU NOW HAVE A MERGED PSC FILE!!! ##################################


### Cheat for next few steps - just pull in the PSC merged csv from your computer and skip the above

Data.PSC.merged <- read.csv("PSC_Merged_Data.csv")







######### Trying to analyze these datasets ################





## Check - how many companies in free product but not in PSC file?


### Stats on merged dataset

## Counts of rows

# How many unique company ids in orignial dataset?
PSC.unique_companies <- length(unique(json_all_dframe$company_number))

# How many unique company ids in merged dataset?
Product.unique_companies <- length(unique(Data.company_info$CompanyNumber))

# How many companies matched to PSC file? - about 77% of PSC-company rows
Product.unique_companies/PSC.unique_companies


## Anything specific about the companies that didn't match that might matter?

# Create a crosswalk of country names
#write.csv(table(Data.company_info$RegAddress.Country), "country crosswalk companies.csv")
table(Data.company_info$RegAddress.Country)
# At least 99.8% of countries are registered within the UK - but there are some that are not

# Locations of companies in original PSC data
head(table(json_all_dframe$data.address.country))

# Locations of companies in merged file



## Distribution of companies

# How many PSC rows are people vs other entities?
100*table(json_all_dframe$data.kind)/length(json_all_dframe$company_number)

# How many merged rows are people vs other entities?
100*table(Data.PSC.merged$data.kind)/length(Data.PSC.merged$company_number)


## Industries of companies

# How many companies are there for each of the industry designations?
#length(unique(Data.PSC.merged[,c("SICCode.SicText_1", "company_number")]))

# list of unique industry codes in file
length(unique(Data.PSC.merged$SICCode.SicText_1))
length(unique(Data.PSC.merged$company_number))
length(unique(Data.company_info$CompanyNumber)) # There are some companies not in PSC file (~300k)

table.industry_code <- table(Data.PSC.merged$SICCode.SicText_1)/length(Data.PSC.merged$company_number)
# Figure out how to sort this table

# There are 1180 unique industry codes in the data file
# The top 40 industries make up 60% of company-person rows in the file (other 1140 make up 40%)
# Some of the top industries are unknown or vague ("Other business support", "Dormany Company", etc...)





