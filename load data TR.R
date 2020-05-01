setwd("C:/Users/Taran/Documents/Personal/Courses/MIT Final Pset")
#install.packages("httr")
#install.packages("jsonlite")
library("httr")
library("readxl")
library("jsonlite")



######### Merging PSC and companies house datasets ##############

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

# Test the files out
# View(head(json_dframe_2))
# colnames(json_dframe_2)
# colnames(json_dframe_1)
# dim(json_dframe_2)

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

########################YOU NOW HAVE A MERGED PSC FILE!!! ##################################


## Run some stats

# Check out data set
# dim(json_all_dframe)
# View(head(json_all_dframe))

# Run some stats
View(unique(json_all_dframe$data.notified_on))
View(unique(json_all_dframe$data.natures_of_control))


### Import the company data and merge

# Import file
Data.company_info <- read.csv("BasicCompanyDataAsOneFile-2020-04-01.csv", header=TRUE)

# Define columns to keep
keep.columns.company <- colnames(Data.company_info)[c(1:33)]

# Keep those columns
Data.company_info <- Data.company_info[,keep.columns.company]


## Merge with other data

# Set seed to make partition reproducable
# set.seed(02139)

# Create a list of sample rows and test merge
# test_rows <- sample(nrow(json_all_dframe), 1000, replace = FALSE)
# test <- merge(json_all_dframe[test_rows,], Data.company_info, by.x = "company_number", by.y = "CompanyNumber")
# View(head(test))
# rm(test)

# Actual merge
Data.PSC.merged <- merge(json_all_dframe, Data.company_info, by.x = "company_number", by.y = "CompanyNumber", all.x = TRUE)
# Note - this does not pull in PSCs for companies not in the companies file - not enough memory for operation

## Check - how many companies in free product but not in PSC file?


### Stats on merged dataset

## Counts of rows

# How many unique company ids in orignial dataset?
PSC.unique_companies <- length(unique(json_all_dframe$company_number))

# How many unique company ids in merged dataset?
Merge.unique_companies <- length(unique(Data.PSC.merged$company_number))

# How many companies matched to PSC file? - about 77% of PSC-company rows
Merge.unique_companies/PSC.unique_companies


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





######## Examining Open Contracts Data ###################3

validate("UK_ocds_data.json")
f <- file("UK_ocds_data.json")
ndjson::stream_in("UK_ocds_data.json")

