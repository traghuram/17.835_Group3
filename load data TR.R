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
digiwhist_all_names <- data.frame(unique(digiwhist_all[,"bidder_name"]), stringsAsFactors = FALSE)
names(digiwhist_all_names) <- "unique_bidder_name"
# Not technically unique - they are unique strings but not unique unformatted - 
# Vast majority of names (>96%) are unique to a country - many of the rest are MNCs


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



### Merge and check match %
merged_all_names <- merge(digiwhist_all_names, Data.company_info.names, by.x = "clean", by.y = "CompanyNameClean", all.x = TRUE)
1 - sum(is.na(merged_all$CompanyNumber))/length(merged_all$CompanyNumber)



### Do another, better merge

## Remove pvt, llp (not private since that's a real word) and trim
Data.company_info.names$CompanyNameClean <- gsub(" llp","",Data.company_info.names$CompanyNameClean)
Data.company_info.names$CompanyNameClean <- gsub(" pvt","",Data.company_info.names$CompanyNameClean)
Data.company_info.names$CompanyNameClean <- trimws(Data.company_info.names$CompanyNameClean)

digiwhist_all_names$clean <- gsub(" llp","",digiwhist_all_names$clean)
digiwhist_all_names$clean <- gsub(" pvt","",digiwhist_all_names$clean)
digiwhist_all_names$clean <- trimws(digiwhist_all_names$clean)


# Check match % again
merged_all_names <- merge(digiwhist_all_names, Data.company_info.names, by.x = "clean", by.y = "CompanyNameClean", all.x = TRUE)
1 - sum(is.na(merged_all_names$CompanyNumber))/length(merged_all_names$CompanyNumber)



# Remove any identifiers in parens or stem beginning (just for remaining) - to do if time


## Checks on merge

# Any many:1 or 1:many matches? How many are unconsolidated rows (multiple company numbers across rows? Likely)
sum(aggregate(merged_all_names$unique_bidder_name, by = list(merged_all_names$unique_bidder_name), FUN = length)$x > 1)

# Number of rows after merge is 300 more because there are a few bidders that match 2 or 3 unique companies
# Need to fix that before submitting - but generally pretty good match (99.8%)! Can fix 300 by hand



# Checking rows for certain companies
#charmatch("selfr",Data.company_info.names$CompanyNameClean)
#Data.company_info.names[grep("selfr", Data.company_info.names$CompanyNameClean),]



### Create final open tender dataset - one line per tender, with all company attributes

# Identify basic data columns needed for merge
keep.columns.company <- colnames(Data.company_info)[c(1:15, 27,33)]


# Bring company numbers from merged file into full open tender dataset
digiwhist_all_merged <- merge(digiwhist_all, merged_all_names, by.x = "bidder_name", by.y = "unique_bidder_name", all.x = TRUE)

# Bring basic data columns into open tender dataset
merged_basic_opentender <- merge(digiwhist_all_merged, Data.company_info[,keep.columns.company],
                                 by.x = "CompanyNumber", by.y = "CompanyNumber", all.x = TRUE)


## Pull digiwhist into basic company file instead - COMMENT OUT IF NOT USING!
merged_basic_opentender_reversed <- merge(Data.company_info[,keep.columns.company], digiwhist_all_merged,
                                 by.x = "CompanyNumber", by.y = "CompanyNumber", all.x = TRUE)

# merged_basic_opentender_reversed$age <- 2020 - as.numeric(substring(merged_basic_opentender_reversed$IncorporationDate, 7, 10))
# merged_basic_opentender_reversed$awarded <- ifelse(merged_basic_opentender_reversed$lot_status == "AWARDED", 1, 0)
# aggregate(merged_basic_opentender_reversed$age, by = list(merged_basic_opentender_reversed$awarded), FUN = mean)
# write.csv(merged_basic_opentender_reversed, "merged_basic_opentender_reversed.csv")


##################### STOP HERE FOR A MERGED OPEN TENDER AND BASIC COMPANY FILE ########################







#################################### Open Tender Descriptive Analysis #########################################

## Create some subsets that are useful for analysis

# First, identify the unit of analysis - is this one row per bid on a tender, or something else?


# How many unique tenders?
length(unique(merged_basic_opentender$tender_id))
#View(aggregate(merged_basic_opentender$tender_id, by = list(merged_basic_opentender$tender_id), FUN = length))


# How many rows do we even have names for?
sum(is.na(merged_basic_opentender$bidder_name))
sum(merged_basic_opentender$bidder_name == "" & !is.na(merged_basic_opentender$bidder_name))



### Understanding the tenders that the UK govt has put out

# How many unique tenders over the last 10 years?
length(unique(merged_basic_opentender$tender_id))

## How many unique tender-price combos?
tender_prices <- group_by(merged_basic_opentender[,c("tender_id", "tender_finalPrice")], tender_id, tender_finalPrice)
tender_prices <- distinct(tender_prices)

mean(tender_prices$tender_finalPrice, na.rm = TRUE)
median(tender_prices$tender_finalPrice, na.rm = TRUE)
sum(tender_prices$tender_finalPrice, na.rm = TRUE)

hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & !is.na(merged_basic_opentender$tender_finalPrice),"tender_finalPrice"],
     main = "Histogram of tenders by price", xlab = "Tender Prices", cex = 0.5)

hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & !is.na(merged_basic_opentender$tender_finalPrice),"tender_finalPrice"],
     main = "Histogram of tenders by price", xlab = "Tender Prices", xlim = c(0,10^5),
     cex = 0.5, breaks = 1000000)


# How many unique tenders by year?
unique_tenders <- unique(merged_basic_opentender[,c("tender_id","tender_year")])
aggregate(unique_tenders$tender_id, by = list(unique_tenders$tender_year), FUN = length)

# How many unique tenders that were awarded by year?
unique_tenders_awarded <- unique(merged_basic_opentender[merged_basic_opentender$lot_status == "AWARDED"
                                                         & !is.na(merged_basic_opentender$bid_price),
                                                         c("tender_id",
                                                           "tender_year", 
                                                           "bid_price",
                                                           "tender_finalPrice",
                                                           "lot_status",
                                                           "secrecy_bidder")])

# Secrecy jurisdiction companies avg tender
aggregate(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 1, "tender_finalPrice"],
          by = list(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 1, "tender_year"]),
          FUN = mean, na.rm = TRUE)

# Other companies avg tender
aggregate(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 0, "tender_finalPrice"],
          by = list(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 0, "tender_year"]),
          FUN = mean, na.rm = TRUE)




### Understanding bids

# How many unique tender-price combos?
bid_prices <- group_by(merged_basic_opentender[,c("tender_id", "bid_price")], tender_id, bid_price)
bid_prices <- distinct(bid_prices)

mean(bid_prices$bid_price, na.rm = TRUE)
median(bid_prices$bid_price, na.rm = TRUE)
sum(bid_prices$bid_price, na.rm = TRUE)

hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & !is.na(merged_basic_opentender$bid_price),"bid_price"],
     main = "Histogram of bids by price", xlab = "Bid Prices", cex = 0.5)

hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & !is.na(merged_basic_opentender$bid_price),"bid_price"],
     main = "Histogram of bids by price", xlab = "Bid Prices", xlim = c(0,10^5),
     cex = 0.5, breaks = 1000000)

# Average bid by year
aggregate(unique_tenders_awarded$bid_price, by = list(unique_tenders_awarded$tender_year), FUN = mean)


## Bid information for secrecy companies

# Secrecy jurisdiction companies avg bids
aggregate(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 1, "bid_price"],
          by = list(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 1, "tender_year"]),
          FUN = mean)

# Other companies avg bids
aggregate(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 0, "bid_price"],
          by = list(unique_tenders_awarded[unique_tenders_awarded$secrecy_bidder == 0, "tender_year"]),
          FUN = mean)






## How many companies can we identify that have bid on contracts?

# Total unique bids
length(unique(merged_basic_opentender$bidder_id))

# Total unique bids tied to a company in basic companies dataset
dim(unique(merged_basic_opentender[,c("bidder_id", "CompanyNumber")]))[1]

# Total unique bids on awarded contracts tied to a company in basic companies dataset
dim(unique(merged_basic_opentender[merged_basic_opentender$lot_status == "AWARDED",
                                   c("bidder_id", "CompanyNumber")]))[1]

# Total unique bids on unawarded contracts tied to a company in basic companies dataset
dim(unique(merged_basic_opentender[merged_basic_opentender$lot_status != "AWARDED",
                                   c("bidder_id", "CompanyNumber")]))[1]


# Total unique bidders - 167k
length(unique(merged_basic_opentender$bidder_name))

# Total unique bidders tied to a company in basic companies dataset - 46k
length(unique(merged_basic_opentender$CompanyNumber))

# Total unique bidders on awarded contracts tied to a company in basic companies dataset - 46k
length(unique(merged_basic_opentender[merged_basic_opentender$lot_status == "AWARDED","CompanyNumber"]))

# Total unique bidders on unawarded contracts tied to a company in basic companies dataset - 166
length(unique(merged_basic_opentender[merged_basic_opentender$lot_status != "AWARDED","CompanyNumber"]))

# Total unique winners of bids on awarded contracts tied to a company in basic companies dataset - 46k
length(unique(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes","CompanyNumber"]))


## What does the distribution of bids look like?

# Total bids on tenders by lot status
aggregate(merged_basic_opentender$lot_status, by = list(merged_basic_opentender$lot_status), FUN = length)

# Average bid price by lot status
aggregate(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"],
          by = list(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "lot_status"]), FUN = mean)

# Distribution of winning bid prices
# Smaller bids
hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & merged_basic_opentender$bid_price<1000000
                             & !is.na(merged_basic_opentender$bid_price),"bid_price"],
     main = "Frequency of winning bids by price (<1,000,000 Euros", xlab = "Winning Bid",
     cex = 0.5)

# Larger bids
hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & merged_basic_opentender$bid_price>100000000
                             & !is.na(merged_basic_opentender$bid_price),"bid_price"],
     main = "Frequency of winning bids by price (>100,000,000 Euros", xlab = "Winning Bid",
     cex = 0.5)

# What have been the largest winning bids in these records?
# View(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
#                              & merged_basic_opentender$bid_price>100000000 
#                              & !is.na(merged_basic_opentender$bid_price),])



###### Who has won the largest bids in these records, and how large have they been?
View(head(merged_basic_opentender[order(-merged_basic_opentender$bid_price),
                                  c("bidder_name","buyer_name", "bid_price")],20))

###### Who has won multiple contracts?
df <- aggregate(merged_basic_opentender$tender_id,by = list(merged_basic_opentender$bidder_name), FUN = length)
View(head(df[order(-df$x),],20))



## What types of public contracts have been awarded in the last 10 or so years?

# Most valuable awarded public contracts by winning company indusry
industry_bids <- aggregate(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"],
                           by = list(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price),
                                                             c("SICCode.SicText_1" )]), FUN = length)

industry_bids$perc <- industry_bids$x/sum(industry_bids$x)*100
#View(industry_bids[order(-industry_bids$x),])
barplot(head(industry_bids[order(-industry_bids$perc),"perc"],15))
head(industry_bids[order(-industry_bids$perc),],15)


## How competitive have the bids been?

# Distribution of awarded contracts by number of bids
hist_bids <- hist(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "yes"
                             & !is.na(merged_basic_opentender$bid_price),"lot_bidsCount"], breaks = 1000)

plot(hist_bids, main = "Number of bids per lot/tender", xlab = "Number of bids", cex = 0.5, xlim = c(0,20))

head(aggregate(merged_basic_opentender$lot_bidsCount, by = list(merged_basic_opentender$lot_bidsCount), FUN = length))
tail(aggregate(merged_basic_opentender$lot_bidsCount, by = list(merged_basic_opentender$lot_bidsCount), FUN = length))

## But all of those bids look like they have been won... Are there any "lost" bids in the dataset?
aggregate(merged_basic_opentender$lot_status,
          by = list(merged_basic_opentender$lot_status, merged_basic_opentender$bid_isWinning),
          FUN = length)

View(merged_basic_opentender[merged_basic_opentender$bid_isWinning == "" 
                             & merged_basic_opentender$lot_status == "AWARDED",])

# Very few... Does it make sense to try and compare given it's overwhelmingly stacked towards winning bids?



## What predicts who wins a bid among bidders? Age of company? Year? Geography? Company wealth? 

# 

# Supervised way - OLS


# Unsupervised way - LASSO or prcomp or k-means cluster of bid results and these features




#################################### Open Tender Network Analysis #########################################
library(igraph)
library(tidyverse)



### Create a network diagram of buyers who have contracts with secrecy jurisdiction companies

## Pull in list of buyers associated with companies in secrecy jurisdictions
buyers_secrecy_raw <- read.csv("secrecy_awarded.csv", stringsAsFactors = FALSE)
columns.to.keep <- c("secret", "company_number", "buyer_name")
buyers_secrecy_raw <- buyers_secrecy[,columns.to.keep]


## Tag the buyers and companies
merged_basic_opentender$secrecy_buyer <- ifelse(merged_basic_opentender$buyer_name %in%
                                                  buyers_secrecy$buyer_name, 1, 0)

merged_basic_opentender$secrecy_bidder <- ifelse(merged_basic_opentender$CompanyNumber %in%
                                                   buyers_secrecy$company_number, 1, 0)


## Basic descriptive info on these secrecy bidders and their buyers

# number of buyers involved in secrecy jurisdictions out of total
num_secrecy_buyers <- dim(unique(subset(merged_basic_opentender, secrecy_buyer == 1, select = buyer_name)))[1]
total_buyers <- length(unique(merged_basic_opentender$buyer_name))
num_secrecy_buyers
total_buyers
num_secrecy_buyers/total_buyers

# number and Avg value of bids for secrecy vs non-secrecy buyers - may need to change value column
aggregate(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"],
          by = list(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "secrecy_buyer"]),
          FUN = mean)
aggregate(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"],
          by = list(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "secrecy_buyer"]),
          FUN = length)

# Number and value of bids as a prportion of all buyers
total_bid_value <- sum(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"])
total_bid_value # 2 trillion euros! Should check that number somewhere
total_bid_value_sb <- sum(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price)
                                                  & merged_basic_opentender$secrecy_buyer == 1, "bid_price"])
total_bid_value_sb/total_bid_value # Only 2% of buyers but 27% of bid value - HOLY SHIT

# how many rows have an actual bid price?
length(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price)
                            & merged_basic_opentender$bid_price > 0, "tender_id"])

# Among awardees?
length(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price)
                               & merged_basic_opentender$lot_status == "AWARDED", "tender_id"])

# how many of these don't have a year?
by_year <- merged_basic_opentender[!is.na(merged_basic_opentender$bid_price)
                        & merged_basic_opentender$bid_price > 0,"tender_year"]
by_year <- ifelse(is.na(by_year), "NA",by_year)
table(by_year)
sum(table(by_year)[1:11])/sum(table(by_year)) # 89% have a year - so I would have to drop 10% of variables


# number of bidders involved in secrecy jurisdictions out of total
num_secrecy_bidders <- dim(unique(subset(merged_basic_opentender, secrecy_bidder == 1, select = bidder_name)))[1]
total_bidders <- length(unique(merged_basic_opentender$bidder_name))
num_secrecy_bidders/total_bidders # only 0.2% of all bidders (makes sense, we saw 99.8% were in UK before)

# number and Avg value of bids for secrecy vs non-secrecy bidders - may need to change value column
aggregate(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"],
          by = list(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "secrecy_bidder"]),
          FUN = mean)
aggregate(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"],
          by = list(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "secrecy_bidder"]),
          FUN = length)

# Number and value of bids as a prportion of all bidders
total_bid_value <- sum(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price), "bid_price"])
total_bid_value # 2 trillion euros! Should check that number somewhere
total_bid_value_sbid <- sum(merged_basic_opentender[!is.na(merged_basic_opentender$bid_price)
                                                  & merged_basic_opentender$secrecy_bidder == 1, "bid_price"])
total_bid_value_sbid/total_bid_value # Only .2% of bidders and only .4% of bid value - in line



## In summary, there's about 180k contracts with years since 2009, summing up to about 2T Euros
# The buyers who awarded these contracts are big deals - they are only 2% of buyers but award more than 
# a quarter of total public procurement funds The contracts and secrecy jurisdiction companies 
# themselves are small enough as a proportion of all procurement spending to fall under the radar - 
# only about .2% of companies (~400/170k) are in these jurisdictions and get .4% of procurement funds.
# BUT it's still a potential leakage of millions of euros/pounds per year


## Create the network of one buyer associated with companies in secrecy jurisdictions

# Create a subset of rows, with just two columns
columns_care <- c("bidder_name", "buyer_name", "clean")
secrecy_buyer_df <- merged_basic_opentender[merged_basic_opentender$secrecy_buyer == 1,columns_care] %>%
  group_by(bidder_name, buyer_name, clean, .drop = TRUE)
#write.csv(bidder_buyer_df, "bidder_buyer_df.csv")

# Create a subset of rows, with just two columns
# secrecy_buyer_df_sample <- secrecy_buyer_df[,c("clean", "buyer_name")]
secrecy_buyer_df_sample <- secrecy_buyer_df[secrecy_buyer_df$buyer_name == "Healthcare Purchasing Consortium (HPC)",
                                               c("clean", "buyer_name")]
secrecy_buyer_df_sample <- subset(secrecy_buyer_df_sample, clean != "", drop = TRUE)

# Create nodes
secrecy_buyers <- secrecy_buyer_df_sample %>%
  distinct(buyer_name) %>%
  rename(label = buyer_name)
secrecy_bidders <- secrecy_buyer_df_sample %>%
  distinct(clean) %>%
  rename(label = clean)
secrecy_nodes <- full_join(secrecy_buyers, secrecy_bidders, by = "label") # makes a single list of buyers and bidders (nodes)
secrecy_nodes <- secrecy_nodes %>% rowid_to_column("id") # adds an id to each row

# Create edges
secrecy_edges <- secrecy_buyer_df_sample %>%
  group_by(buyer_name, clean) %>%
  summarise(weight = n()) %>%
  ungroup()

secrecy_edges <- secrecy_edges %>% 
  left_join(secrecy_nodes, by = c("buyer_name" = "label")) %>% 
  rename(from = id)

secrecy_edges <- secrecy_edges %>% 
  left_join(secrecy_nodes, by = c("clean" = "label")) %>% 
  rename(to = id)

secrecy_edges_list <- secrecy_edges

unique_secrecy_bidders <- unique(merged_basic_opentender[,c("clean", "secrecy_bidder")])
secrecy_edges_list <- merge(secrecy_edges_list, unique_secrecy_bidders, by = "clean", all.x = TRUE)
secrecy_edges_list$color <- ifelse(secrecy_edges_list$secrecy_bidder == 1, "black", "light blue")

secrecy_edges <- select(secrecy_edges_list, from, to, weight, color)


## Plot network using igraph
secrecy_bids_igraph <- graph_from_data_frame(d = secrecy_edges, vertices = secrecy_nodes, directed = TRUE)

#Plot and save to file
jpeg("bids_igraph.jpg", 
     width=6.8, height=6.8, 
     units='in',res=600)
plot(secrecy_bids_igraph, edge.arrow.size = 0.2, vertex.label = NA, vertex.size = 5,
     vertex.color = secrecy_edges$color, edge.color = "grey")
dev.off()


## Create diagram of those companies with weight > 1 for same buyer
secrecy_edges_heavy <- secrecy_edges[secrecy_edges$weight > 1,]

secrecy_nodes_heavy <- merge(secrecy_nodes, secrecy_edges_heavy, by.x = "id", by.y = "to", all.x = TRUE)
secrecy_nodes_heavy[secrecy_nodes_heavy$id == 1,"from"] <- 1
secrecy_nodes_heavy <- subset(secrecy_nodes_heavy, from == 1, select = c("id", "label"), drop = TRUE)

secrecy_bids_igraph_heavy <- graph_from_data_frame(d = secrecy_edges_heavy, vertices = secrecy_nodes_heavy,
                                                   directed = TRUE)

# Plot and save to file
jpeg("bids_secrecy_igraph.jpg", 
     width=6.8, height=6.8, 
     units='in',res=600)
plot(secrecy_bids_igraph_heavy, edge.arrow.size = 0.2, vertex.label.cex = .6, vertex.size = 5,
     vertex.color = ifelse(secrecy_nodes_heavy$id == 55 | secrecy_nodes_heavy$id == 9, "black", "light blue"),
     edge.color = "grey")
dev.off()




####################### Reading in PSC dataset ############################

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







######### Analyze merged PSC data ################


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





