rm(list=ls())
setwd("C:/Users/rasim/Desktop/MPP1/MIT/PSC/")
install.packages("httr")
install.packages("jsonlite")
library("httr")
library("readxl")
library("jsonlite")
json_dframe <- stream_in(file("UKtext.txt")) #"Uktext" is what I call the PSC file on my end, use your own text file

#Trying the API
###############################################################################
my_api_key <- "x"

GET(
  "https://api.companieshouse.gov.uk/company/00000006",
  authenticate("PSC Learner", my_api_key)
)
disq <- GET("https://api.companieshouse.gov.uk/search/disqualified-officers",
            authenticate("PSC Learner", my_api_key, type = "basic"))
################################################################################


#Initial Analysisvbn
###############################################################################
train_ind <- sample(seq_len(nrow(json_dframe)), size = 7000, replace = FALSE)
uk <- json_dframe[train_ind,]
table(uk[[2]][[1]]$country)
table(uk[[2]]$kind)
uk[uk[[2]]$kind=="legal-person-person-with-significant-control",]
library(plyr)

table(uk[[2]]$name)

sanc <- read_excel("sanc.xlsx")
ben_own <- uk[[2]]$name


table(json_dframe[[2]]$address$region)
tab <- as.data.frame(table(json_dframe[[2]]$name))

#How to find people who own multiple companies
multi_owner <- subset(tab, Freq > 1)
bigboy <- as.data.frame(multi_owner[multi_owner$Freq>10,1]) #people who own more than 10 companies under the same name

#Rich people and company beneficial owners
rich <- json_dframe[json_dframe[[2]]$name %in% bigboy$`multi_owner[multi_owner$Freq > 10, 1]`,] #separating 10+ owners
comp_owned <- rich[is.na(rich[[2]]$name_elements$forename),] #10+ owners that are companies only
comp_owned <- flatten(comp_owned, recursive = T) #How to flatten a dataframe
examin <- subset(comp_owned, select=c("data.name", "data.identification.legal_authority"))

#Human benecficial owners with more than 10 companies
humans <- rich[!is.na(rich[[2]]$name_elements$forename),] #10+ owners that are humans
humans <- flatten(humans, recursive = T)
table(humans$data.address.country)

#Finding people with over 10 comapnies listed to their names, where over 100 companies are listed in the same address. 
susp <- as.data.frame(table(humans$data.address.address_line_1))
big_susp <- as.data.frame(susp[susp$Freq>100,1])

#comp_owned is all the rows in the dataset owned by companies. 
table(comp_owned$data.identification.place_registered)
