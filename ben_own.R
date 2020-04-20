rm(list=ls())
setwd("C:/Users/rasim/Desktop/MPP1/MIT/PSC/")
#install.packages("httr")
#install.packages("jsonlite")
#library("httr")
#library("readxl")
library(jsonlite)
library(readr)
library(dplyr)
library(plyr)

#How to convert the Json files into csv files
# for (i in 1:16) {
#   data2 <- stream_in(file(paste("data", i, ".txt", sep=""))) 
#   data2 <- flatten(data2)
#   
#   data2<-data.frame(lapply(data2, as.character), stringsAsFactors=FALSE)
#   write.table(data2, file=paste("data",i,".csv", sep=""), row.names=FALSE, na="", col.names = TRUE, sep=",")
# #write.csv(data1, "Data1.csv", row.names=F)
# }
# rm(data1)

#There are differences in columns for each of the data files
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
diff <- colnames(data2) %not in% colnames(data1)

#How to bind the csv files
files <- list.files(pattern = "*.csv", full.names = TRUE)
dat_csv = ldply(files, read_csv) #Very computationally expensive

# library(data.table)
# dat <- read_csv("data1.csv")
# dat_csv = ldply(files, fread) #Very computationally expensive

mul_own <- as.data.frame(table(dat_csv$data.name))
bigboy <- as.data.frame(mul_own[mul_own$Freq > 100,])
plot(bigboy)
write.csv(bigboy, "bigboy.csv")

bigboy <- as.data.frame(own[own[,2] > 100])

pl <- data.frame(nrow=412, ncol=2)
pl <- bigboy
pl <- as.data.frame(pl)
library(ggplot2)
pl$V3 <- as.numeric(pl$V2)
pl$Var1 <- as.character(pl$Var1)
pl2 <- pl[pl$Freq>1000,]

#The Bar Graph for owners over 1000 companues
p<-ggplot(data=pl2, aes(x=reorder(pl2$Var1, pl2$Freq), y=pl2$Freq)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Owners") + ylab("Number of Companes") + ggtitle("Owners with over 1000 Companies")
p

#Bar Graph for Owners over 100 companies
q<-ggplot(data=pl, aes(x=reorder(pl$Var1, pl$Freq), y=pl$Freq)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Owners") + ylab("Number of Companes") + ggtitle("Owners with over 1000 Companies") +
  theme(axis.text.x=element_blank())
q


dat_csv <- dat_csv[,-c(30:50)]
sum(is.na(dat_csv$data.name_elements.forename))
bigboy <- read.csv("./Research/bigboy.csv")

#To find number of rich
rich <- dat_csv[dat_csv$data.name %in% bigboy$Var1,1]

#Let's examine nationality
nation <- as.data.frame(table(dat_csv$data.nationality))
Brit <- c("Biritish", "Biritsh", "Births", "Birtish", "Birtish National (Overseas)",
          "Br5itish", "Briish", "Briitish", "Briitsh", "Brietish", "Brirish", "Brirtish",
          "Brish", "Brishish", "Brisish", "Brishith", "British/Irish", "Bristish", "Bristih",
          "Bristis", "British", "Britiish", "British Virgin Islander", "British,American",
          "British,Australian", "British, Canadian", "English", "Uk", "United Kingdom")
British <- length(which(dat_csv$data.nationality %in% Brit))

for_own <- matrix(nrow = 2, ncol = 2)
for_own <- as.data.frame(for_own)
for_own$V1 <- c("British Owner", "Foreign Owner")
for_own$V2 <- c(5381456, 2371270)

m <- ggplot(data=for_own, aes(x=V1, y=V2)) + geom_bar(stat = "identity", width = 0.2) + xlab("Owner Origin") + 
  scale_y_continuous(name="Number of Companies", labels = c("0", "2m", '4m', "6m"), 
                     breaks = c(0,2000000,4000000,6000000)) +ggtitle("Owner Origin")
m

#Exploring Residence
resid <- as.data.frame(table(dat_csv$data.country_of_residence))
resid <- resid[order(-resid$Freq),]
head(resid)

rpl <- resid[3:15,]

n <- ggplot(data=rpl, aes(x=reorder(Var1,Freq), y=Freq)) + geom_bar(stat = "identity", width = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +xlab("Residence of Owners") +
  ggtitle("Owners Uutside of England") + scale_y_continuous(name="Number of Companies",
                                                            labels = c("0", "100k", '200k', "300k"), 
                                                            breaks = c(0,100000,200000,300000))
n
#Other than England, where are the residences of the owners?


#What about addresses?



#Trying the API
###############################################################################
# my_api_key <- "x"
# 
# GET(
#   "https://api.companieshouse.gov.uk/company/00000006",
#   authenticate("PSC Learner", my_api_key)
# )
# disq <- GET("https://api.companieshouse.gov.uk/search/disqualified-officers",
#             authenticate("PSC Learner", my_api_key, type = "basic"))
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


library(ff)
bigdata <- read.table.ffdf(file="statements.latest.jsonl")
