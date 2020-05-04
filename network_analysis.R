rm(list=ls())
setwd("C:/Users/rasim/Desktop/MPP1/MIT/PSC/") #Set your own working directory here
#install.packages("httr")
#install.packages("jsonlite") #important for reading data in
#library("httr")
#library("readxl")
library(jsonlite)
library(readr)
library(dplyr)

#my code to load the whole data is below. I renamed all the data files as data1.txt, data2.txt and so on


data1 <- stream_in(file("data1.txt")) #load first file
data1 <- flatten(data1, recursive = T) #flatten

keep <- colnames(data1[,c(1,3,6,23,24,25,31)]) #relevant column names
data1 <- data1[,keep] #make it into an array of column names in characters

#create an empty dataframe where the data will be merged
data <- data.frame(matrix(nrow=nrow(data1), ncol=ncol(data1)))
colnames(data) <- colnames(data1)

#merge data1 to data file
data <- rbind(data,data1)
data <- data[-c(0:500000),] #remove the empty first 500k rows
rm(data1)

#I load the rest of the data here except file 16 because it's different from the rest
for (i in 2:15) {
  
  data2 <- stream_in(file(paste("data", i, ".txt", sep="")))
  data2 <- flatten(data2)
  data2 <- data2[,keep]
  data <- rbind(data, data2)
  rm(data2)
}

#Load the basic data

#I call the basic companies feature dataset "basic.csv"
basic <- read_csv("basic.csv")
#choosing the relevant variables
basic <- basic[,c(1,2,13,27)]


#Merge the two datasets

m_data <- merge(data,basic, by.x = "company_number", by.y = "CompanyNumber", all.x = TRUE)


##############################################################################
# Network Analysis
##############################################################################

#We will be using m_data as the root dataframe and will create different dataframes from it to 
#make the network plots

#NOTE: Do not load the "network" package and the "igraph" package together. This seenms to cause 
#problems

#First we create two arrays, an array of companies, and an array of owners

#Company numbers
company <- m_data$company_number #this is the first column where the company number is stored
#Registration Numbers of owner companies
owners <- m_data$data.identification.registration_number #this column is the registration number of a company that owns other companies

#We match it using the base function match() to find companies that are also in the owners list
ma <- match(company, owners)
#Here I do the same thing but in the tidyverse way
ma_in <- company %in% owners

#Put both the match tags in the original dataset
m_data$circular <- ma_in
m_data$match <- ma
#This will tag only the rows that show up on the owner list

#Now we separate the dataframe with only those rows that are owners
circ <- subset(m_data, circular == TRUE)

#The owns column should have the name of the company that is owned. This is done by matching the row number
#from our column "match". Make sure that the column is referring to the company name column in m_data
circ$owns <- m_data[circ$match,4] #the company name column in m_data
#This is superfluous but just to make things clear, we call the company name variable as owner and put it in a
#different column
circ$owner <- circ$CompanyName

#Create a data matrix which will be used for creating the network object
net <- matrix(nrow=nrow(circ), ncol=2)
net[,1] <- circ$owner #feed in the owner column
net[,2] <- circ$owns #feed in the owns column
net <- as.data.frame(net)
colnames(net) <- c("Parent", "Child") #This step is super important to signify the columns for the subsequent functions


library(tidyverse)

#Distill the unique values for parent
parent <- net %>% distinct(Parent) %>% dplyr::rename(label=Parent)
#Distill the unique values for child
child <- net %>% distinct(Child) %>% dplyr::rename(label=Child)

#full join creates the matrix that we'll use for the network object
nodes <- full_join(parent,child, by="label")
#presents each unique node with an id
nodes <- nodes %>% rowid_to_column("id")

#Creating the weight object
net_weight <- net %>%  
  dplyr::group_by(Parent, Child) %>%
  dplyr::summarise(weight = dplyr::n()) %>% 
  dplyr::ungroup()

#Adding the id numbers for the Parent column
edges <- net_weight %>% 
  dplyr::left_join(nodes, by = c("Parent" = "label")) %>% 
  dplyr::rename(from = id)

#Adding the id numbers for the Child column
edges <- edges %>% 
  dplyr::left_join(nodes, by = c("Child" = "label")) %>% 
  dplyr::rename(to = id)

#We select out the from ids, the to ids, and the weight
net_edges <- select(edges, from, to, weight)

#Create the network object with network package - make sure igraph is not loaded
company_network <- network(net_edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

#plot
plot(company_network, vertex.cex = 0.5)



