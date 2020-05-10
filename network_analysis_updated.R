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

#################### load and merge ###############################

data1 <- stream_in(file("data1.txt")) #load first file
data1 <- flatten(data1, recursive = T) #flatten
colnames(data1)

keep <- colnames(data1[,c(1,3,6,11,29,31)]) #relevant column names
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
colnames(basic)
#choosing the relevant variables
basic <- basic[,c(1,2,13,15,27)]



#Merge the two datasets

m_data <- merge(data,basic, by.x = "company_number", by.y = "CompanyNumber", all.x = TRUE)


#################### crosswalk ###################

#Load the cross walk
cross <- read_csv("country_crosswalk_complete.csv")
cross <- cross[,-2] #Remove the frequency column


#The crosswalk function takes the value of the column needed to be crosswalked
#For exampe x = "data.country_of_residence"/"data.address.country"/"data.identification.country_registered"
crosswalk_merge <- function (x) {
  #The function takes the value of the variable name that will be crosswalked
  cross <- read_csv("country_crosswalk_complete.csv")
  cross <- cross[,-2] #Remove the frequency column
  
  m2 <- merge(m_data, cross, by.x = x, by.y = "Row Labels", all.x = T)
  colnames(m2)[ncol(m2)] <- paste("new_",x, sep="")
  return(m2)
}

m2 <- crosswalk_merge("data.identification.country_registered")


#################### Secrecy Jurisdiction #########################

#FSI secrecy index score above 75; spelling consistent with "country crosswalk" dictionnary
mega_secret <- c("Cayman Islands","United Arab Emirates", "Qatar",
                 "The Bahamas", "Algeria", "Kenya", "Angola", "Jordan",
                 "Anguilla", "Puerto Rico", "Saint Kitts And Nevis", "Maldives",
                 "Paraguay", "Bolivia", "Turks And Caicos Islands", "Vanuatu", "Liberia",
                 "Antigua And Barbuda", "Brunei")

#FSI secrecy index score above 70
secret <- c("Cayman Islands", "United Arab Emirates", "Qatar", "The Bahamas",
            "Algeria", "Kenya", "Angola", "Jordan", "Anguilla", "Puerto Rico",
            "Saint Kitts And Nevis", "Maldives", "Paraguay", "Bolivia", "Turks And Caicos Islands",
            "Vanuatu", "Liberia", "Antigua And Barbuda", "Brunei","Switzerland", "British Virgin Islands",
            "Guernsey", "Panama", "Thailand", "Kuwait", "Nigeria", "Vietnam", "Sri Lanka", "Bermuda", "Egypt",
            "Marshall Islands", "Mauritius", "Liechtenstein", "Cameroon", "Bangladesh", "Barbados", "Guatemala",
            "Samoa", "US Virgin Islands", "Seychelles", "Curacao", "Tanzania", "Monaco", "Belize", "Aruba",
            "Dominica", "Gambia", "Grenada", "Montserrat", "Saint Lucia", "Cook Islands")


m2$secret <- as.numeric(m2$new_data.identification.country_registered %in% secret)

data <- subset(m2, secret==1)
data <- subset(data, !is.na(data$new_data.identification.country_registered))

#Simple network of owners [USE THIS]
net <- matrix(nrow=nrow(data), ncol=2)
net[,1] <- data$data.name #feed in the owner column
net[,2] <- data$company_number #feed in the owns column
net <- as.data.frame(net)
colnames(net) <- c("Parent", "Child")
#table(net$Parent)

#Studying the company: Partners Group Holding AG
data <- subset(m_data, m_data$data.name== "Partners Group Holding Ag")

net <- matrix(nrow=nrow(data), ncol=2)
net[,1] <- data$data.name #feed in the owner column
net[,2] <- data$company_number #feed in the owns column
net <- as.data.frame(net)
colnames(net) <- c("Parent", "Child")

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

library(network)

#Create the network object with network package - make sure igraph is not loaded
company_network <- network(net_edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

#plot
plot(company_network, vertex.cex = 0.5)

#Table of suspicious companies
sus <- as.data.frame(table(net_weight$Parent))
sus$Freq <- as.numeric(sus$Freq)
sus$Var1 <- as.character(sus$Var1)
baje <- sus[order(-sus$Freq),]

#How to add the industry list to the dataframe
edges <- merge(edges, m_data[,c("company_number","SICCode.SicText_1")], by.x = "Child", by.y = "company_number")
edges <- unique(edges)
colnames(edges)[6] <- "industry"
#run this before the plot to get industry list
net_edges <- select(edges,from,to,weight,industry)

#How to get an array of unique colors and match it to each industry
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
length(color)
col_data <- as.data.frame(matrix(nrow = length(unique(net_edges$industry)), ncol=2))
col_data$V1 <- unique(net_edges$industry)
col_data$V2 <- olor[floor(runif(length(unique(net_edges$industry)), min=1, max=433))]
#Use this merge to get the designated color values in the dataframe
net_edges <- merge(net_edges, col_data, by.x = "industry", by.y = "V1")

detach(package:network)
#detach(package:igraph)

library(igraph)
net_edges[,c(1,2,3,4,5)] <- net_edges[,c(2,3,4,1,5)]
colnames(net_edges) <- c("from", "to", "weight", "industry", "color")

routes_igraph <- graph_from_data_frame(d = net_edges, vertices = nodes, directed = F)


#Run plot function last
plot(routes_igraph, edge.arrow.size = 1, layout = layout.fruchterman.reingold,
     vertex.label= NA, vertex.size = 5, vertex.color = net_edges$color, col=net_edges$color)

legend("bottomleft", legend=unique(net_edges$industry),
       col=unique(net_edges$color))
