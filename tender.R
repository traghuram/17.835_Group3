setwd("C:/Users/Leonie/Desktop/Machine_learning")
#install.packages("httr")
#install.packages("jsonlite")
library("httr")
library("readxl")
library("jsonlite")
library(dplyr)


################# Import the basic company data #####################
## Read in all files (condense code later) - create a folder in your working directory and paste the files in there
digiwhist_2009 <- read.csv("data-uk-2009.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2010 <- read.csv("data-uk-2010.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2011 <- read.csv("data-uk-2011.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2012 <- read.csv("data-uk-2012.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2013 <- read.csv("data-uk-2013.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2014 <- read.csv("data-uk-2014.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2015 <- read.csv("data-uk-2015.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2016 <- read.csv("data-uk-2016.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2017 <- read.csv("data-uk-2017.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2018 <- read.csv("data-uk-2018.csv", sep = ";", stringsAsFactors = FALSE)
digiwhist_2019 <- read.csv("data-uk-2019.csv", sep = ";", stringsAsFactors = FALSE)


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
                       digiwhist_2019)

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
   digiwhist_2019)

dw <- digiwhist_all
#single bid received 
mean_single <- tapply(dw$tender_indicator_INTEGRITY_SINGLE_BID, dw$tender_year, mean, na.rm = TRUE)
df_mean_single <- as.data.frame(mean_single)
df_mean_single$tender_year <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
df_mean_single$mean_single <- 100 - df_mean_single$mean_single 

#risky advertisement period 
mean_ad <- tapply(dw$tender_indicator_INTEGRITY_ADVERTISEMENT_PERIOD, dw$tender_year, mean, na.rm = TRUE)
df_mean_ad <- as.data.frame(mean_ad)
df_mean_ad$tender_year <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
df_mean_ad$mean_ad <- 100 - df_mean_ad$mean_ad
df <- merge(df_mean_single, df_mean_ad, by = "tender_year")

#new company 
mean_new <- tapply(dw$tender_indicator_INTEGRITY_NEW_COMPANY, dw$tender_year, mean, na.rm = TRUE)
df_mean_new <- as.data.frame(mean_new)
df_mean_new$tender_year <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
df_mean_new$mean_new <- 100 - df_mean_new$mean_new
df_all <- merge(df, df_mean_new, by = "tender_year")
View(df_all)

jpeg("bids_igraph.jpg",
     width=6.8, height=6.8,
     units='in',res=600)

plot(x = df_all$tender_year, y = df_all$mean_single, ylim = c(0,100), col = "red", 
     ylab = "", xlab = "tender year",
     main = "Mean tender riskiness indicators over time")
points(x = df_all$tender_year, y = df_all$mean_ad, col = "blue")
points(x = df_all$tender_year, y = df_all$mean_new, col = "green")
legend(2009, 100, legend = c("single bid", "rushed tender", "new company wins"), 
       col = c("red", "blue", "green"), lty = 1, cex = 0.6)
lines(x = df_all$tender_year, y = df_all$mean_single, col = "red")
lines(x = df_all$tender_year, y = df_all$mean_ad, col = "blue")
lines(x = df_all$tender_year, y = df_all$mean_new, col = "green")
dev.off()