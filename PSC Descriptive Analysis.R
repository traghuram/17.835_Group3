#Set the working directory and check it worked
setwd("C:/Users/Taran/Documents/Personal/Courses/MIT Final Pset")
getwd()

#load csv file
library("dplyr")
library("rjson")
library("jsonlite")
#json_dframe <- stream_in(file("persons-with-significant-control-snapshot-2020-04-14.txt"))
#View(json_dframe)



library(ff)

school.ff <- read.txt.ffdf(file="persons-with-significant-control-snapshot-2020-04-14.txt")
