getwd()
setwd("C:/Users/Leonie/Desktop/Machine_learning")

#install.packages("httr")
#install.packages("jsonlite")
library("httr")
library("readxl")
library("jsonlite")
getwd()
######### Merging PSC and companies house datasets ##############

json_dframe_1 <- stream_in(file("psc-snapshot-2020-05-03_1of16.txt"))
json_dframe_2 <- stream_in(file("psc-snapshot-2020-05-03_2of16.txt")) 
json_dframe_3 <- stream_in(file("psc-snapshot-2020-05-03_3of16.txt")) 
json_dframe_4 <- stream_in(file("psc-snapshot-2020-05-03_4of16.txt")) 
json_dframe_5 <- stream_in(file("psc-snapshot-2020-05-03_5of16.txt")) 
json_dframe_6 <- stream_in(file("psc-snapshot-2020-05-03_6of16.txt")) 
json_dframe_7 <- stream_in(file("psc-snapshot-2020-05-03_7of16.txt")) 
json_dframe_8 <- stream_in(file("psc-snapshot-2020-05-03_8of16.txt")) 
json_dframe_9 <- stream_in(file("psc-snapshot-2020-05-03_9of16.txt")) 
json_dframe_10 <- stream_in(file("psc-snapshot-2020-05-03_10of16.txt")) 
json_dframe_11 <- stream_in(file("psc-snapshot-2020-05-03_11of16.txt")) 
json_dframe_12 <- stream_in(file("psc-snapshot-2020-05-03_12of16.txt")) 
json_dframe_13 <- stream_in(file("psc-snapshot-2020-05-03_13of16.txt")) 
json_dframe_14 <- stream_in(file("psc-snapshot-2020-05-03_14of16.txt")) 
json_dframe_15 <- stream_in(file("psc-snapshot-2020-05-03_15of16.txt")) 
json_dframe_16 <- stream_in(file("psc-snapshot-2020-05-03_16of16.txt")) 
class(json_dframe_8)

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
keep.columns <- colnames(json_dframe_2)[c(1,2,4:7,11:16,18:21,24:25,27,29:33)]
json_dframe_16[keep.columns] <- NA

# Bind dataframes together
json_all_dframe <- rbind(json_dframe_2[,keep.columns],json_dframe_1[,keep.columns])
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
View(json_all_dframe)

### 1 ### - What types of PSCs are recorded? 
#split by type of PSC 
unique(json_all_dframe$data.kind)
all_ind <- json_all_dframe[json_all_dframe$data.kind == "individual-person-with-significant-control",]
all_corp <- json_all_dframe[json_all_dframe$data.kind == "corporate-entity-person-with-significant-control",]
all_legal <- json_all_dframe[json_all_dframe$data.kind == "legal-person-person-with-significant-control",]
all_secret <- json_all_dframe[json_all_dframe$data.kind == "super-secure-person-with-significant-control",]
#how many of each type of PSC
nrow(all_ind) #6,944,817
nrow(all_corp) #817,100 
nrow(all_legal)#288593 
nrow(all_secret) #279860

### 2 ### - Any suspicious entries in terms of age? 
toddlers <- subset(json_all_dframe, json_all_dframe$data.date_of_birth.year > 2015) #5 or younger 
nrow(toddlers) #7603
old_guys <- subset(json_all_dframe, json_all_dframe$data.date_of_birth.year < 1920) #more than 100 years 
nrow(old_guys) #291

###3### - What do the super secret PSCs look like? 
#based on sections 790ZF and 790ZG of the Companies Act 2006:
#related to animal testing or defence (including suppliers and family members) or
#previously targeted by activists
View(all_secret) #no obvious connection to any of the above for some rows, but not clear how to verify 

### 4 ### 
#Are people incorrectly recording individuals and corporate entities as "legal persons"? 
all_legal_noindi <- subset(all_legal, is.na(all_legal$data.name_elements.forename))
View(all_legal_noindi) #does not help

### 5 ### - Any suspicious corporate PSCs in secrecy jurisdictions? 
#FSI secrecy index score above 75; spelling consistent with "country crosswalk" dictionnary
mega_secret <- c("Cayman Islands", "United Arab Emirates", "Qatar", "The Bahamas", "Algeria", "Kenya", "Angola", "Jordan", "Anguilla", "Puerto Rico", "Saint Kitts And Nevis", "Maldives", "Paraguay", "Bolivia", "Turks And Caicos Islands", "Vanuatu", "Liberia", "Antigua And Barbuda", "Brunei")
#FSI secrecy index score above 70
secret <- c("Cayman Islands", "United Arab Emirates", "Qatar", "The Bahamas", "Algeria", "Kenya", "Angola", "Jordan", "Anguilla", "Puerto Rico", "Saint Kitts And Nevis", "Maldives", "Paraguay", "Bolivia", "Turks And Caicos Islands", "Vanuatu", "Liberia", "Antigua And Barbuda", "Brunei","Switzerland", "British Virgin Islands", "Guernsey", "Panama", "Thailand", "Kuwait", "Nigeria", "Vietnam", "Sri Lanka", "Bermuda", "Egypt", "Marshall Islands", "Mauritius", "Liechtenstein", "Cameroon", "Bangladesh", "Barbados", "Guatemala", "Samoa", "US Virgin Islands", "Seychelles", "Curacao", "Tanzania", "Monaco", "Belize", "Aruba", "Dominica", "Gambia", "Grenada", "Montserrat", "Saint Lucia", "Cook Islands")


secret_psc <- subset(all_corp, all_corp$data.identification.country_registered %in% secret)
mega_secret_psc <- subset(all_corp, all_corp$data.identification.country_registered %in% mega_secret)
View(secret_psc) #6486 --> DO COUNTRY CROSSWALK
View(mega_secret_psc) #736 --> DO COUNTRY CROSSWALK

###Taking a look at exemptions 
place_reg <- unique(json_all_dframe$data.identification.place_registered)
View(place_reg) #Use this and legislation to create a vector of exempt stock exchanges 

exempt_all <- c("State of Israel, Listed On Tel Aviv Stock Exchange", "Tokyo Stock Exchange", "Tokyo Stock Exchange (2nd Section)", "Tokyo Listed Exchange", "Tokyo Stock Exchange Listed Companies (Japan)", "Listed On Tokyo Stock Exchange", "Tokyo Stock Exchange and Nasdaq", "Tokyo Stock Exchange, Inc", "Tokyo Stock Exchange (First Section)", "N/A (But Tokyo Stock Exchange)", "Tokyo Stock Exchange, First Section", "Listed On The Tokyo Stock Exchange, Tokyo Ticker:", "Tokyo Stock Exchange, Japan", "Listed On The Tokyo Stock Exchange, Stock No. 4508", "Tokyo Stock Exchange 2 (Code 6670)", "Japan/Listed On Tokyo Stock Exchange", "Listed On The Tokyo Stock Exchange, Tokyo Ticker", "Japan (Including Tokyo Stock Exchange)", "Stock Exchange Listings: Tokyo And Others", "Six Swiss Exchange", "Swiss Six Exchange", "Six Swiss Stock Exchange", "Swiss Stock Exchange", "New York Stock Exchange", "It Has Voting Shares Admitted To Trading On The New York Stock Exchange", "Listed On The New York Stock Exchange, Ticker Symbol-Yge", "Delaware Law, New York Stock Exchange And The U.S. Securities And Exchange Commission", "State Of Indiana Companies Registry And The New York Stock Exchange", "Listed On New York Stock Exchange", "New York Stock Exchange Llc", "Listed On The New York Stock Exchange, Ticker-Yge", "New York Stock Exchange", "Listed On The New York Stock Exchange, Cusip No. 8", "Listed On New York Stock Exchange Under Symbol 'Leg'", "New York Stock Exchange, Register Of Delaware Corporations", "New York Stock Exchange Listed", "New York Stock Exchange (Nyse)", "Delaware And New York Stock Exchange (Br.N)", "Registered On The New York Stock Exchange. Regulated By The U.S. Securities And Exchange Commission", "Listed On The New York Stock Exchange", "Delaware And New York Stock Exchange", "Delaware Division Of Corporations Listed On New York Stock Exchange", "Listed On The New York Stock Exchange (Nyse)", "New York Stock Exchange, (Jpm)", "New York Stock Exchange (Ge)", "New York Stock Exchange", "New York Stock Exchanges", "The New York Stock Exchange", "Nyse", "Listed On Lse, Dow Jones Nyse", "Public Company Listed On The Nyse", "Listed On The Nyse", "Nyse:Cbg", "Bermudan Registrar & Listed On Nyse", "Nyse (Ctl)", "Nyse, Delaware Corp.", "Listed On Nyse", "Cre Ireland And Listed On The Nyse", "Listed On The Nyse As Clb", "Corp Bureau Of Pennsylvania & Listed On The Nyse", "Usa - Delaware - Listed On Nyse", "(Nyse:Csc)", "(Nyse:Csc", "Nyse Dhr", "State Of Delaware & Listed On Nyse", "Listed On Nyse, Governed By U.S. Securities And Exchange Commission", "Nyse: Azo", "Admitted For Trading In Nyse", "Nyse:Dxc", "Nyse:Bx", "Securities And Exchange Commission And Listed On The Nyse", "Delaware - Nasdaq", "Nasdaq", "Nasdaq: Lake", "Nasdaq Stock Market Llc", "Nasdaq Global Select Market", "Nasdaq and Toronto Stock Exchange", "Tokyo Stock Exchange And Nasdaq", "Nasdaq Stock Market", "The Nasdaq Stock Market Llc", "Usa Securities And Exchange Commission / Nasdaq Stock Market Llc", "(Nasdaq:Atni)", "Nasdaq Listed", "Nasdaq: Bcli", "The Nasdaq Global Select Market", "Nasdaq Global Select", "Nasdaq Stock Exchange", "Nasdaq Gs Stock Exchange", "Nasdaq Omx", "Usa - Washington / Registered On Nasdaq", "Registered On Nasdaq", "Listed On The Nasdaq Stock Market Llc. Under 'Mind'", "Nasdaq - Usa", "Us Law/Nasdaq Listed", "Delaware Division Of Corporations (Listed On Nasdaq)", "The Registrar Of Companies At The Ministry Of Justice Of Israel (Listed On Nasdaq)", "Cayman Islands - Listed On Nasdaq", "Sec & Nasdaq", "Listed On Nasdaq", "Nasdaq Stock Market Llc (Nasdaq Global Select Market Tier)", "Nasdaq (United States)", "Traded On The Nasdaq Under Twou", "Nasdaq And Tsx Venture Exchange", "Usa - Nasdaq", "Publicly Traded Us Company Trading On Nasdaq", "Nasdaq & Massachusetts Division Of Corporations")
exempt <- c("Tel Aviv Stock Exchange", "Fukuoka Stock Exchange", "Nagoya Stock Exchange", "Osaka Securities Exchange", "Sapporo Securities Exchange", "Tokyo Stock Exchange", "BX Berne Exchange", "SIX Swiss Exchange", "BATS Exchange, Inc.", "BATS Y-Exchange, Inc.", "BOX Options Exchange LLC", "C2 Options Exchange, Incorporated", "Chicago Board Options Exchange Incorporated", "Chicago Stock Exchange, Inc.", "EDGA Exchange, Inc.", "EDGX Exchange, Inc.", "International Securities Exchange, LLC", "ISE Gemini LLC", "Miami International Securities Exchange, LLC", "NASDAQ OMX BX, Inc.", "NASDAQ OMX PHLX LLC", "The NASDAQ Stock Market LLC", "National Stock Exchange, Inc.", "New York Stock Exchange LLC", "NYSE Arca, Inc.", "NYSE MKT LLC")

'%nin%' <- Negate('%in%') #create not in operator 
secret_excl_exempt <- subset(secret_psc, secret_psc$data.identification.place_registered %nin% exempt_all)
mega_secret_excl_exempt <- subset(mega_secret_psc, mega_secret_psc$data.identification.place_registered %nin% exempt_all)
nrow(secret_excl_exempt) #6440 
nrow(mega_secret_excl_exempt) #729
