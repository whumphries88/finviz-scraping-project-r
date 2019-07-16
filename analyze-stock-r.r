#end of week analysis of stock data after news release made public
install.packages("data.table") #for nearest date time join
install.packages("zoo")
install.packages("rvest")
install.packages("xml2")
install.packages("selectr")
install.packages("plyr")
library(plyr)
library(stringr)
library(zoo)
library(rvest)
library(httr) 
library(jsonlite)
library(tidyverse)
library(rjson)
library(lubridate)
library(dplyr)
library(tidytext)
library(taRifx)

#list of stocks for plotting and analysis to find value investing picks
# reverse-split stocks:  SDRL, OPGN, PRPO
# stock_list_for_analysis <- c("ACRX", "EKSO", "AFMD", "CDR", "QHC","DSKE", "FSI", "GALT", "ASUR", "ATXI", "GORO", "BKS", "DLNG", "CTHR", "EXPR", "HMHC", "HZN", "LBY", "MGI", "OESX", "PRPO", "PTN", "RIOT", "RWLK", "TTNP", "WKHS")
stock_list_for_analysis <- c("ACRX","APRN", "EKSO", "AFMD", "AHT")
# stock_list_for_analysis <- c("ACRX","APRN", "EKSO", "AFMD", "AHT", "CDR", "QHC","DSKE","EKSO","FLKS", "FSI", "GALT","IGC", "ASUR", "ATXI", "GORO","BNGO", "BKS", "BTG","CRR", "DLNG", "CTHR","ENDP","EMAN", "EXPR","HCR", "HMHC", "HZN","IDSA","JASN","LIFE", "LBY","LPI","MEET", "MGI","NE","NAKD","NGD","OBE", "OESX", "OTLK","PIXY","PHUN","PLYA", "PTN","ROAN", "RIOT","RGSE", "SHIP","SWN")
# stock_list_for_analysis <- c("ACRX","APRN", "EKSO", "AFMD", "AHT", "CDR", "QHC","DSKE","EKSO","FLKS", "FSI", "GALT","IGC", "ASUR", "ATXI", "GORO","BNGO", "BKS", "BTG","CRR", "DLNG", "CTHR","ENDP","EMAN", "EXPR","HCR", "HMHC", "HZN","IDSA","JASN","LIFE", "LBY","LPI","MEET", "MGI","NE","NAKD","NGD","OBE", "OESX", "OTLK","PIXY","PHUN","PLYA", "PTN","ROAN", "RIOT","RGSE", "RWLK", "SHIP","SWN","SVRA","TLGT","TGB","TOUR", "TTNP","UXIN","VEON", "VBIV", "WISA", "WKHS","XOG", "XRF")
words_to_ignore <- c("globenewswire", "earnings", "announces", "offering", "zacks")

count_for_api_call_limitation <- 0

for (stock in stock_list_for_analysis) {
  print(paste(count_for_api_call_limitation, " calls to API made"))
  count_for_api_call_limitation <- count_for_api_call_limitation + 2
  
  file_system_path <- paste("C:\\Users\\wh95998\\R and Data Science\\finviz-scraping-project\\stocks_scraped\\stocks_",stock,".html", sep="")
  url <- paste("https://finviz.com/quote.ashx?t=",stock,sep="") #read url to variable to find Finviz news biggest gainers and losers
  download.file(url, destfile = file_system_path,quiet=TRUE)
  pctTbl <- html_table(html_nodes(read_html(file_system_path), '#news-table'), header = NA, fill = TRUE)[[1]] #extract html table from webpage then convert to dataframe
  pctTbl$stock = stock #add stock name to dataframe
  rgxNum <- "(\\-|\\+).*[0-9]{1,}\\.[0-9]{2}%"  #regex for parsing news articles
  rgxDate <- "(\\-.*\\-)"
  pctTbl$X1[!grepl(rgxDate, pctTbl$X1)] <- NA   # necessary for filldown data using previous available date
  pctTbl$X1 = na.locf(pctTbl$X1, na.rm = TRUE) # using zoo library - fill from last
  
  #newsSubset <- grepl(rgxNum, pctTbl$X2) #finds all news articles with large moves (over 5%)
  #pctTbl[which(!grepl(rgxDate, pctTbl$X1)),] #doesn't have date
  newsMoving <- pctTbl[which(grepl(rgxNum, pctTbl$X2)),] # makes dataframe of only large moves
  colnames(newsMoving) <- c("date","article", "stock") #change column names of dataframe
  
  newsMoving <- mutate(newsMoving, percent = str_extract(newsMoving$article, "(\\-|\\+)[0-9]{1,}\\.[0-9]{2}%"))   #major movers with news
  newsMoving$date <- substr(newsMoving$date, 1, 9) #remove time from dates for formatting
  newsMoving$date <- as.Date(newsMoving$date, format = "%B-%d-%y") #converts dates to same format - MUST BE as.Date
  newsMoving <- newsMoving[!apply(newsMoving, 1, function(x){any(is.na(x))}),] #remove all rows with NA in row
  
  #Combine newsMoving with stock day data
  path <- "https://www.alphavantage.co/query?"
  query <- paste("function=TIME_SERIES_DAILY_ADJUSTED&symbol=",stock,"&outputsize=full&apikey=92C4N7A6LTBOKLIH", sep = "")
  pathFull <- paste(path, query, sep = "")  #combine url api with query for stock
  json_file_date <- fromJSON(file = pathFull) #pull in json data from api
  symbol <- json_file_date[["Meta Data"]][[2]] #get the symbol of stock from the metadata
  dta_date <- do.call(rbind.data.frame, json_file_date[[2]])  #converts to dataframe
  dta_date <- setNames(cbind(rownames(dta_date), dta_date, row.names = NULL), c("date","open", "high", "low","close", "adj close", "volume", "div amt", "spl cft"))
  dta_date$date <- ymd(dta_date$date) #convert to same format as other dataframe
  dta_date <- dta_date[ , !(names(dta_date) %in% c("adj close", "div amt", "spl cft"))]  #remove unnecessary columns
  newsMoving <- merge(newsMoving, dta_date, by = "date", all.x = T) #merge on Date

  #Query from api to gather stock prices - weekly
  # Only allowed 5 calls per minute
  print(paste("trying for stock: ", stock))
  path <- "https://www.alphavantage.co/query?"
  query <- paste("function=TIME_SERIES_WEEKLY&symbol=",stock,"&apikey=92C4N7A6LTBOKLIH", sep = "")
  pathFull <- paste(path, query, sep = "")  #combine url api with query for stock
  json_file <- fromJSON(file = pathFull) #pull in json data from api
  symbol <- json_file[["Meta Data"]][[2]] #get the symbol of stock from the metadata
  dta_hist <- do.call(rbind.data.frame, json_file[[2]])  #converts to dataframe 
  dta_hist <- setNames(cbind(rownames(dta_hist), dta_hist, row.names = NULL), c("date","open","high", "low","close", "volume"))
  dta_hist$date <- ymd(dta_hist$date) #convert to same format as other dataframe

  if(count_for_api_call_limitation %% 4 == 0){
    print("Waiting 60 seconds before next API call due to limitation")
    Sys.sleep(60)
  }  
 
  #Convert to one datatable
  for (i in 0:6) {
    if(i == 0 & exists('combined')) {remove(combined)}
    numWeeks = i
    # print(paste("Joining tables on week: ", i))
    
    oldDate <- data.table::data.table(date = as.POSIXct(newsMoving$date) + as.difftime(numWeeks, unit = "weeks") + as.difftime(1, unit = "days"))
    oldDate$date <- if_else(newsMoving$date + as.difftime(numWeeks, unit = "weeks") > Sys.time(), as.POSIXct("2021-09-09"), oldDate$date) #placeholder for too old date
    new <- data.table::data.table(date = as.POSIXct(dta_hist$date), dtahistDateWeek1 = dta_hist$date, openWeek1 = dta_hist$open, lowWeek1 = dta_hist$low, highWeek1 = dta_hist$high, closeWeek1 = dta_hist$close, volumeWeek1 = dta_hist$volume)
    colnames(new) <- c("date", paste("dtahistDate",i, sep = ""), paste("open",i,sep = ""), paste("low",i,sep = ""), paste("high",i,sep = ""), paste("close",i,sep = ""), paste("volume",i,sep = ""))
    data.table::setkey(oldDate, date)
    data.table::setkey(new, date)
    
    if (exists('combined') == FALSE) {
      combined <- new[oldDate, roll = "nearest"] 
      print(paste("combined table created for ", stock))
    } else {
      combined_test <- new[oldDate, roll = "nearest"]
      data.table::setDT(combined)
      data.table::setDT(combined_test)
      combined_test$date  <- if_else(combined$date + as.difftime(numWeeks, unit = "weeks") > Sys.time(), as.POSIXct("2021-09-09"), combined$date) # for merging tables, will bring NA where future stock data doesn't exist
      combined <- merge(combined, combined_test, all.x=T, by = "date") #merge the table on forced similar dates
    }
  }
  
  combined$date <- as.Date(combined$date, format = "%B-%d-%y")
  combined$date <- combined$date - as.difftime(1, unit = "days") #to match dates
  df <- merge(newsMoving, combined, all = TRUE)
  
  #combine datatable and replace old with new
  if (exists('alldata') == FALSE) {
    alldata <- merge(newsMoving, combined, all = TRUE)
  } else {
    if(!any(alldata$stock == stock)){alldata <- merge(alldata, df, all = TRUE)}  #if stock isn't already in datatable add it now
  }
}

df <- alldata
df$percent <- substr(df$percent, 2,nchar(df$percent) - 1) #remove all + - % signs
df$percent <- as.numeric(substr(df$percent, 1,nchar(df$percent) - 1)) #replace column as numeric
df <- japply( df, which(sapply(df, class)=="factor"), as.character) #requires taRifx library - converts all 'factor' class cols to double for calculations
df <- japply( df, which(sapply(df, class)=="character" & !colnames(df) %in% c("article", "stock", "date", "percent")) , as.numeric)


df <- mutate_at(df, vars(starts_with("open")), funs("change" = ((. - df$open ) / df$open) * 100)) #add columns to DF
df <- mutate_at(df, vars(starts_with("high")), funs("change_from_news_open_high" = ((. - df$open ) / df$open) * 100)) #add columns to DF
df <- mutate_at(df, vars(starts_with("low")), funs("change_from_news_open_low" = ((. - df$open ) / df$open) * 100)) #add columns to DF
df <- mutate_at(df, vars(starts_with("volume")), funs("change_from_news_vol" = ((. - df$volume ) / df$volume) * 100)) #add columns to DF

#dropping all columns in table that we are not running analysis over
colms <- c("dtahistDate ", "dtahistDate", "open", "low", "high", "close", "volume")
for (i in 0:6) {
  if(i == 0 & exists('rmCols')) {remove(rmCols)}
  if(!exists('rmCols')) {rmCols <- c("")}
  for (c in colms) {
    c <- paste(c, i, sep = "")
    # print(c)
    rmCols <- append(rmCols, c, after = length(rmCols))
  } 
}
df <- df[, !(names(df) %in% rmCols)]
df <- df[, !(names(df) %in% c("volume_change_from_news_vol", "open_change"))]

text_df <- tibble(text = df$article) %>% unnest_tokens(word, text) #separate text into words
text_df <- anti_join(text_df, get_stopwords(), by = "word") #remove common words from list
text_df <- text_df %>% count(word, sort = TRUE)  #count of all words in articles
text_df <- text_df[text_df$n > 6, ] #text words with more than x count

# Takes the average of every word 
for (word in c(t(text_df$word))) {
  search_word <- word
  text_match_df <- df[unlist(as.data.frame(grep(search_word, tolower(df$article)))),]  #temp dataframe for combining into words table
  
  ### ADD here to remove company names from data
  print(word)
  text_match_df <- df[unlist(as.data.frame(grep(word, tolower(df$article)))),]  #temp dataframe for combining into words table
  company_names <- unique(df[unlist(as.data.frame(grep(word, tolower(df$article)))),1:3]$stock)
  for (cn in company_names) {
    stock_count <- sum(str_count(text_match_df$stock, paste("\\b", cn ,"\\b", sep = "")))  #find count of unique stocks per search word 
    # Some words we know aren't company names
    if (word %in% words_to_ignore) {    
      print(paste("word found", cn))
      break()
    } else if (stock_count > 6) {
      print(paste("Company name: ", cn, " ", stock_count))
      text_match_df <- text_match_df[!text_match_df$stock == cn,]
    } else {
      print(paste("Not company: ", cn, " ", stock_count ))
    }
  }
  
  text_match_df <- as.data.frame.list(colMeans(text_match_df[,10:39], na.rm = TRUE)) #finds average of all columns in analysis, unlists, and puts into dataframe
  text_match_df$word <- search_word
  print(search_word)
  # Gather means into one dataframe
  if (exists('text_merge') == FALSE) {    
    text_merge <- rbind.fill(text_df[!text_df$word == text_match_df$word,], text_match_df[,])
  } else {
    text_merge <- rbind.fill(text_merge[!text_merge$word == text_match_df$word,], text_match_df[,])
  }
  search_word <- paste("\\b", search_word, "\\b", sep = "") #grep finds exact match in DF to bring in count
  word_count <- text_df[grep(search_word, tolower(text_df$word)), which(colnames(text_df)=="n")] #return number of times word appears in text_merge
  # print(paste(word_count, search_word, sep = ": "))
  text_merge[grep(search_word, tolower(text_merge$word)), which(colnames(text_merge)=="n")] <- word_count  #put back value in dataframe
}

text_merge <- text_merge[!is.nan(text_merge$open0_change),] #remove all NaN from dataframe

for (word in c(t(text_merge$word))) {
  stock_count <- length(unique(df[unlist(as.data.frame(grep(word, tolower(df$article)))),1:3]$stock)) #find count of unique stocks per search word
  if(!stock_count > 2) { 
    text_merge <- text_merge[!text_merge$word == word,]
    print(paste("Removing word: ", word))
  }
}

observation <- arrange(text_merge, desc(text_merge$open1_change)) %>% slice(1:15) #top ten words of highest value change
word_list_from_observation <- df[unlist(as.data.frame(grep("ended", tolower(df$article)))),] #shows all articles from original dataframe with that word





## TEST CODE ##
## create a check that if all data comes from same stock remove from table, 
## or if company name in stock by doing a count if > 5, do this check earlier
text_merge <- text_merge[!is.nan(text_merge$open0_change),] #remove all NaN from dataframe

for (word in c(t(text_merge$word))) {
  stock_count <- length(unique(df[unlist(as.data.frame(grep(word, tolower(df$article)))),1:3]$stock)) #find count of unique stocks per search word
  if(!stock_count > 2) { 
    text_merge <- text_merge[!text_merge$word == word,]
    print(paste("Removing word: ", word))
  }
}


text_match_df <- df[unlist(as.data.frame(grep('therapeutics', tolower(df$article)))),]  #temp dataframe for combining into words table
company_names <- unique(df[unlist(as.data.frame(grep('therapeutics', tolower(df$article)))),1:3]$stock)
for (word in company_names) {
  stock_count <- sum(str_count(text_match_df$stock, paste("\\b", word ,"\\b", sep = "")))  #find count of unique stocks per search word 
  # Some words we know aren't company names
  if (word == "ATXI") {    
    print("word found")
  } else if (stock_count > 6) {
    print(paste("company name: ", word, " ", stock_count))
    text_match_df <- text_match_df[!text_match_df$stock == word,]
  } else {
    print(paste("not company: ", word, " ", stock_count ))
  }
}


words_to_ignore <- c("globenewswire", "earnings", "announces", "offering", "zacks")

for (word in words_to_ignore) {
  print(word)
  text_match_df <- df[unlist(as.data.frame(grep('therapeutics', tolower(df$article)))),]  #temp dataframe for combining into words table
  company_names <- unique(df[unlist(as.data.frame(grep('therapeutics', tolower(df$article)))),1:3]$stock)
  for (cn in company_names) {
    stock_count <- sum(str_count(text_match_df$stock, paste("\\b", cn ,"\\b", sep = "")))  #find count of unique stocks per search word 
    # Some words we know aren't company names
    if (word %in% words_to_ignore) {    
      print("word found")
      break()
    } else if (stock_count > 6) {
      print(paste("Company name: ", cn, " ", stock_count))
      text_match_df <- text_match_df[!text_match_df$stock == word,]
    } else {
      print(paste("Not company: ", cn, " ", stock_count ))
    }
  }
}


stock_count <- sum(str_count(text_match_df$stock, paste("\\b", word ,"\\b", sep = "")))
stock_count <- str_count(text_match_df$stock, "\\ATXI\\b")


# 2) do a second breakdown of words from already complete text analysis to find words in words with greatest jump

# --------- 1 ----------

word_list_from_observation <- df[unlist(as.data.frame(grep("globenewswire", tolower(df$article)))),] #shows all articles from original dataframe with that word










