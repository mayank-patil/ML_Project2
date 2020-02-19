#____________________________libraries______________________________________________
lib=c("quantmod","TTR","wikipediatrend","dplyr","ggplot2","reticulate","readr","e1071")
lapply(lib, require, character.only = TRUE)
Sys.which("python")
use_python("C:\\Python27\\python.exe", required = T)
setwd("F:/Games/finproj")
getwd()

### Get the input, stock ticker and date range


stock <- "AMZN"
terms <- c("Amazon Prime","AWS","Alexa Internet")
date_begin <- "1/1/2010"
date_end <- "12/1/2019"

### Prepare data



#Change the date format for "Quantmod" package
date_begin_m <- strptime(as.character(date_begin), "%m/%d/%Y")
date_end_m <- strptime(as.character(date_end), "%m/%d/%Y")
# Define the variables that we are going to use

Input_vars = c("Open","Close","High","Low","Wiki_5day_Disparity","Wiki_Move",
               "Wiki_MA3_Move","Wiki_EMA5_Move","Wiki_5day_Disparity_Move",
               "Google_EMA5_Move","Google_3day_Disparity_Move","Google_ROC_Move",
               "Google_RSI_Move","Wiki_3day_Disparity","Stochastic Oscillator",
               "RSI Move","Wiki_RSI_Move","Google_MA_6","Google_Move")

### Pull the market data

# Get the market data

getSymbols(stock, from=date_begin_m, to = date_end_m, src="yahoo")

# Create the date list

date_market <- data.frame(index(AMZN))   # ??? Change to data frame

# Get Open_Price, Close_Price, High_Price, Low_Price


data_from_yahoo <- AMZN # ? How to change this using input ???? try noquote(stock), not working
adjust_coff <- data_from_yahoo[,4]/data_from_yahoo[,6]  # Get the adjust index
Open_Price <- data_from_yahoo[,1]/adjust_coff   # Get the adjusted value based on index, similar below
Close_Price <- data_from_yahoo[,6]
High_Price <- data_from_yahoo[,2]/adjust_coff
Low_Price <- data_from_yahoo[,3]/adjust_coff
head(data_from_yahoo)


### Calulate the technical indicators

# Get the Stochastic Oscillator

data_for_sto <- data.frame(High_Price,Low_Price,Close_Price,row.names = NULL)
colnames(data_for_sto) <- c("High","Low","Close")    #Meet the format of fuction
full_sto <- data.frame(stoch(data_for_sto))
Stochastic_Oscillator <- full_sto$fastK * 100


# Get the RSI_Move
the_RSI <- RSI(Close_Price)     #Get the RSI
RSI_Move <- diff(the_RSI)       #Get the difference as previous day
RSI_Move[RSI_Move<0] <- 0       # 0 means going down
RSI_Move[RSI_Move>0] <- 1       # 1 means going up
RSI_Move <- data.frame(RSI_Move)  # Transfer to data frame


### Temporary full data

fulldata_temp <- data.frame(date_market,Open_Price,Close_Price,
                            High_Price,Low_Price, Stochastic_Oscillator,
                            RSI_Move,row.names = NULL)
colnames(fulldata_temp) <- c("Date","Open","Close","High","Low","Stochastic Oscillator",
                             "RSI Move")

str(fulldata_temp)

### Create the target

# Based on Target 2: O(i+1) - O(i)

Target <- diff(Open_Price)
Target_temp <- Target[2:length(Target),]
Target <- rbind.data.frame(Target_temp,"NA")
names(Target) <- c("Target")
Target=as.numeric(Target$Target)
Target[Target<0] <- 0       # 0 means going down
Target[Target>0] <- 1       # 1 means going up
Target <- data.frame(Target)  # Transfer to data frame
fulldata_temp <- cbind(fulldata_temp,Target)
str(fulldata_temp)

### Pull the Wikipedia data

# Get Wikipeida data                    Start from here
Wiki_traffic_ALL <- wp_trend(page = c(stock,terms),
                             from = date_begin_m,
                             to= date_end_m)          # Pull the data based on pre-defined terms
term_count <- length(terms) +1

# and stock ticker
while(nrow(Wiki_traffic_ALL) %% term_count != 0) {
  print(1)
  Wiki_traffic_ALL=Wiki_traffic_ALL[-1,]
}
#############
Wiki_traffic <- colSums(matrix(Wiki_traffic_ALL$views, nrow = term_count)) # Take the sum by each day for all search terms
Wiki_traffic_ALL$date <- as.Date(Wiki_traffic_ALL$date)
Wiki_traffic_date <- colSums(matrix(Wiki_traffic_ALL$date, nrow = term_count)/term_count) # Collect related date. To check the
# date, use as.Date()
Wiki_traffic_date=as.Date(Wiki_traffic_date)
Wiki_traffic_with_date <- data.frame(Wiki_traffic_date,Wiki_traffic)  # Combine the data
date_market_compare <- data.matrix(date_market)  # Tranfer from list to double for comparsion
date_market_compare = as.Date(date_market_compare)
#Compare the seq date with market open date
date_diff_wiki <- setdiff(Wiki_traffic_with_date$Wiki_traffic_date,date_market_compare) 
date_diff_wiki=as.Date(date_diff_wiki)
#Only keep the dates when market opens
Wiki_traffic_with_date_new <- Wiki_traffic_with_date[!Wiki_traffic_with_date$Wiki_traffic_date %in% date_diff_wiki,]
#This is the limitation, pull wiki traffic data might miss some data point
miss_wiki <- setdiff(date_market_compare,Wiki_traffic_with_date_new$Wiki_traffic_date)
miss_wiki=as.Date(miss_wiki)

### Calculate moving average of wiki traffic 

# Calculate the Wiki_Move
Wiki_traffic_market <- data.frame(Wiki_traffic_with_date_new$Wiki_traffic)

Wiki_Move <- diff(Wiki_traffic_with_date_new$Wiki_traffic)  #Get the difference as previous day
Wiki_Move[Wiki_Move<0] <- 0       # 0 means going down
Wiki_Move[Wiki_Move>0] <- 1       # 1 means going up
Wiki_Move <- data.frame(Wiki_Move) # Transfer to data frame
Wiki_Move <- rbind("N/A",Wiki_Move) # Move down for one row


# Calulate Wiki_MA3_Move, Wiki_EMA5_Move, Wiki_RSI_Move
Wiki_MA_3 <- SMA(Wiki_traffic_market, 3)  # 3 day Moving average of Wiki Traffic
Wiki_EMA_5 <- EMA(Wiki_traffic_market, 5) # 5 day Exponential moving average of Wiki Traffic
Wiki_RSI <- RSI(Wiki_traffic_market, maType = "WMA") # RSI of wiki traffic

Wiki_MA3_Move <-diff(Wiki_MA_3)
Wiki_MA3_Move[Wiki_MA3_Move<0] <- 0       # 0 means going down
Wiki_MA3_Move[Wiki_MA3_Move>0] <- 1       # 1 means going up
Wiki_MA3_Move <- data.frame(Wiki_MA3_Move)
Wiki_MA3_Move <- rbind("N/A",Wiki_MA3_Move) # Move down for one row

Wiki_EMA5_Move <- diff(Wiki_EMA_5)
Wiki_EMA5_Move[Wiki_EMA5_Move<0] <- 0       # 0 means going down
Wiki_EMA5_Move[Wiki_EMA5_Move>0] <- 1       # 1 means going up
Wiki_EMA5_Move <- data.frame(Wiki_EMA5_Move)
Wiki_EMA5_Move <- rbind("N/A",Wiki_EMA5_Move) # Move down for one row

Wiki_RSI_Move <- diff(Wiki_RSI)
Wiki_RSI_Move[Wiki_RSI_Move<0] <- 0       # 0 means going down
Wiki_RSI_Move[Wiki_RSI_Move>0] <- 1       # 1 means going up
Wiki_RSI_Move <- data.frame(Wiki_RSI_Move)
Wiki_RSI_Move <- rbind("N/A",Wiki_RSI_Move) # Move down for one row

# Calculate Wiki_5day_Disparity, Wiki_5day_Disparity_Move, Wiki_3day_Disparity

Wiki_MA_5 <- SMA(Wiki_traffic_market,5)  # 5 day moving average of Wiki Traffic
Wiki_3day_Disparity <- Wiki_traffic_market/Wiki_MA_3  #Please refer to Appendix II formula 3 
Wiki_5day_Disparity <- Wiki_traffic_market/Wiki_MA_5
Wiki_3day_Disparity <- as.numeric(unlist(Wiki_3day_Disparity))
Wiki_5day_Disparity <- as.numeric(unlist(Wiki_5day_Disparity))
Wiki_5day_Disparity_Move <- diff(Wiki_5day_Disparity)
Wiki_5day_Disparity_Move[Wiki_5day_Disparity_Move <0] <- 0       # 0 means going down
Wiki_5day_Disparity_Move[Wiki_5day_Disparity_Move >0] <- 1       # 1 means going up
Wiki_5day_Disparity_Move <- data.frame(Wiki_5day_Disparity_Move)
Wiki_5day_Disparity_Move <- rbind("N/A",Wiki_5day_Disparity_Move) # Move down for one row

### Temporary full data after Wiki date match

# Dealing with the missing data point after wiki
fulldata_temp <- fulldata_temp[!fulldata_temp$Date %in% miss_wiki,]
Wiki_data <- data.frame(Wiki_Move,Wiki_MA3_Move,Wiki_EMA5_Move, Wiki_RSI_Move,Wiki_5day_Disparity, 
                        Wiki_5day_Disparity_Move, Wiki_3day_Disparity)
fulldata_temp <- cbind(fulldata_temp,Wiki_data)

### Pull Google Data

# Load Google Web Scraping google.py
# Function: getNewsCount (term, begDate, endDate) call this function use python.call
# python.call("getNewsCount", stock, a[3], date_end)

#Load python function
source_python("F:/Games/finproj/google.py")

date_format_google <- format(fulldata_temp$Date,"%m/%d/%Y") #format the date to "01/01/2013"
#Create a dummy vector of zeros
Google_counts <- rep(0,length(date_format_google))
str(Google_counts)
#Get the google news count by each day
for (i in 1:length(date_format_google)) {
  Google_counts[i] = getNewsCount(stock, date_format_google[i], date_end)
}

# Combine the date with google counts to double check the data
Google_counts_with_date <- data.frame(fulldata_temp$Date, Google_counts)

#Get Google_EMA5_Move
Google_counts_market <- as.numeric(gsub(",","",Google_counts))   #Dealing with the 1,234 format to 1234

Google_counts_market=na.approx(Google_counts_market) #handling 'Series contains non-leading NAs' in TTR library with xts objects?


### Calculate moving averages of google traffic

#Get Google_EMA5_Move

Google_EMA_5 <- EMA(Google_counts_market,5)

Google_EMA5_Move <- diff(Google_EMA_5)
Google_EMA5_Move[Google_EMA5_Move <0] <- 0       # 0 means going down
Google_EMA5_Move[Google_EMA5_Move >0] <- 1       # 1 means going up
Google_EMA5_Move <- data.frame(Google_EMA5_Move)
Google_EMA5_Move <- rbind("N/A",Google_EMA5_Move) # Move down for one row

#Get Google_MA_6
Google_MA_6 <- SMA(Google_counts_market,6)

#Get Google_Move
Google_Move <- diff(Google_counts_market)  #Get the difference as previous day
Google_Move[Google_Move<0] <- 0       # 0 means going down
Google_Move[Google_Move>0] <- 1       # 1 means going up
Google_Move <- data.frame(Google_Move) # Transfer to data frame
Google_Move <- rbind("N/A",Google_Move) # Move down for one row

#Get Google_3day_Disparity_Move
Google_MA_3 <- SMA(Google_counts_market,3)
Google_3day_Disparity <- Google_counts_market/Google_MA_3
Google_3day_Disparity <- as.numeric(unlist(Google_3day_Disparity))

Google_3day_Disparity_Move <- diff(Google_3day_Disparity)
Google_3day_Disparity_Move[Google_3day_Disparity_Move <0] <- 0       # 0 means going down
Google_3day_Disparity_Move[Google_3day_Disparity_Move >0] <- 1       # 1 means going up
Google_3day_Disparity_Move <- data.frame(Google_3day_Disparity_Move)
Google_3day_Disparity_Move <- rbind("N/A",Google_3day_Disparity_Move) # Move down for one row

#Get Google_RSI_Move
Google_RSI <- RSI(Google_counts_market, maType = "WMA")

Google_RSI_Move <- diff(Google_RSI)
Google_RSI_Move[Google_RSI_Move<0] <- 0       # 0 means going down
Google_RSI_Move[Google_RSI_Move>0] <- 1       # 1 means going up
Google_RSI_Move <- data.frame(Google_RSI_Move)
Google_RSI_Move <- rbind("N/A",Google_RSI_Move) # Move down for one row


Google_ROC <- ROC(Google_counts_market,n=5) * 100

Google_ROC_Move <- diff(Google_ROC)
Google_ROC_Move[Google_ROC_Move<0] <- 0       # 0 means going down
Google_ROC_Move[Google_ROC_Move>0] <- 1       # 1 means going up
Google_ROC_Move <- data.frame(Google_ROC_Move)
Google_ROC_Move <- rbind("N/A",Google_ROC_Move) # Move down for one row"""

### Temporary full data after Wiki and Google

# Dealing with the missing data point after wiki
Google_data <- data.frame(Google_EMA5_Move,Google_MA_6,Google_Move,
                          Google_3day_Disparity_Move,Google_RSI_Move,
                          Google_ROC_Move)
fulldata_temp <- cbind(fulldata_temp,Google_data)
head(fulldata_temp,20)

### Finalize the data
##### Preparing data for Fitting Model

fulldata <- fulldata_temp[20:nrow(fulldata_temp),]
move_cols <- sapply(fulldata, is.character)
move_cols[["RSI Move"]] <- TRUE
move_cols[["Target"]] <- TRUE
move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
move_col_names <- names(move_data)
non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
fulldata <- cbind(non_move_data,move_data)

Target <- fulldata$Target
fulldata$Target <- NULL
fulldata <- cbind(fulldata,Target)
fulldata <- fulldata[,2:ncol(fulldata)]
numRows = dim(fulldata)[1]
lastday = fulldata[numRows, ] 
fulldata = fulldata[1:numRows-1, ]
fulldata = fulldata[names(fulldata) %in% Input_vars]


#write.csv("stock.csv")

### Model Fitting
##### Applying 2 algorithm 
###### 1. Decision Tree
###### 2. SVM

#Decison Tree Classifier


fulldata=read.csv("stock.csv")

# libraries
#install.packages("C50")
library(caret)
library(C50)

fulldata$Target = as.factor(fulldata$Target)
accuracy=c()
for (i in 1:100){
  #assign 80% of the data to the training set
  train.index = createDataPartition(fulldata$Target,p=0.80,list = F)
  train <- fulldata[train.index,]
  test = fulldata[-train.index,]
  #build model using training data
  c50_model = C5.0(Target ~.,train,trials=100,rules=T)
  #calculate accuracy on test data
  c50predict = predict(c50_model,test[,-20],type="class")
  accuracy[i] <- mean(c50predict == test$Target)
}
a=mean(accuracy) 
b=sd(accuracy)

mean(accuracy)

fulldata=read.csv("stock.csv")

#assign 80% of the data to the training set

fulldata[, "train"] <- ifelse(runif(nrow(fulldata)) < 0.8, 1, 0)
trainColNum <- grep("train", names(fulldata))
trainset <- fulldata[fulldata$train == 1, -trainColNum]
testset <- fulldata[fulldata$train == 0, -trainColNum]

accuracy=c()
for (i in 1:100){
  #assign 80% of the data to the training set
  fulldata[, "train"] <- ifelse(runif(nrow(fulldata)) < 0.8, 1, 0)
  trainColNum <- grep("train", names(fulldata))
  trainset <- fulldata[fulldata$train == 1, -trainColNum]
  testset <- fulldata[fulldata$train == 0, -trainColNum]
  #build model using training data
  svm_model <- svm(Target~ ., data = trainset, 
                   type = "C-classification", kernel = "linear")
  #calculate accuracy on test data
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$Target)
}


mean(accuracy)

##Conclusion
#### we got better accuracy with SVM 

