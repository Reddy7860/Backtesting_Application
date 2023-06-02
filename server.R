
server <- function(input, output,session) {
  
  
  get_signals_data <- function(){
    
    browser()
    
    stock <- input$backtest_stock
    
    strategy <- input$bot_strategy
    
    starttime <- as.integer(as.POSIXct(input$backtest_range[1]))
    endtime <- as.integer(as.POSIXct(input$backtest_range[2]))
    
    money_control_stock <- toupper(gsub("\\..*","",stock))
    
    final_data_combined = data.frame()
    target_combined_data = data.frame()
    
    
    today = Sys.Date()
    f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
    previousWorkingDay <- f(today)
    if(stock == "%5ENSEBANK"){
      
      stock_data <- read.csv(paste0(getwd(),"/data/nifty_bank/day.csv"))
      stock_data <- stock_data[stock_data$date >= input$backtest_range[1] & stock_data$date < input$backtest_range[2],]
      stock_data <- stock_data[!duplicated(stock_data), ]
      
      row.names(stock_data) <- NULL
      
    }else if(stock == "%5ENSEI"){
      stock_data <- read.csv(paste0(getwd(),"/data/nifty/day.csv"))
      stock_data <- stock_data[stock_data$date >= input$backtest_range[1] & stock_data$date < input$backtest_range[2],]
      stock_data <- stock_data[!duplicated(stock_data), ]
      
      row.names(stock_data) <- NULL
      
    }
    # else if(stock == "IBM.NS"){
    #   stock_data <- read.csv(paste0(getwd(),"/data/IBM/ibm_5_minutes_data.csv"))
    #   
    #   # convert datetime format
    #   stock_data$date <- dmy_hms(stock_data$date) # parse input format as day-month-year hour:minute:second
    #   stock_data$date <- format(stock_data$date, "%d-%m-%Y %H:%M:%S")
    #   
    #   # convert the date column to the correct format
    #   stock_data$dates <- as.Date(stock_data$date, format = "%d-%m-%Y")
    #   # add the dates column to the data frame
    #   stock_data$dates <- format(stock_data$dates, "%Y-%m-%d")
    #   
    #   stock_data <- stock_data[stock_data$dates >= input$backtest_range[1] & stock_data$dates < input$backtest_range[2],]
    #   stock_data <- stock_data[!duplicated(stock_data), ]
    #   
    #   row.names(stock_data) <- NULL
    #   
    # }
    else{
      stock_data <- na.omit(getSymbols(stock, src = "yahoo", from = "2018-01-01", to = previousWorkingDay, auto.assign = FALSE))
      stock_data <- data.frame(Date = index(stock_data), coredata(stock_data))
    }
    
    
    if(strategy == "cowboy"){
      
      # browser()
      
      
      
      ####  Cowboy strategy Overview
      
      #### Creating the levels at Daily level
      
      stock_data["Rider_Bullish"] = "No"
      stock_data["Bullish_Level"] = 100000
      stock_data["Rider_Bearish"] = "No"
      stock_data["Bearish_Level"] = 0
      
      for(i in 4:nrow(stock_data)){
        if(abs((stock_data[i,3] - stock_data[(i-1),3])/(stock_data[(i-1),3])*100) < 0.5){
          stock_data[i,"Rider_Bullish"] = "Yes"
          stock_data[i,"Bullish_Level"] = max(stock_data[(i-1),3],stock_data[(i-1),3])
        }
        else{
          stock_data[i,"Rider_Bullish"] = "No"
          stock_data[i,"Bullish_Level"] = 100000
        }
        if(abs((stock_data[i,4] - stock_data[(i-1),4])/(stock_data[(i-1),4])*100) < 0.5){
          stock_data[i,"Rider_Bearish"] = "Yes"
          stock_data[i,"Bearish_Level"]= min(stock_data[i,4],stock_data[(i-1),4])
        }
        else{
          stock_data[i,"Rider_Bearish"] = "No"
          stock_data[i,"Bearish_Level"] = 0
        }
      }
      
      browser()
      
      
      # money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
      # 
      # row_number <- which(grepl(stock, money_control_data$Company))
      # 
      # money_control_stock <- money_control_data[row_number,5]
      
      
      # starttime <- as.integer(as.POSIXct("2020-05-01"))
      # endtime <- as.integer(as.POSIXct("2021-05-20"))
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
          
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))  
        }
        
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
      }else if(stock == "IBM"){
        response_data <- read.csv(paste0(getwd(),"/data/IBM/ibm_5_minutes_data.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        
        # convert datetime format
        final_data$date <- dmy_hms(final_data$date) # parse input format as day-month-year hour:minute:second
        final_data$date <- format(final_data$date, "%d-%m-%Y %H:%M:%S")
        
        # convert the date column to the correct format
        final_data$dates <- as.Date(final_data$date, format = "%d-%m-%Y")
        # add the dates column to the data frame
        final_data$dates <- format(final_data$dates, "%Y-%m-%d")
        
        final_data <- final_data[final_data$dates >= input$backtest_range[1] & final_data$dates < input$backtest_range[2],]
        final_data = final_data[c("date","open","high","low","close","volume")]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      
      
      
      
      
      ### Checking the conditions met cases
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      if(stock == "IBM"){
        # convert the date column to the correct format
        stock_5_min_historic_data$date <- as.Date(stock_5_min_historic_data$Datetime, format = "%d-%m-%Y")
        # add the dates column to the data frame
        stock_5_min_historic_data$date <- format(stock_5_min_historic_data$date, "%Y-%m-%d")
        
      }
      else{
        stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      }
      
      
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      increment = 1
      Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0),"Date"=character(0),"StopLoss"=character(0),"Target"=character(0))
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        if(stock == "%5ENSEBANK"){
          temp_stock <- stock_data[as.Date(stock_data$date) == previous_date,]
        }else  if(stock == "%5ENSEI"){
          temp_stock <- stock_data[as.Date(stock_data$date) == previous_date,]
        }else{
          temp_stock <- stock_data[stock_data$Date == previous_date,]  
        }
        
        # if(previous_date == as.Date("2021-05-14")){
          # browser()
          # print("Hello")
          # 
          
          # print(temp_stock)
          if(nrow(temp_stock) > 0){
            # browser()
            # temp_stock <- temp_stock[!duplicated(temp_stock), ]
            rownames(temp_stock) <- 1
            # print(i)
            # print(temp_stock[1,8])
            
            if(temp_stock[1,8] == "Yes"){
              satisfied_df = data.frame()
              
              for(i in 1:nrow(current_data)){
                if((current_data[i,"Close"]) > temp_stock[1,9]){
                  satisfied_df = rbind(satisfied_df,current_data[i,])
                  # print(satisfied_df)
                }
                else{
                  next
                }
              }
              if(nrow(satisfied_df) == 0){
                next
              }
              else{
                
                # browser()
                satisfied_df = head(satisfied_df,1)
                # print(satisfied_df)
                rownames(satisfied_df) <- 1
                
                # if(as.Date(satisfied_df[1,"Datetime"]) == "2022-06-09"){
                #   browser()
                # }
                
                if(stock == "%5ENSEI"){
                  # curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"])),tz="Asia/Kolkata"))
                  # curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"])),tz="Asia/Kolkata"))
                  
                  datetime_str <- sub("\\+.*$", "", satisfied_df[1,"Datetime"])
                  datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Kolkata")
                  curr_hr <- format(datetime, format = "%H")
                  curr_min <- format(datetime, format = "%M")
                  
                }else if(stock == "%5ENSEBANK"){
                  # curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"])),tz="Asia/Kolkata"))
                  # curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"])),tz="Asia/Kolkata"))
                  
                  datetime_str <- sub("\\+.*$", "", satisfied_df[1,"Datetime"])
                  datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Kolkata")
                  curr_hr <- format(datetime, format = "%H")
                  curr_min <- format(datetime, format = "%M")
                  
                }else if(stock == "IBM"){
                  # browser()
                  # Convert to datetime object
                  datetime <- as.POSIXct(satisfied_df[1,"Datetime"], format="%d-%m-%Y %H:%M:%S")
                  
                  # Extract the hour
                  curr_hr <- format(datetime, format="%H")
                  curr_min <- format(datetime, format="%M")
                  # curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"])),tz="Asia/Kolkata"))
                  # curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"])),tz="Asia/Kolkata"))
                }else{
                  curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
                  curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
                  
                }
                
                
                
                if(curr_hr == 15 && curr_min >= 15){
                  next
                }else{
                  
                  
                  Signal_df[increment,"Strategy"] <- "Cowboy"
                  Signal_df[increment,"Stock"]=stock
                  Signal_df[increment,"Signal"]="Buy"
                  Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
                  Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
                  
                  increment = increment + 1
                  
                }
              }
            }
            else{
              next
            }
            
            if(temp_stock[1,10] == "Yes"){
              satisfied_df = data.frame()
              
              for(i in 1:nrow(current_data)){
                if((current_data[i,"Close"]) < temp_stock[1,11]){
                  # print("Sell")
                  satisfied_df = rbind(satisfied_df,current_data[i,])
                  # print(satisfied_df)
                }
                else{
                  next
                }
                
              }
              
              # browser()
              if(nrow(satisfied_df) == 0){
                next
              }
              else{
                satisfied_df = head(satisfied_df,1)
                # browser()
                
                if(stock == "%5ENSEBANK"){
                  # curr_hr <- hour(satisfied_df[1,"Datetime"])
                  # curr_min <- minute(satisfied_df[1,"Datetime"])
                  datetime_str <- sub("\\+.*$", "", satisfied_df[1,"Datetime"])
                  datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Kolkata")
                  curr_hr <- format(datetime, format = "%H")
                  curr_min <- format(datetime, format = "%M")
                }else if(stock == "%5ENSEI"){
                  # curr_hr <- hour(satisfied_df[1,"Datetime"])
                  # curr_min <- minute(satisfied_df[1,"Datetime"])
                  datetime_str <- sub("\\+.*$", "", satisfied_df[1,"Datetime"])
                  datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Kolkata")
                  curr_hr <- format(datetime, format = "%H")
                  curr_min <- format(datetime, format = "%M")
                }else if(stock == "IBM"){
                  # Convert to datetime object
                  datetime <- as.POSIXct(satisfied_df[1,"Datetime"], format="%d-%m-%Y %H:%M:%S")
                  
                  # Extract the hour
                  curr_hr <- format(datetime, format="%H")
                  curr_min <- format(datetime, format="%M")
                }else{
                  curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
                  curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
                }
                
                
                
                if(curr_hr == 15 && curr_min >= 15){
                  next
                }else{
                  rownames(satisfied_df) <- 1
                  Signal_df[increment,"Strategy"] <- "Cowboy"
                  Signal_df[increment,"Stock"]=stock
                  Signal_df[increment,"Signal"]="Sell"
                  Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
                  Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
                  increment = increment + 1
                }
                
              }
            }
            else{
              next
            }
          }
        # }
        
        
        
      }
      
      
    }else if (strategy == "sweths_violation"){
      
      # browser()
      
      Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
        }
        
        
        
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      for(i in 1:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        rownames(current_data) <- 1:nrow(current_data)
        
        current_date <- current_data[1,"date"]
        
        # print(current_data)
        
        trigger_price = 0
        stage = ""
        
        if((current_data[1,"Close"]>current_data[1,"Open"]) && abs(current_data[1,"Close"] - current_data[1,"Open"])>= 0.7*abs(current_data[1,"High"] - current_data[1,"Low"])){
          trigger_price = current_data[1,"Low"]
          stage = "Green"
        }
        else if((current_data[1,"Close"]<current_data[1,"Open"]) && abs(current_data[1,"Close"] - current_data[1,"Open"])>= 0.7*abs(current_data[1,"High"] - current_data[1,"Low"])){
          trigger_price = current_data[1,"High"]
          stage = "Red"
        }
        else{
          next
        }
        
        satisfied_df = data.frame()
        
        for(j in 5:nrow(current_data)){
          
          if(stage == "Green"){
            # browser()
            if(current_data[j,"Close"] < trigger_price){
              satisfied_df = rbind(satisfied_df,current_data[j,])
              call ="Sell"
              # print(stock)
              # print("Moving up")
            }
          }
          else if(stage == "Red"){
            if(current_data[j,"Close"] > trigger_price){
              satisfied_df = rbind(satisfied_df,current_data[j,])
              call = "Buy"
              # print(stock)
              # print("Moving Down")
            }
          }
          else{
            next
          }
          
        }
        
        if(nrow(satisfied_df) == 0){
          next
        }
        else{
          # browser()
          # print(satisfied_df)
          satisfied_df = head(satisfied_df,1)
          
          if(stock == "%5ENSEBANK"){
            curr_hr <- hour(satisfied_df[1,"Datetime"])
            curr_min <- minute(satisfied_df[1,"Datetime"])
          }else if(stock == "%5ENSEI"){
            curr_hr <- hour(satisfied_df[1,"Datetime"])
            curr_min <- minute(satisfied_df[1,"Datetime"])
          }else{
            curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
            curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
          }
          
          if(curr_hr == 15 && curr_min >= 15){
            next
          }else{
            
            Signal_df[increment,"Strategy"] <- "Sweths Violation"
            Signal_df[increment,"Stock"]=stock
            Signal_df[increment,"Signal"]=call
            Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
            Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
            
            increment = increment + 1
          }
        }
        
        
        
      }
      
      print(Signal_df)
      
    }else if(strategy == "reds_rocket"){
      
      # browser()
      
      
      
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Reds_High" = numeric(0),"Reds_Low"=numeric(0))
      
      inc = 1
      for(i in 4:nrow(stock_data)){
        
        current_date <- stock_data[i,1]
        
        l1_day_range <- abs(stock_data[i,3] - stock_data[i,4])
        l2_day_range <- abs(stock_data[(i-1),3] - stock_data[(i-1),4])
        l3_day_range <- abs(stock_data[(i-2),3] - stock_data[(i-2),4])
        l4_day_range <- abs(stock_data[(i-3),3] - stock_data[(i-3),4])
        
        l2_day_high <- stock_data[(i-1),3]
        l1_day_high <- stock_data[i,3]
        
        l2_day_low <- stock_data[(i-1),4]
        l1_day_low <- stock_data[i,4]
        
        if((l1_day_range < l2_day_range) && (l1_day_range < l3_day_range) && (l1_day_range < l4_day_range)){
          if(l1_day_low > l2_day_low && l1_day_high < l2_day_high){
            # print(current_date)
            final_levels_df[inc,"Date"] = current_date
            final_levels_df[inc,"Stock"] = stock
            final_levels_df[inc,"Reds_High"] = l1_day_high
            final_levels_df[inc,"Reds_Low"] = l1_day_low
            
            
            
            inc = inc + 1
          }
          
        }
        else{
          next
        }
        
      }
      
      stock_data <- final_levels_df
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
        }
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        # temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        # print(temp_stock)
        
        if(stock == "%5ENSEBANK"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else if(stock == "%5ENSEI"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else{
          temp_stock <- stock_data[anydate(as.numeric(stock_data$Date)) == previous_date,]  
        }
        
        
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          
          # print(temp_stock)
          
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          for(j in 1:nrow(current_data)){
            
            
            if((current_data[j,"Close"]) > temp_stock[1,"Reds_High"]){
              # print(satisfied_df)
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Buy")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
            }else if((current_data[j,"Close"]) < temp_stock[1,"Reds_Low"]){
              
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Sell")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
            }
            else{
              next
            }
          }
          # # print(nrow(satisfied_df))
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            
            if(stock == "%5ENSEBANK"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else if(stock == "%5ENSEI"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else{
              curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
            }
            
            if(curr_hr == 15 && curr_min >= 15){
              next
            }else{
              
              Signal_df[increment,"Strategy"] <- "Reds Rocket"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              
              increment = increment + 1
            }
          }
          
          
        }
        
      }
      
    }else if(strategy == "reds_brahmos"){
      
      # browser()
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Reds_High" = numeric(0),"Reds_Low"=numeric(0))
      
      inc = 1
      
      for(i in 6:nrow(stock_data)){
        
        current_date <- stock_data[i,1]
        
        l1_day_range <- abs(stock_data[i,3] - stock_data[i,4])
        l2_day_range <- abs(stock_data[(i-1),3] - stock_data[(i-1),4])
        l3_day_range <- abs(stock_data[(i-2),3] - stock_data[(i-2),4])
        l4_day_range <- abs(stock_data[(i-3),3] - stock_data[(i-3),4])
        l5_day_range <- abs(stock_data[(i-4),3] - stock_data[(i-4),4])
        l6_day_range <- abs(stock_data[(i-5),3] - stock_data[(i-5),4])
        
        l2_day_high <- stock_data[(i-1),3]
        l1_day_high <- stock_data[i,3]
        
        l2_day_low <- stock_data[(i-1),4]
        l1_day_low <- stock_data[i,4]
        
        if((l1_day_range < l2_day_range) && (l1_day_range < l3_day_range) && (l1_day_range < l4_day_range) && (l1_day_range < l5_day_range) && (l1_day_range < l6_day_range)){
          
          final_levels_df[inc,"Date"] = current_date
          final_levels_df[inc,"Stock"] = stock
          final_levels_df[inc,"Reds_High"] = l1_day_high
          final_levels_df[inc,"Reds_Low"] = l1_day_low
          
          inc = inc + 1
        }
        
      }
      
      # browser()
      
      stock_data <- final_levels_df
      
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
        }
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        # temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        
        if(stock == "%5ENSEBANK"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else if(stock == "%5ENSEI"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else{
          temp_stock <- stock_data[anydate(as.numeric(stock_data$Date)) == previous_date,]
        }
        
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          
          # print(temp_stock)
          
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          for(j in 1:nrow(current_data)){
            
            
            if((current_data[j,"Close"]) > temp_stock[1,"Reds_High"]){
              # print(satisfied_df)
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Buy")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
            }else if((current_data[j,"Close"]) < temp_stock[1,"Reds_Low"]){
              
              satisfied_df = rbind(satisfied_df,current_data[j,])
              rownames(satisfied_df) <- 1:nrow(satisfied_df)
              # print("Sell")
              # print(satisfied_df)
              satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
            }
            else{
              next
            }
          }
          # # print(nrow(satisfied_df))
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            # print(satisfied_df)
            
            if(stock == "%5ENSEBANK"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else if(stock == "%5ENSEI"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else{
              curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
            }
            
            
            
            if(curr_hr == 15 && curr_min >= 15){
              next
            }else{
              
              Signal_df[increment,"Strategy"] <- "Reds Rocket"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              
              increment = increment + 1
            }
          }
          
          
        }
        
      }
      
      
    }else if(strategy == "blackout"){
      
      
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"target" = numeric(0),"stage" = character(0))
      
      inc = 1
      
      for(i in 4:nrow(stock_data)){
        
        current_date <- stock_data[i,1]
        
        l4_high <- stock_data[(i-3),3]
        l3_high <- stock_data[(i-2),3]
        l2_high <- stock_data[(i-1),3]
        l1_high <- stock_data[i,3]
        
        
        l4_low <- stock_data[(i-3),4]
        l3_low <- stock_data[(i-2),4]
        l2_low <- stock_data[(i-1),4]
        l1_low <- stock_data[i,4]
        
        
        if((l1_low > l2_low) && (l1_high > l2_high) && (l2_low > l3_low) && (l2_high > l3_high) && (l3_low > l4_low) && (l3_high > l4_high)){
          l1_open <- stock_data[i,2]
          l1_close <- stock_data[i,5]
          real_body <- abs(l1_open - l1_close)
          body_high <- max(l1_open,l1_close)
          if((l1_high - body_high) > 2*(real_body)){
            final_levels_df[inc,"Stock"] = stock
            final_levels_df[inc,"target"] = l1_low
            final_levels_df[inc,"stage"] = "Short"
            final_levels_df[inc,"Date"] = current_date
            inc = inc + 1
          }
          
          
        }
        else if((l1_low < l2_low) && (l1_high < l2_high) && (l2_low < l3_low) && (l2_high < l3_high) && (l3_low < l4_low) && (l3_high < l4_high)){
          l1_open <- stock_data[i,2]
          l1_close <- stock_data[i,5]
          real_body <- abs(l1_open - l1_close)
          body_low <- min(l1_open,l1_close)
          if((l1_low - body_low) > 2*(real_body)){
            final_levels_df[inc,"Stock"] = stock
            final_levels_df[inc,"target"] = l1_high
            final_levels_df[inc,"stage"] = "Long"
            final_levels_df[inc,"Date"] = current_date
            inc = inc + 1
          }
          
          
        }
        else{
          next
        }
        
      }
      
      stock_data <- final_levels_df
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
        }
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      # stock_5_min_historic_data <- read.csv("data/Reliance 5 Min Data.csv")
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        # temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        
        if(stock == "%5ENSEBANK"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else if(stock == "%5ENSEI"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else{
          temp_stock <- stock_data[anydate(as.numeric(stock_data$Date)) == previous_date,]
        }
        
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          rownames(temp_stock) <- 1
          # print(temp_stock)
          stage <- temp_stock[1,"stage"]
          target_value <- temp_stock[1,"target"]
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          if(stage == "Short"){
            for(j in 1:nrow(current_data)){
              if((current_data[j,"Close"]) < target_value){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }
              
            }
          }
          else{
            for(j in 1:nrow(current_data)){
              if((current_data[j,"Close"]) > target_value){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              
            }
          }
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            
            if(stock == "%5ENSEBANK"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else if(stock == "%5ENSEI"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else{
              curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
            }
            
            
            
            if(curr_hr == 15 && curr_min >= 15){
              next
            }else{
              
              # print(satisfied_df)
              Signal_df[increment,"Strategy"] <- "Blackout"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              
              increment = increment + 1
            }
          }
          
        }
      }
    }else if(strategy == "gap_up"){
      browser()
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Previous_Open" = numeric(0),"Previous_High"=numeric(0),"Previous_Low"=numeric(0),"Previous_Close"=numeric(0))
      
      inc = 1
      
      for(i in 2:nrow(stock_data)){
        
        # current_date <- stock_data[i,1]
        current_stock_data <- stock_data[i-1,]
        
        current_stock_data <- as.data.frame(current_stock_data)
        
        rownames(current_stock_data) <- 1
        
        final_levels_df[inc,"Stock"] = stock
        final_levels_df[inc,"Previous_Open"] = current_stock_data[1,2]
        final_levels_df[inc,"Previous_High"] = current_stock_data[1,3]
        final_levels_df[inc,"Previous_Low"] = current_stock_data[1,4]
        final_levels_df[inc,"Previous_Close"] = current_stock_data[1,5]
        final_levels_df[inc,"Date"] = current_stock_data[1,1]
        
        inc = inc + 1
        
      }
      
      stock_data <- final_levels_df
      
      stock_data <- stock_data[!duplicated(stock_data), ]
      rownames(stock_data) <- NULL
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
        }
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        # temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        
        # browser()
        if(stock == "%5ENSEBANK"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else if(stock == "%5ENSEI"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else{
          temp_stock <- stock_data[anydate(as.numeric(stock_data$Date)) == previous_date,]  
        }
        
        
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          
          # browser()
          
          rownames(temp_stock) <- 1
          # print(temp_stock)
          # stage <- temp_stock[1,"stage"]
          # target_value <- temp_stock[1,"target"]
          
          high_price = temp_stock[1,"Previous_High"]
          previous_close = temp_stock[1,"Previous_Close"]
          
          
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          
          open_price = current_data[1,2]
          close_price = current_data[1,5]
          
          if(open_price > previous_close){
            
            rownames(current_data) <- 1:nrow(current_data)
            
            for(j in 5:nrow(current_data)){
              current_date <- current_data[j, "Datetime"]
              
              day_high <- max(na.omit(c(max(current_data[1:j-1, "Close"], na.rm = TRUE), max(current_data[1:j-1, "Open"], na.rm = TRUE))))
              
              day_low <- min(na.omit(c(min(current_data[1:j-1, "Close"], na.rm = TRUE), min(current_data[1:j-1, "Open"], na.rm = TRUE))))
              
              low_range <- min(current_data[j-1, "Low"], current_data[j-2, "Low"], current_data[j-3, "Low"], current_data[j-4, "Low"])
              high_range <- max(current_data[j-1, "High"], current_data[j-2, "High"], current_data[j-3, "High"], current_data[j-4, "High"])
              
              current_close <- current_data[j, "Close"]
              
              if ((abs(high_range - low_range)/low_range * 100 < 0.4) && (current_close >= high_price) && (current_close >= day_high)){
                # browser()
                
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
                
                
              } else if ((abs(high_range - low_range)/low_range * 100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
                # browser()
                
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
                
              }
            }
            
            
          }
          
          # browser()
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            
            if(stock == "%5ENSEBANK"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else if(stock == "%5ENSEI"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else{
              curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
            }
            
            
            
            if(curr_hr == 15 && curr_min >= 15){
              next
            }else{
              
              # print(satisfied_df)
              Signal_df[increment,"Strategy"] <- "Gap_up"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              
              increment = increment + 1
              
            }
          }
          
        }
      }
    }else if(strategy == "gap_down"){
      # print("pass")
      
      
      final_levels_df = data.frame("Date"=numeric(0),"Stock" = character(0),"Previous_Open" = numeric(0),"Previous_High"=numeric(0),"Previous_Low"=numeric(0),"Previous_Close"=numeric(0))
      
      inc = 1
      
      for(i in 2:nrow(stock_data)){
        
        # current_date <- stock_data[i,1]
        current_stock_data <- stock_data[i-1,]
        
        current_stock_data <- as.data.frame(current_stock_data)
        
        rownames(current_stock_data) <- 1
        
        final_levels_df[inc,"Stock"] = stock
        final_levels_df[inc,"Previous_Open"] = current_stock_data[1,2]
        final_levels_df[inc,"Previous_High"] = current_stock_data[1,3]
        final_levels_df[inc,"Previous_Low"] = current_stock_data[1,4]
        final_levels_df[inc,"Previous_Close"] = current_stock_data[1,5]
        final_levels_df[inc,"Date"] = current_stock_data[1,1]
        
        inc = inc + 1
        
      }
      
      stock_data <- final_levels_df
      
      stock_data <- stock_data[!duplicated(stock_data), ]
      rownames(stock_data) <- NULL
      
      if(stock == "%5ENSEBANK"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else if(stock == "%5ENSEI"){
        if(input$backtest_timeframe == "15m"){
          response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
        }else{
          response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
        }
        final_data <- response_data %>% select(date, open, high, low, close,volume)
        final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
        colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
        stock_5_min_historic_data <- final_data
        
      }else{
        
        response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        
        print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
        stock_timestamp <- response_data$t
        Close <- response_data$c
        High <- response_data$h
        Low <- response_data$l
        Open <- response_data$o
        Volume <- response_data$v
        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        
        colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }else{
          final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
        }
        
        stock_5_min_historic_data <- na.omit(final_data)
        
      }
      
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      
      Signal_df = data.frame("Strategy"=character(0),"Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
      
      
      increment = 1
      
      
      for(i in 2:max(final_5_min_stocks$dns_rank)){
        
        
        
        current_data <- final_5_min_stocks[final_5_min_stocks$dns_rank == i,]
        
        temp_data2 <- final_5_min_stocks[final_5_min_stocks$dns_rank == i-1,]
        
        current_date <- current_data[1,"date"]
        previous_date <- temp_data2[1,"date"]
        # print(current_date)
        # print(previous_date)
        # temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        
        # browser()
        if(stock == "%5ENSEBANK"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else if(stock == "%5ENSEI"){
          temp_stock <- stock_data[as.Date(stock_data$Date) == previous_date,]
        }else{
          temp_stock <- stock_data[anydate(as.numeric(stock_data$Date)) == previous_date,]  
        }
        
        
        # print(temp_stock)
        if(nrow(temp_stock) > 0){
          
          # browser()
          
          rownames(temp_stock) <- 1
          # print(temp_stock)
          # stage <- temp_stock[1,"stage"]
          target_value <- temp_stock[1,"target"]
          
          high_price = temp_stock[1,"Previous_High"]
          previous_close = temp_stock[1,"Previous_Close"]
          prev_low_price = temp_final_levels_df[ind,"Previous_Low"]
          
          
          current_data$Call <- ""
          
          satisfied_df = data.frame(dates = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
          
          
          open_price = current_data[1,2]
          close_price = current_data[1,5]
          
          if(open_price < previous_close){
            
            
            for(j in 5:nrow(current_data)){
              # if(stock == "UPL.NS"){
              
              current_date <- current_data[i,"dates"]
              # print(current_date)
              
              day_high <- max(current_data$Close)
              day_low <- min(current_data$Low)
              
              low_range <- min(current_data[j-1,4],current_data[j-2,4],current_data[j-3,4],current_data[j-4,4])
              high_range <- max(current_data[j-1,3],current_data[j-2,3],current_data[j-3,3],current_data[j-4,3])
              current_close <- current_data[j,"Close"]
              
              if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close >= high_price) && (current_high >= day_high)){
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
              }
              # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
              else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= prev_low_price) && (current_low <= day_low)){
                # print(current_date)
                satisfied_df = rbind(satisfied_df,current_data[j,])
                rownames(satisfied_df) <- 1:nrow(satisfied_df)
                satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
              }else{
                next
              }
              
              
            }
            
            
            ## Commented this one needs to be checked
            
            # for(i in 5:nrow(current_data)){
            #   # if(stock == "UPL.NS"){
            #   
            #   current_date <- current_data[i,"Datetime"]
            #   # print(current_date)
            #   
            #   day_high <- max(current_data$Close)
            #   day_low <- min(current_data$Low)
            #   
            #   low_range <- min(current_data[i-1,4],current_data[i-2,4],current_data[i-3,4],current_data[i-4,4])
            #   high_range <- max(current_data[i-1,3],current_data[i-2,3],current_data[i-3,3],current_data[i-4,3])
            #   current_close <- current_data[i,"Close"]
            #   if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close >= high_price) && (current_close >= day_high)){
            #     satisfied_df = rbind(satisfied_df,current_data[j,])
            #     rownames(satisfied_df) <- 1:nrow(satisfied_df)
            #     satisfied_df[nrow(satisfied_df),"Call"] <- "Buy"
            #   }
            #   # else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price) && (current_close <= day_low)){
            #   else if((abs(high_range - low_range)/low_range*100 < 0.4) && (current_close <= close_price)){
            #     print(current_date)
            #     satisfied_df = rbind(satisfied_df,current_data[j,])
            #     rownames(satisfied_df) <- 1:nrow(satisfied_df)
            #     satisfied_df[nrow(satisfied_df),"Call"] <- "Sell"
            #   }else{
            #     next
            #   }
            #   
            # }
          }
          
          
          if(nrow(satisfied_df) == 0){
            next
          }
          else{
            satisfied_df = head(satisfied_df,1)
            
            if(stock == "%5ENSEBANK"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else if(stock == "%5ENSEI"){
              curr_hr <- hour(satisfied_df[1,"Datetime"])
              curr_min <- minute(satisfied_df[1,"Datetime"])
            }else{
              curr_hr <- hour(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(satisfied_df[1,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
            }
            
            
            
            if(curr_hr == 15 && curr_min >= 15){
              next
            }else{
              
              # print(satisfied_df)
              Signal_df[increment,"Strategy"] <- "Gap_up"
              Signal_df[increment,"Stock"]=stock
              Signal_df[increment,"Signal"]=satisfied_df[1,"Call"]
              Signal_df[increment,"Datetime"]=satisfied_df[1,"Datetime"]
              Signal_df[increment,"Value"]=satisfied_df[1,"Close"]
              
              increment = increment + 1
              
            }
          }
          
        }
      }
      
      
    }else{
      print("pass")
    }
    
    return(Signal_df)
  }
  
  
  target_and_sl <- function(Signal_df){
    
    if(nrow(Signal_df) == 0){
      print("break")
    }else{
      # Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
      
      # Signal_df$Datetime <- Signal_df$Datetime + hm("5:30")
      
      # Signal_df <- Signal_df[order(Signal_df$Datetime),]
      
      rownames(Signal_df) <- 1:nrow(Signal_df)
      
      Signal_df$Value <- round(as.numeric(Signal_df$Value),2)

      # browser()
      
      if(Signal_df$Stock[1] == "%5ENSEBANK"){
        Signal_df$Date <- as.Date(Signal_df$Datetime)
      }else if(Signal_df$Stock[1] == "%5ENSEI"){
        Signal_df$Date <- as.Date(Signal_df$Datetime)
      }else{
        Signal_df$Date <- anydate(as.numeric(Signal_df$Datetime))
      }


      Capital <- input$initial_cap
      
      if(input$target_selection == 'Percentage'){
        
        stop_loss <- as.numeric(input$percentage_ip_risk)
        target <- as.numeric(input$percentage_ip_reward)
        
        Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
        Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
      }else if (input$target_selection == 'Points'){
        
        stop_loss <- as.numeric(input$points_ip_risk)
        target <- as.numeric(input$points_ip_reward)
        
        Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-stop_loss,stop_loss+Signal_df$Value)
        Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+target,Signal_df$Value-target)
      }else{
        Signal_df$StopLoss <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
        Signal_df$Target <- ifelse(Signal_df$Signal == "Buy",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
      }
      

      
    }
    return(Signal_df)
  }
  
  check_final_calls <- function(Signal_df){
    
    # browser()
    stock <- input$backtest_stock
    
    money_control_stock <- toupper(gsub("\\..*","",stock))
    starttime <- as.integer(as.POSIXct(input$backtest_range[1]))
    endtime <- as.integer(as.POSIXct(input$backtest_range[2]))
    
    final_signal_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
    
    if(stock == "%5ENSEBANK"){
      response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
      final_data <- response_data %>% select(date, open, high, low, close,volume)
      final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
      colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
      stock_5_min_historic_data <- final_data
    }else if(stock == "%5ENSEI"){
      if(input$backtest_timeframe == "15m"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
      }else{
        response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
      }
      final_data <- response_data %>% select(date, open, high, low, close,volume)
      final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
      colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
      stock_5_min_historic_data <- final_data
      
    }else{
      response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
      print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
      stock_timestamp <- response_data$t
      Close <- response_data$c
      High <- response_data$h
      Low <- response_data$l
      Open <- response_data$o
      Volume <- response_data$v
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
      }else{
        final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
        final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
      }
      stock_5_min_historic_data <- na.omit(final_data)
    }
    
    for(i in 1:nrow(Signal_df)){
      
      # browser()
      
      print(i)
      stock <- Signal_df[i,"Stock"]
      call_time <- Signal_df[i,"Datetime"]
      signal_val <- Signal_df[i,"Signal"]
      call_val <- Signal_df[i,"Value"]
      StopLoss <- Signal_df[i,"StopLoss"]
      Target <- Signal_df[i,"Target"]
      Strategy <- Signal_df[i,"Strategy"]
      
      current_date <- Signal_df[i,"Date"]
      current_signal <- Signal_df[i,"Signal"]
      stock_5_min_historic_data <- data.frame(stock_5_min_historic_data)
      
      stock_5_min_historic_data <- stock_5_min_historic_data %>% mutate(date = as.Date(stock_5_min_historic_data$Datetime))
      final_5_min_stocks <- sqldf("select *,
                            dense_rank() over(order by date) as dns_rank
                    
                            from stock_5_min_historic_data sm
                            ")
      current_data = data.frame()
      if(input$eod_square_off == "yes"){
        current_data <- final_5_min_stocks[final_5_min_stocks$date == current_date,]
        if(stock == "%5ENSEBANK"){
          sub_data <- current_data[current_data$Datetime > call_time,]
        }else if(stock == "%5ENSEI"){
          sub_data <- current_data[current_data$Datetime > call_time,]
        }else{
          sub_data <- current_data[current_data$Datetime > anytime(as.numeric(call_time)),]
        }
        satisfied_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
        incr = 1
        if(nrow(sub_data) > 0 ) {
          rownames(sub_data) <- 1:nrow(sub_data)
          if(signal_val == "Buy"){
            for(j in 1:nrow(sub_data)){
              curr_hr <- hour(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              if((sub_data[j,"High"]) >= Target){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"Target"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
                break
              }
              else if((sub_data[j,"Low"]) <= StopLoss){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"SL"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                # print(abs(destring(sub_data[j,"Low"]) - destring(signal_val)))
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
                break
              }
              else if(curr_hr == 15 && curr_min == 15){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
                break
              }
              else{
                
              }
            }
            
          }else{
            for(j in 1:nrow(sub_data)){
              curr_hr <- hour(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              if((sub_data[j,"Low"]) <= Target){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"Target"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
                break
              }
              else if((sub_data[j,"High"]) >= StopLoss){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"SL"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(destring(call_val))),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
                break
              }
              else if(curr_hr == 15 && curr_min == 15){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
                break
              }
              else{
                
              }
            }
          }
          if(nrow(satisfied_df) == 0){
            next
          }else{
            satisfied_df = head(satisfied_df,1)
            final_signal_df = rbind(final_signal_df,satisfied_df)
          }
        }
      }else{
        current_data <- final_5_min_stocks[final_5_min_stocks$date >= current_date,]
        if(stock == "%5ENSEBANK"){
          sub_data <- current_data[current_data$Datetime > call_time,]
        }else if(stock == "%5ENSEI"){
          sub_data <- current_data[current_data$Datetime > call_time,]
        }else{
          sub_data <- current_data[current_data$Datetime > anytime(as.numeric(call_time)),]
        }
        satisfied_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
        incr = 1
        if(nrow(sub_data) > 0 ) {
          rownames(sub_data) <- 1:nrow(sub_data)
          if(signal_val == "Buy"){
            for(j in 1:nrow(sub_data)){
              curr_hr <- hour(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              if((sub_data[j,"High"]) >= Target){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"Target"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
                break
              }
              else if((sub_data[j,"Low"]) <= StopLoss){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"SL"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                # print(abs(destring(sub_data[j,"Low"]) - destring(signal_val)))
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
                break
              }
              else{
                
              }
            }
            
          }else{
            for(j in 1:nrow(sub_data)){
              curr_hr <- hour(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              curr_min <- minute(as_datetime(as.character(as_datetime(sub_data[j,"Datetime"]) + hm("5:30")),tz="Asia/Kolkata"))
              if((sub_data[j,"Low"]) <= Target){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"Target"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"Low"])) - as.numeric(call_val)),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"Low"],2)
                incr = incr + 1
                break
              }
              else if((sub_data[j,"High"]) >= StopLoss){
                satisfied_df[incr,"Strategy"] <- Strategy
                satisfied_df[incr,"stock"] <- stock
                satisfied_df[incr,"Call_time"] <- call_time
                satisfied_df[incr,"Call"] <- current_signal
                satisfied_df[incr,"SL"] <- "Yes"
                satisfied_df[incr,"achieved_ts"] <- sub_data[j,"Datetime"]
                satisfied_df[incr,"points"] <- round(abs(as.numeric(destring(sub_data[j,"High"])) - as.numeric(destring(call_val))),2)
                satisfied_df[incr,"Value"]<- round(sub_data[j,"High"],2)
                incr = incr + 1
                break
              }
              else{
                
              }
            }
          }
          if(nrow(satisfied_df) == 0){
            next
          }else{
            satisfied_df = head(satisfied_df,1)
            final_signal_df = rbind(final_signal_df,satisfied_df)
          }
          
        }
      }
    }
    
    return(final_signal_df)
    
  }
  
  
  observeEvent(input$backtest_action,{
    
    
    stock <- input$backtest_stock
    
    Signal_df <- get_signals_data()
    
    #########    Setting the target for the selected stock   ###########
    
    # browser()
    print("Signal df")
    print(Signal_df)
    
    Signal_df <- target_and_sl(Signal_df)
    
    final_signal_df = data.frame(Strategy = character(0),Call_time = character(0),Call = character(0),stock = character(0),Target = character(0),SL = character(0), achieved_ts = character(0),points = character(0),Value=character(0))
    
    
    # browser()
    
    print(final_signal_df)
    
    
    # browser()
    
    if(nrow(Signal_df) > 0){
      
      final_signal_df <- check_final_calls(Signal_df)
     
  }
    
    
    print(final_signal_df)
    
    # browser()
    
    
    if(nrow(final_signal_df) >0 ){
      if(length(final_signal_df[is.na(final_signal_df$Target),]$Target) > 0){
        final_signal_df[is.na(final_signal_df$Target),]$Target <- ""
      }
      if(length(final_signal_df[is.na(final_signal_df$SL),]$SL) > 0 ){
        final_signal_df[is.na(final_signal_df$SL),]$SL <- ""
      }
      
      final_signal_df$sign <- ifelse((final_signal_df$Call=="Buy"),-1,1)
      
      # View(final_signal_df)
      Signal_df$sign <- ifelse(Signal_df$Signal=="Buy",1,-1)
      Signal_df
      
      
      # Signal_df$Datetime <- as_datetime(as.character(as_datetime(Signal_df$Datetime) + hm("5:30")),tz="Asia/Kolkata")
      # final_signal_df$achieved_ts <- as_datetime(as.character(as_datetime(final_signal_df$achieved_ts) + hm("5:30")),tz="Asia/Kolkata")
      # # final_signal_df$StartTime <- as_datetime(as.character(as_datetime(final_signal_df$StartTime) + hm("5:30")),tz="Asia/Kolkata")
      # final_signal_df$Call_time <- as_datetime(as.character(as_datetime(final_signal_df$Call_time) + hm("5:30")),tz="Asia/Kolkata")
      
      if(!stock %in% c("%5ENSEI","%5ENSEBANK")){
        Signal_df$Datetime <- anytime(as.numeric(Signal_df$Datetime))
        final_signal_df$achieved_ts  <- anytime(as.numeric(final_signal_df$achieved_ts))
        final_signal_df$Call_time <- anytime(as.numeric(final_signal_df$Call_time))
      }
      
      
      colnames(Signal_df)
      colnames(final_signal_df)
      
      
      combined_data <- sqldf("with main_table as
                      (
                        select Strategy,stock,Datetime,Value as price,Target, StopLoss,sign
                        from Signal_df
                        union all
                        select Strategy,stock,achieved_ts as Datetime,Value as price,0 as Target, 0 as StopLoss,sign
                        from final_signal_df
                      )
                      select * from main_table order by Datetime asc
                      ")
      
      target_combined_data <- sqldf("select Strategy,sd1.stock,sd1.Datetime as StartTime,sd1.Value as price,sd1.Target, sd1.StopLoss,sd1.sign as initial_sign,sd2.achieved_ts,sd2.hit_price,sd2.sign as final_sign
      from Signal_df sd1
      left join 
       (
      select Call_time,stock,achieved_ts,Value as hit_price,sign
     from final_signal_df
      ) sd2 on sd1.Datetime = sd2.Call_time and sd1.stock = sd2.stock
       ")
      
      target_combined_data$price_diff <- round(ifelse(target_combined_data$initial_sign == 1,as.numeric(target_combined_data$hit_price) - as.numeric(target_combined_data$price),as.numeric(target_combined_data$price) - as.numeric(target_combined_data$hit_price)),2)
      # target_combined_data$price_diff <- round(as.numeric(target_combined_data$hit_price) - as.numeric(target_combined_data$price),2)
      for(i in 1:nrow(target_combined_data)){
        if(i==1){
          capital <- input$initial_cap
          target_combined_data[i,"QTY"] <- round(abs((20/100)*capital/(target_combined_data[i,"Target"] - target_combined_data[i,"StopLoss"])),0)
          target_combined_data[i,"corrected_capital"] <- round(capital + ((target_combined_data[i,"QTY"])*(target_combined_data[i,"price_diff"])),2)
        }
        else{
          capital <- target_combined_data[(i-1),"corrected_capital"]
          target_combined_data[i,"QTY"] <- round(abs((20/100)*capital/(target_combined_data[i,"Target"] - target_combined_data[i,"StopLoss"])),0)
          target_combined_data[i,"corrected_capital"] <- round(capital + ((target_combined_data[i,"QTY"])*(target_combined_data[i,"price_diff"])),2)
        }
      }
    }
    
    
    
    
    
    # browser()
    
    
    # target_combined_data$QTY <- round(abs((20/100)*Capital/(target_combined_data$Target - target_combined_data$StopLoss)),0)
    
    # money_control_data <- read.csv(paste0(getwd(),"/data/Money_Control_Tickers.csv", sep = ""))
    # 
    # row_number <- which(grepl(stock, money_control_data$Company))
    # 
    # money_control_stock <- money_control_data[row_number,5]
    money_control_stock <- toupper(gsub("\\..*","",stock))
    
    # starttime <- as.integer(as.POSIXct("2020-05-01"))
    # endtime <- as.integer(as.POSIXct("2021-05-22"))
    
    starttime <- as.integer(as.POSIXct(input$backtest_range[1]))
    endtime <- as.integer(as.POSIXct(input$backtest_range[2]))
    
    
    if(stock == "%5ENSEBANK"){
      response_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))
      final_data <- response_data %>% select(date, open, high, low, close,volume)
      final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
      colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
      stock_5_min_data <- final_data
      
    }else if(stock == "%5ENSEI"){
      if(input$backtest_timeframe == "15m"){
        response_data <- read.csv(paste0(getwd(),"/data/nifty/15min_data_Nifty.csv"))
      }else{
        response_data <- read.csv(paste0(getwd(),"/data/nifty/5minute.csv"))
      }
      final_data <- response_data %>% select(date, open, high, low, close,volume)
      final_data <- final_data[final_data$date >= input$backtest_range[1] & final_data$date < input$backtest_range[2],]
      colnames(final_data) <- c("Datetime","Open","High","Low","Close","Volume")
      stock_5_min_data <- final_data
      
    }else{
      
      response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
      
      print(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
      stock_timestamp <- response_data$t
      Close <- response_data$c
      High <- response_data$h
      Low <- response_data$l
      Open <- response_data$o
      Volume <- response_data$v
      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      
      colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
      }else{
        final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
      }
      
      stock_5_min_data <- na.omit(final_data)
      
    }
    
    # response_data <- fromJSON(paste0("https://priceapi.moneycontrol.com/techCharts/techChartController/history?symbol=",money_control_stock,"&resolution=5&from=",starttime,"&to=",endtime,sep = ""))
    # 
    # stock_timestamp <- response_data$t
    # Close <- response_data$c
    # High <- response_data$h
    # Low <- response_data$l
    # Open <- response_data$o
    # Volume <- response_data$v
    # final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
    # 
    # 
    # colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
    # 
    # if(typeof(final_data$V1) == "list"){
    #   final_data <- final_data[-c(which(final_data$Close == "NULL")),]
    #   new_stock_timestamp <- unlist(final_data$V1)
    #   Close <- unlist(final_data$Close)
    #   High <- unlist(final_data$High)
    #   Open <- unlist(final_data$Open)
    #   Low <- unlist(final_data$Low)
    #   Volume <- unlist(final_data$Volume)
    #   
    #   final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
    #   
    #   final_data$Datetime <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
    #   
    #   final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
    # }else{
    #   final_data$Datetime <- as.POSIXct(final_data$V1, origin="1970-01-01")
    #   
    #   final_data <- final_data %>% select(Datetime, Open, High, Low, Close,Volume)
    # }
    # 
    # stock_5_min_data <- na.omit(final_data)
    
    stock_5_min_data <- data.frame(stock_5_min_data)
    
    stock_5_min_data <- stock_5_min_data %>%
      arrange(Datetime) %>%
      mutate(
        pnl = Close/lag(Close) - 1
      )
    
    
    stock_5_min_data[is.na(stock_5_min_data$pnl),]$pnl <- 0
    
    
    stock_5_min_data$Datetime <- as_datetime(as.character(as_datetime(stock_5_min_data$Datetime) + hm("5:30")),tz="Asia/Kolkata")
    Signal_df$Datetime <- as_datetime(as.character(as_datetime(Signal_df$Datetime) + hm("5:30")),tz="Asia/Kolkata")
    
    combined_data
    
    final_combined <- sqldf("select sm.Datetime,
                                sm.pnl,
                              -- cd.Strategy,
                              --  cd.stock,
                              case when cd.price > 0 then cd.price else sm.Close end as price,
                                cd.sign
                         from stock_5_min_data sm
                        left join combined_data cd on sm.Datetime = cd.Datetime")
    
    
    final_combined
    
    # final_combined[which(is.na(final_combined$price)),]$price <- 0
    final_combined[which(is.na(final_combined$sign)),]$sign <- 0
    
    if(!stock %in% c("%5ENSEI","%5ENSEBANK")){
    
    target_combined_data$StartTime <- as_datetime(as.character(as_datetime(target_combined_data$StartTime) + hm("5:30")),tz="Asia/Kolkata")
    
    }
    
    final_data_combined = data.frame()
    
    final_data_combined <- rbind(final_data_combined,target_combined_data)
    
    # 
    # 
    # write.csv(final_combined,paste0(getwd(),"/data/bot_backtest.csv", sep = ""))
    
    # browser()
    if(!stock %in% c("%5ENSEI","%5ENSEBANK")){
      target_combined_data$StartTime <- target_combined_data$StartTime + hm("5:30")
      target_combined_data$achieved_ts <- target_combined_data$achieved_ts + hm("5:30")
      target_combined_data$time_diff <- difftime(as.POSIXct(target_combined_data$achieved_ts, origin="1970-01-01"),as.POSIXct(target_combined_data$StartTime, origin="1970-01-01"),"mins")
    }else if(stock == "%5ENSEI"){
      target_combined_data$time_diff <- difftime(anytime(target_combined_data$achieved_ts),anytime(target_combined_data$StartTime),"mins")
    }else{
      target_combined_data$time_diff <- difftime(as.POSIXct(target_combined_data$achieved_ts, origin="1970-01-01"),as.POSIXct(target_combined_data$StartTime, origin="1970-01-01"),"mins")*60
    }
   
    
    
    write.csv(target_combined_data,paste0(getwd(),"/data/bot_targets_backtest.csv", sep = ""))

  
  output$backtest_results <-  DT::renderDataTable({
    
    # stock <- "RELIANCE.NS"
    
    target_combined_data <- subset(target_combined_data, select = -c(Strategy,Stock) )
    
    target_combined_data$initial_sign <- ifelse(target_combined_data$initial_sign == "-1","Sell","Buy")
    
    target_combined_data <- subset(target_combined_data,select = c(StartTime,price,Target,StopLoss,initial_sign,QTY,achieved_ts,hit_price,price_diff,corrected_capital,time_diff))
    
    colnames(target_combined_data) <- c("Transaction Time","Open Price","Target","Stop Loss","Signal","Optimal Quantity","Closed Time","Closed Price","Price Change","Corrected Capital","Time Diff")
    
    DT::datatable(target_combined_data,extensions = c('FixedColumns'),selection = "single",filter = 'top',
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top',
                                 # columnDefs = list(list(width = '200px', targets = c(2))),
                                 columnDefs = list(list(width = '250px', targets = c(1,7)),list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8))),
                                 rowCallback=JS(
                                   'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1)
            $(row).css("background","#D7DBDD")
   }')
                  ))%>% formatStyle(
                    'Signal',
                    backgroundColor = styleEqual(c("Buy", "Sell"), c('green', 'red'))
                  )
    
    
  })
  
  # output$weekly_and_hourly_chart <- renderPlot({
  output$weekly_and_hourly_chart <- renderPlotly({
    
    
    
    if(!stock %in% c("%5ENSEI","%5ENSEBANK")){
      
      target_combined_data$month <- month(target_combined_data$StartTime  - hm("5:30"), label = TRUE)
      target_combined_data$year <- year(target_combined_data$StartTime  - hm("5:30"))
      target_combined_data$wday <- wday(target_combined_data$StartTime  - hm("5:30"), label = TRUE)
      target_combined_data$hour <- hour(target_combined_data$StartTime  - hm("5:30"))
      
      
    }else if(stock == "%5ENSEI"){
      target_combined_data$month <- month(target_combined_data$StartTime, label = TRUE)
      target_combined_data$year <- year(target_combined_data$StartTime)
      target_combined_data$wday <- wday(target_combined_data$StartTime, label = TRUE)
      target_combined_data$hour <- hour(anytime(target_combined_data$StartTime))
      
    }else{
      target_combined_data$month <- month(target_combined_data$StartTime, label = TRUE)
      target_combined_data$year <- year(target_combined_data$StartTime)
      target_combined_data$wday <- wday(target_combined_data$StartTime, label = TRUE)
      target_combined_data$hour <- hour(target_combined_data$StartTime)
    }
    
    
    
    dayHour <- target_combined_data  %>% dplyr::group_by(hour,wday) %>% dplyr::summarise(profits = mean(price_diff, na.rm = TRUE), N = length(wday))
    
    dayHour$sign <- ifelse(dayHour$profits > 0,1,0)
    
    #Wrap ggplot of time-series heatmap in ggplotly, call "tooltip"  
    ggplot_ts_heatmap <- dayHour %>%
      ggplot(aes(as.factor(hour), wday, 
                 fill=as.factor(sign), label = N, label2 = hour, label3 = profits,
                 text = paste("Weekday:", wday))) + 
      geom_tile(col=1) +
      theme_bw(base_line_size = 0, base_rect_size = 0, base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank()) +
      scale_fill_manual(labels = levels(dayHour$profits),
                        values=c("#FF6347","#00FF00")) +
      labs(x = "", y = "")
    
    ggplotly(ggplot_ts_heatmap, tooltip = c("text","label","label2","label3"))
    
    
  })
  
  output$monthly_and_hourly_chart <- renderPlotly({
    
    # browser()
    
    if(!stock %in% c("%5ENSEI","%5ENSEBANK")){
      
      target_combined_data$month <- month(target_combined_data$StartTime  - hm("5:30"), label = TRUE)
      target_combined_data$year <- year(target_combined_data$StartTime  - hm("5:30"))
      target_combined_data$wday <- wday(target_combined_data$StartTime  - hm("5:30"), label = TRUE)
      target_combined_data$hour <- hour(target_combined_data$StartTime  - hm("5:30"))
      
      
    }else if(stock == "%5ENSEI"){
      target_combined_data$month <- month(anytime(target_combined_data$StartTime), label = TRUE)
      target_combined_data$year <- year(anytime(target_combined_data$StartTime))
      target_combined_data$wday <- wday(anytime(target_combined_data$StartTime), label = TRUE)
      target_combined_data$hour <- hour(anytime(target_combined_data$StartTime))
      
    }else{
      target_combined_data$month <- month(target_combined_data$StartTime, label = TRUE)
      target_combined_data$year <- year(target_combined_data$StartTime)
      target_combined_data$wday <- wday(target_combined_data$StartTime, label = TRUE)
      target_combined_data$hour <- hour(target_combined_data$StartTime)
    }
    
    
    dayHour <- target_combined_data  %>% dplyr::group_by(hour,month) %>% dplyr::summarise(profits = mean(price_diff, na.rm = TRUE), N = length(month))
    
    dayHour$sign <- ifelse(dayHour$profits > 0,1,0)
    #Wrap ggplot of time-series heatmap in ggplotly, call "tooltip"  
    ggplot_ts_heatmap <- dayHour %>%
      ggplot(aes(as.factor(hour), month, 
                 fill=as.factor(sign), label = N, label2 = hour, label3 = profits,
                 text = paste("Month :", month))) + 
      geom_tile(col=1) +
      theme_bw(base_line_size = 0, base_rect_size = 0, base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank()) +
      scale_fill_manual(labels = levels(dayHour$profits),
                        values=c("#FF6347","#00FF00")) +
      labs(x = "", y = "")
    
    ggplotly(ggplot_ts_heatmap, tooltip = c("text","label","label2","label3"))
    
    
    # dayHour <- ddply(target_combined_data, c( "hour", "month"), summarise,
    #                  N  = length(StartTime)
    # )
    # 
    # #reverse order of months for easier graphing
    # dayHour$month <- factor(dayHour$month, levels=rev(levels(dayHour$month)))
    # attach(dayHour)
    # 
    # 
    # ggplot(dayHour, aes(hour, month)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
    #   scale_fill_gradient(low = col1, high = col2) +  
    #   guides(fill=guide_legend(title="Total Calls")) +
    #   theme_bw() + theme_minimal() + 
    #   labs(title = "Histogram of Stock Calls by Month and Hour",
    #        x = "Calls Per Hour", y = "Month") +
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  })
  
  output$total_pnl <- renderInfoBox({
    
    final_pnl <- tail(target_combined_data,1)
    
    infoBox(
      "Total PNL", final_pnl$corrected_capital,icon = icon("fas fa-history"),
      color = "yellow",width = 4
    )
  })
  output$total_signals <- renderInfoBox({
    
    infoBox(
      "Total Transactions", nrow(target_combined_data),icon = icon("fas fa-book"),
      color = "blue",width = 4
    )
  })
  output$total_wins <- renderInfoBox({
    won_df <- data.frame()
    buy_signals <- target_combined_data[target_combined_data$initial_sign == "1",]
    sell_signals <- target_combined_data[target_combined_data$initial_sign == "-1",]
    
    won_buys <- buy_signals[buy_signals$hit_price >= buy_signals$Target,]
    won_sells <- sell_signals[sell_signals$hit_price <= sell_signals$Target,]
    won_df <- rbind(won_df,won_buys)
    won_df <- rbind(won_df,won_sells)
    infoBox(
      "Total Target Hit", nrow(won_df),icon = icon("fas fa-thumbs-up"),
      color = "green",width = 4
    )
  })
  output$total_losses <- renderInfoBox({
    loss_df <- data.frame()
    buy_signals <- target_combined_data[target_combined_data$initial_sign == "1",]
    sell_signals <- target_combined_data[target_combined_data$initial_sign == "-1",]
    
    loss_buys <- buy_signals[buy_signals$hit_price <= buy_signals$StopLoss,]
    loss_sells <- sell_signals[sell_signals$hit_price >= sell_signals$StopLoss,]
    loss_df <- rbind(loss_df,loss_buys)
    loss_df <- rbind(loss_df,loss_sells)
    infoBox(
      "Total StopLoss Hit", nrow(loss_df),icon = icon("fas fa-thumbs-down"),
      color = "red",width = 4
    )
  })
  
  output$total_closed_eod <- renderInfoBox({
    won_df <- data.frame()
    buy_signals <- target_combined_data[target_combined_data$initial_sign == "1",]
    sell_signals <- target_combined_data[target_combined_data$initial_sign == "-1",]
    
    won_buys <- buy_signals[(buy_signals$hit_price < buy_signals$Target) & (buy_signals$hit_price > buy_signals$StopLoss),]
    won_sells <- sell_signals[(sell_signals$hit_price > sell_signals$Target) & (sell_signals$hit_price < sell_signals$StopLoss),]
    won_df <- rbind(won_df,won_buys)
    won_df <- rbind(won_df,won_sells)
    
    
    infoBox(
      "Total Auto squareoff ", nrow(won_df),icon = icon("fas fa-user-astronaut"),
      color = "black",width = 4
    )
  })
  
  output$time_hit_target <- renderInfoBox({
    
    # browser()
    average_time_to_hit_target <- mean(as.numeric(target_combined_data[target_combined_data$price_diff >= 0, ]$time_diff))


    infoBox(
      "Avg Time to Hit Target (Mins)", round(average_time_to_hit_target,2),icon = icon("fas fa-user-astronaut"),
      color = "green",width = 4
    )
  })
  
  output$time_hit_stoploss <- renderInfoBox({
    
    average_time_to_hit_stoploss <- mean(as.numeric(target_combined_data[target_combined_data$price_diff < 0, ]$time_diff))
    
    
    infoBox(
      "Avg Time to Hit StopLoss (Mins)", round(average_time_to_hit_stoploss,2),icon = icon("fas fa-user-astronaut"),
      color = "red",width = 4
    )
  })
  
  output$returns_plot<-renderPlotly({
  # browser()
  p <-  target_combined_data %>%
    ggplot(aes(x = as.Date(StartTime), y = price_diff * QTY))+
    geom_bar(stat = "identity", fill = palette_light()[[1]]) +
    labs(title = "Daily Returns",
         subtitle = "Profit vs Loss",
         caption = "Shows an above-zero trend meaning positive returns",
         x = "", y = "Gained Money") +
    geom_smooth(method = "lm") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar)
  
  ggplotly(p)
  
  })
  
  
  }) 
  
  getYahooSymbols <- function(){
    NSE_List <- read.csv(paste0(getwd(),"/data/NSE_Stocks_List.csv", sep = ""))
    NSE_List <- as.data.frame(NSE_List)
    
    NSE_Tickers <- NSE_List$Yahoo.Symbol
    
    return(NSE_Tickers)
  }
  
  
  observe({
    # browser()
    NSE_Tickers <- getYahooSymbols()
    
    NSE_Tickers <- sample(NSE_Tickers)
    
    updateSelectizeInput(
      session,
      "backtest_stock",
      label = "Company :",
      choices = NSE_Tickers,
      selected = "RELIANCE.NS",
      options = list(),
      server = FALSE
    )
    
  })
}
