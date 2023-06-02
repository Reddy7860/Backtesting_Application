library("quantmod")

daily_nifty_data <- read.csv(paste0(getwd(),"/data/nifty_bank/day.csv"))
five_minute_nifty_data <- read.csv(paste0(getwd(),"/data/nifty_bank/5minute.csv"))

daily_nifty_data$pivot_point = (lag(daily_nifty_data$high,1) + lag(daily_nifty_data$low,1) + lag(daily_nifty_data$close,1))/3

daily_nifty_data$support_1 = round((2*daily_nifty_data$pivot_point) - lag(daily_nifty_data$high,1),2)

daily_nifty_data$resistance_1 = round((2*daily_nifty_data$pivot_point) - lag(daily_nifty_data$low,1),2)

daily_nifty_data$support_2 = round(daily_nifty_data$pivot_point - (daily_nifty_data$resistance_1 - daily_nifty_data$support_1),2)

daily_nifty_data$resistance_2 = round((daily_nifty_data$pivot_point - daily_nifty_data$support_1 ) + daily_nifty_data$resistance_1,2)

daily_nifty_data$resistance_3 = round((daily_nifty_data$pivot_point - daily_nifty_data$support_2 ) + daily_nifty_data$resistance_2,2)

daily_nifty_data$support_3 = round(daily_nifty_data$pivot_point - (daily_nifty_data$resistance_2 - daily_nifty_data$support_2),2)


daily_nifty_data$price_difference = (lag(daily_nifty_data$high,1) - lag(daily_nifty_data$low,1))

daily_nifty_data$R1 = round((23.6*daily_nifty_data$price_difference/100) + daily_nifty_data$pivot_point,2)

daily_nifty_data$R2 = round((38.2*daily_nifty_data$price_difference/100) + daily_nifty_data$pivot_point,2)

daily_nifty_data$R3 = round((50*daily_nifty_data$price_difference/100) + daily_nifty_data$pivot_point,2)

daily_nifty_data$R4 = round((61.8*daily_nifty_data$price_difference/100) + daily_nifty_data$pivot_point,2)

daily_nifty_data$R5 = round((100*daily_nifty_data$price_difference/100) + daily_nifty_data$pivot_point,2)

daily_nifty_data$S1 = round(daily_nifty_data$pivot_point - (23.6*daily_nifty_data$price_difference/100),2)

daily_nifty_data$S2 = round(daily_nifty_data$pivot_point - (38.2*daily_nifty_data$price_difference/100),2)

daily_nifty_data$S3 = round(daily_nifty_data$pivot_point - (50*daily_nifty_data$price_difference/100),2)

daily_nifty_data$S4 = round(daily_nifty_data$pivot_point - (61.8*daily_nifty_data$price_difference/100),2)

daily_nifty_data$S5 = round(daily_nifty_data$pivot_point - (100*daily_nifty_data$price_difference/100),2)


tail(daily_nifty_data)


xbt <- daily_nifty_data %>% 
  mutate(
    ema = TTR::EMA(close, 10),
    rsi = TTR::RSI(close, 10)
  )

xbt


write.csv(xbt,paste0(getwd(),"/data/nifty_bank/modified_day.csv"))

five_minute_nifty_data$current_date <- as.Date(five_minute_nifty_data$date)

five_minute_nifty_data

xbt$current_date <- as.Date(xbt$date)

final_five_minute_nifty_data <- left_join(five_minute_nifty_data,xbt,by = "current_date")

colnames(final_five_minute_nifty_data) <- c("datetime","open","high","low","close","volume","name","current_date","previous_date","previous_open","previous_high","previous_low","previous_close","previous_volume","previous_name","pivot_point","support_1","resistance_1","support_2","resistance_2","resistance_3","support_3","price_difference","R1","R2","R3","R4","R5","S1","S2","S3","S4","S5","ema","rsi")

target_raw = read.csv(paste0(getwd(),"/data/bot_targets_backtest.csv"))

library("sqldf")

final_target_raw = sqldf("select Strategy,StartTime,price,Target,StopLoss,initial_sign,achieved_ts,
hit_price,
final_sign,
price_diff,
QTY,
corrected_capital,
pivot_point,
support_1,
resistance_1,support_2,resistance_2, resistance_3,support_3,  R1,R2, R3,R4,R5,S1,S2,S3,S4,S5,ema,rsi

from target_raw tr
left join final_five_minute_nifty_data fm on tr.StartTime = fm.datetime
                   ")


write.csv(final_target_raw,paste0(getwd(),"/data/modified_bot_targets_backtest.csv"))
# final_target_raw[final_target_raw$price_diff < 0,]

library(lubridate)

# Achived within time 
final_target_raw$time_diff <- difftime(as.POSIXct(final_target_raw$achieved_ts, origin="1970-01-01"),as.POSIXct(final_target_raw$StartTime, origin="1970-01-01"),"mins")

final_target_raw$pivot_check <- ifelse(final_target_raw$initial_sign == 1, ifelse(final_target_raw$price >= final_target_raw$pivot_point,TRUE,FALSE),ifelse(final_target_raw$price <= final_target_raw$pivot_point,TRUE,FALSE))
final_target_raw$support_1_check <- ifelse(final_target_raw$initial_sign == 1, ifelse(final_target_raw$price >= final_target_raw$support_1,TRUE,FALSE),ifelse(final_target_raw$price <= final_target_raw$support_1,TRUE,FALSE))
final_target_raw$resistance_1_check <- ifelse(final_target_raw$initial_sign == 1, ifelse(final_target_raw$price >= final_target_raw$resistance_1,TRUE,FALSE),ifelse(final_target_raw$price <= final_target_raw$resistance_1,TRUE,FALSE))

# View(final_target_raw)


final_target_raw$month <- month(final_target_raw$StartTime, label = TRUE)
final_target_raw$year <- year(final_target_raw$StartTime)
final_target_raw$wday <- wday(final_target_raw$StartTime, label = TRUE)
final_target_raw$hour <- hour(final_target_raw$StartTime)

library("dplyr")

dayHour <- final_target_raw  %>% dplyr::group_by(hour,wday) %>% dplyr::summarise(profits = mean(price_diff, na.rm = TRUE), N = length(wday))

dayHour$sign <- ifelse(dayHour$profits > 0,1,0)

#Wrap ggplot of time-series heatmap in ggplotly, call "tooltip"  
ggplot_ts_heatmap <- dayHour %>%
  ggplot(aes(as.factor(hour), reorder(wday,N), 
             fill=as.factor(sign), label = N, label2 = hour, label3 = profits,
             text = paste("Weekday:", wday))) + 
  geom_tile(col=1) +
  theme_bw(base_line_size = 0, base_rect_size = 0, base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank()) +
  scale_fill_manual(values=c("#FF6347","#00FF00"),labels = levels(dayHour$profits)) +
  labs(x = "", y = "")

ggplotly(ggplot_ts_heatmap, tooltip = c("text","label","label2","label3"))





#Wrap ggplot of time-series heatmap in ggplotly, call "tooltip"  
ggplot_ts_heatmap <- confirmed %>%
  ggplot(aes(as.factor(date), reorder(`Country/Region`,`cases count`), 
             fill=cnt.cat, label = `cases count`, label2 = as.factor(date), 
             text = paste("country:", `Country/Region`))) + 
  geom_tile(col=1) +
  theme_bw(base_line_size = 0, base_rect_size = 0, base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank()) +
  scale_fill_manual(labels = levels(confirmed$cnt.cat),
                    values = pal_simpsons("springfield")(7)) +
  labs(x = "", y = "")

ggplotly(ggplot_ts_heatmap, tooltip = c("text","label","label2"))
























day_data %>% summarise(profits = sum(price_diff))


mtcars %>% 
  group_by(cyl, gear)

dayHour <- ddply(final_target_raw, c( "hour", "wday","price_diff"), summarise,
                 N  = length(StartTime)
)

#reverse order of months for easier graphing
dayHour$wday <- factor(dayHour$wday, levels=rev(levels(dayHour$wday)))
attach(dayHour)


ggplot(dayHour, aes(hour, wday)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Histogram of Stock Calls by Day of Week and Hour",
       x = "Calls Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())






final_target_raw$call_hour = hour(final_target_raw$StartTime)

x  <- as.matrix(mtcars)

x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")


heatmap(as.matrix(mtcars), Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))

final_target_heatmap <- final_target_raw %>% select(StartTime,call_hour) %>% mutate(count = count(call_hour))

df <- as.data.frame(sequence(c(7),from = 9))
colnames(df) <- "call_hour"

final_target_heatmap <- left_join(df,final_target_heatmap)

final_target_heatmap[is.na(final_target_heatmap)] <- 0

result <- final_target_heatmap[-1]
row.names(result) <- final_target_heatmap$call_hour
result

heatmap(as.matrix(result), Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))






library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)


#Data Set Located at: https://catalog.data.gov/dataset/seattle-police-department-911-incident-response-52779
incidents <-read.table("https://data.seattle.gov/api/views/3k2p-39jp/rows.csv?accessType=DOWNLOAD", head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F) 

#If the above data set is unavailable please use this code
df= fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggmap/i2Sample.csv', stringsAsFactors = FALSE)
incidents <- df


#Assign color variables
col1 = "#d8e1cf" 
col2 = "#438484"

#Peek at the data set and attach the column names
head(incidents)
attach(incidents)
str(incidents)


incidents$ymd <-mdy_hms(Event.Clearance.Date)
incidents$month <- month(incidents$ymd, label = TRUE)
incidents$year <- year(incidents$ymd)
incidents$wday <- wday(incidents$ymd, label = TRUE)
incidents$hour <- hour(incidents$ymd)
attach(incidents)
head(incidents)



dayHour <- ddply(incidents, c( "hour", "wday"), summarise,
                 N    = length(ymd)
)

#reverse order of months for easier graphing
dayHour$wday <- factor(dayHour$wday, levels=rev(levels(dayHour$wday)))
attach(dayHour)


ggplot(dayHour, aes(hour, wday,label = hour)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Histogram of Seattle Incidents by Day of Week and Hour",
       x = "Incidents Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



ggplot_ts_heatmap <- dayHour %>% 
                    ggplot(aes(hour, wday,label = profits)) + 
  geom_tile(col=1) +
                    scale_fill_gradient(low = "red", high = "green") +  
                    guides(fill=guide_legend(title="Total Incidents")) +
                    theme_bw() + theme_minimal() + 
                    labs(title = "Histogram of Seattle Incidents by Day of Week and Hour",
                         x = "Incidents Per Hour", y = "Day of Week")

ggplotly(ggplot_ts_heatmap)




library(tidyquant)

#install ggplot2
#install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)

#Load the function to the local through Paul Bleicher's GitHub page
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")



amznStock = as.data.frame(tidyquant::tq_get(c("AMZN"),get="stock.prices")) # get data using tidyquant
amznStock = amznStock[year(amznStock$date) > 2012, ] # Using data only after 2012


library(plyr)
library(plotly)

amznStock$weekday = as.POSIXlt(amznStock$date)$wday #finding the day no. of the week
amznStock$weekdayf<-factor(amznStock$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) # converting the day no. to factor

amznStock$monthf<-factor(month(amznStock$date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) # finding the month

amznStock$yearmonth<- factor(as.yearmon(amznStock$date)) # finding the year and the month from the date. Eg: Nov 2018

amznStock$week <- as.numeric(format(amznStock$date,"%W")) # finding the week of the year for each date

amznStock<-ddply(amznStock,.(yearmonth),transform,monthweek=1+week-min(week)) # normalizing the week to start at 1 for every month

p <- ggplot(amznStock, aes(monthweek, weekdayf, fill = amznStock$adjusted)) + 
  geom_tile(colour = "white") + facet_grid(year(amznStock$date)~monthf) + scale_fill_gradient(low="red", high="green") +  xlab("Week of Month") + ylab("") + ggtitle("Time-Series Calendar Heatmap: AMZN Stock Prices") + labs(fill = "Price")

p



library(shiny)
library(plotly)
library(tidyverse)
library(RCurl)
library(ggsci)
install.packages("ggsci")

#Read example data from Gist
confirmed <- read_csv("https://gist.githubusercontent.com/GeekOnAcid/5638e37c688c257b1c381a15e3fb531a/raw/80ba9704417c61298ca6919343505725b8b162a5/covid_selected_europe_subset.csv")


dayHour <- dayHour %>% select(c("wday","hour","N","N"))

dayHour$cases <- dayHour$N



#Wrap ggplot of time-series heatmap in ggplotly, call "tooltip"  
ggplot_ts_heatmap <- dayHour %>%
  ggplot(aes(as.factor(hour), reorder(wday,profits), 
             fill=as.factor(N), label = as.factor(profits), label2 = as.factor(hour), 
             text = paste("country:", wday))) + 
  geom_tile(col=1) +
  theme_bw(base_line_size = 0, base_rect_size = 0, base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank()) +
  scale_fill_manual(labels = levels(as.factor(dayHour$N)),
                    values = pal_simpsons("springfield")(16)) +
  labs(x = "", y = "")

#Wrap ggplot of time-series heatmap in ggplotly, call "tooltip"  
ggplot_ts_heatmap <- confirmed %>%
  ggplot(aes(as.factor(date), reorder(`Country/Region`,`cases count`), 
             fill=cnt.cat, label = `cases count`, label2 = as.factor(date), 
             text = paste("country:", `Country/Region`))) + 
  geom_tile(col=1) +
  theme_bw(base_line_size = 0, base_rect_size = 0, base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank()) +
  scale_fill_manual(labels = levels(confirmed$cnt.cat),
                    values = pal_simpsons("springfield")(7)) +
  labs(x = "", y = "")

ggplotly(ggplot_ts_heatmap, tooltip = c("text","label","label2"))









library("httr")
library(dplyr)
library(reshape2)
library("sqldf")

url = 'https://www.nseindia.com/api/option-chain-indices?symbol=NIFTY'

url = 'https://www.nseindia.com/api/option-chain-indices?symbol=BANKNIFTY'

response = GET(url, add_headers("user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.79 Safari/537.36"))

# as.data.frame(content(response)$filtered$data)
# 
# fromJSON(content(response)$filtered$data) %>% as.data.frame
# 
# 
# fromJSON(content(response)$filtered$data)
# 
# type_of(content(response)$records$data)

temp <- content(response)$records$data

data_raw <- tibble::enframe(unlist(temp))
# data_raw

data_raw <- data.frame(data_raw)


d <- within(data_raw, {
  name <- as.character(name)
  ID <- ave(name, name, FUN=seq_along)
})



final_strike_data <- dcast(d, ID ~ name, value.var="value")

View(final_strike_data)


final_strike_data %>% filter((expiryDate == "22-Jul-2021") & (strikePrice == 34500))

final_strike_data %>% filter((CE.expiryDate == "22-Jul-2021")&(CE.strikePrice) == 34500)

nrow(final_strike_data)

colnames(final_strike_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice")

final_options_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice
                          from final_strike_data t1 
                          left join final_strike_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice
                                "
                          )

final_options_data %>% filter((expiryDate == "22-Jul-2021")&(strikePrice) == 34500)


write.csv(data_raw,"nifty_raw_scrapping.csv")

View(data_raw)

type_of(temp[[1]])



df <- data.frame(matrix(unlist(temp), nrow=length(temp), byrow=TRUE))

unlist(temp)




temp_data <- read.csv("~/Desktop/Options_Chain/nifty_50_chain_data.csv")

tail(temp_data)

unique(temp_data$current_time)

current_Data <-temp_data %>% filter((expiryDate == "05-Aug-2021") &(strikePrice == 15800))


unique(temp_data$expiryDate)

colnames(temp_data)

library(dplyr)
library(ggplot2)
current_Data <- current_Data %>% select(c("Call_openInterest","current_time"))

current_Data$current_time <- as.POSIXct(current_Data$current_time,tz=Sys.timezone())


p <- ggplot(current_Data, aes(x=current_time, y=as.numeric(Call_openInterest))) +
  geom_line() 


current_Data %>% 
  ggplot() + 
  aes(x = current_time, y = as.numeric(Call_openInterest)) + 
  geom_point() + 
  theme(text = element_blank())

p

15856.05 - (15856.05)%%50

url <- "https://www.nseindia.com/api/chart-databyindex?index=OPTIDXNIFTY29-07-2021CE15800.00"
url <- "https://www.nseindia.com/api/chart-databyindex?index=OPTIDXNIFTY05-08-2021CE15800.00"

url <- "https://www.nseindia.com/api/chart-databyindex?index=OPTIDXNIFTY29-07-2021PE15800.00"
url <- "https://www.nseindia.com/api/chart-databyindex?index=OPTIDXNIFTY05-08-2021PE15800.00"

url = 'https://www.nseindia.com/api/option-chain-indices?symbol=NIFTY'


response = httr::GET(url, 
               add_headers("user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.107 Safari/537.36"),
               accept_json(),
               set_cookies(`ak_bmsc`='8E2F94F8235778E40687E99CF5AA8F1E~000000000000000000000000000000~YAAQhXxBF8EpQ896AQAATZ+W3gy4O2CXnO4zHSQCEfNHlQ/rUeZDivSdsuz+Tfe+pYYIN/pEuDoTHFwUn+IAUHT7iEMEDW1UBTYq+VpxxRfxLFkDq+er3E2N/bde2y2z86e40K1sd1Ytw41s4FTETqD1HcvnuyO3eB2qVCx4brug8NkwZZT6cN6IVlEehSm56JTIA37/BUFS340S6G2j65u0DzX2Aj6mqbLz5D7pgYr0VEjG3M1txTTQD00T52kvnGeLCbO5V5GfdPD+i1NlH/XZqqmhygkjtyuklEhiYXPWpqVGeO353aZ5yUZyYnznyPTgj4EiJE7kx0ti0/Og+gHPuOfJfB6GlErTAVhb/9KRuSNctkKoUilXwEoc',
                           `nseappid`= 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJhcGkubnNlIiwiYXVkIjoiYXBpLm5zZSIsImlhdCI6MTYyNzIzMjA1MCwiZXhwIjoxNjI3MjM1NjUwfQ.K5tPhAOjGrmeoqTxsCZRNoGkcJC6u5LkKWV3xZp7BQg', 
                           `nsit`= 'gPNMktA06L8VarxivBBgV8KD'
                 
               )
)

temp <- content(response)$grapthData

data_raw <- tibble::enframe(unlist(temp))


anytime(as.numeric(temp[[5]][1])/1000)


for(i in 1:length(temp)){
  print(as.numeric(temp[[i]][2][[1]]))
}


data_raw <- data.frame(data_raw)

View(data_raw)

data_raw$value <- as.numeric(data_raw$value)


str(data_raw)


content(response)$grapthData[[1]][1]

library(anytime)

anytime(as.numeric(content(response)$grapthData[[1]][1]))

anytime(as.numeric(content(response)$grapthData[[1]][1])/1e6)

as.Date(as.numeric(data_raw$value))
as.POSIXct(as.numeric(data_raw$value)/1e6,origin="1970-01-01")

as.POSIXct(as.numeric(content(response)$grapthData[[1]][1])/1000,origin="1970-01-01")

data_raw

dts <- c(as.numeric(content(response)$grapthData[[2]][1])/1000,as.numeric(content(response)$grapthData[[3]][1])/1000)


mydates = dts

class(mydates) = c('POSIXt','POSIXct')

mydates

as_datetime(data_raw$value)

as.POSIXct(data_raw$value, origin = "1970-01-01")


as.Date(as.POSIXct(as.numeric(data_raw$value), origin="1970-01-01"))







prices <- seq(700,950,1) # Vector of prices
strike <- 850 # strike price for both put and call 
premium_call <- 20 # option price call
premium_put <- 10  # option price put 

# call option payoff at expiration 
intrinsicValuesCall <- prices - strike - premium_call
payoffLongCall <- pmax(-premium_call,intrinsicValuesCall)

# put option payoff at expiration
intrinsicValuesPut <- strike - prices - premium_put
payoffLongPut <- pmax(-premium_put,intrinsicValuesPut)

# The payoff of the Strategy is the sum of the call and put payoff. Need
# to sum wise element by element between the two vectors
payoff <- rowSums(cbind(payoffLongCall,payoffLongPut))

# Make a DataFrame with all the variable to plot it with ggplot
results <- data.frame(cbind(prices,payoffLongCall,payoffLongPut,payoff))

ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
  geom_line(aes(y = payoffLongPut, color="LongPut"))+
  geom_line(aes(y=payoff, color = 'Payoff')) +
  scale_colour_manual("", 
                      breaks = c("LongCall", "LongPut", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Long Straddle Payoff")  





library("derivmkts")


s = 15763; v = 15750 ; v = 0 ; r = 0.038 ; tt = 30/365 ; d =0

s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
greeks(bscall(s, k, v, r, tt, d), complete=FALSE, long=FALSE, initcaps=TRUE)
greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))[c('Delta', 'Gamma'), ]
bsopt(s, k, v, r, tt, d)
bsopt(s, c(35, 40, 45), v, r, tt, d)
bsopt(s, c(35, 40, 45), v, r, tt, d)[['Call']][c('Delta', 'Gamma'), ]

## plot Greeks for calls and puts for 500 different stock prices
##
## This plot can generate a "figure margins too large" error
## in Rstudio
k <- 100; v <- 0.30; r <- 0.08; tt <- 2; d <- 0
S <- seq(.5, 250, by=.5)
Call <- greeks(bscall(S, k, v, r, tt, d))
Put <- greeks(bsput(S, k, v, r, tt, d))
y <- list(Call=Call, Put=Put)
par(mfrow=c(4, 4), mar=c(2, 2, 2, 2))  ## create a 4x4 plot
for (i in names(y)) {
  for (j in rownames(y[[i]])) {  ## loop over greeks
    plot(S, y[[i]][j, ], main=paste(i, j), ylab=j, type='l')
  }
}
## Not run: 
## Using complete option for calls
call_long <- greeks(bscall(S, k, v, r, tt, d), long=TRUE)
ggplot2::ggplot(call_long, aes(x=s, y=value)) +
  geom_line() + facet_wrap(~greek, scales='free')










#### Bull Call Spread

prices <- seq(400, 550,1) # Vector of prices
k_low = 450  # Low Strike price for call
k_high = 500 # High Strike Price for call 
premium_low = 10
premium_high = 1
# Intrinsic Value and Payoff long call
intrinsicValueLongCall <- prices - k_low - premium_low
payoffLongCall <- pmax(-premium_low,intrinsicValueLongCall)
# Intrinsic Value and Payoff short call
intrinsicValueShortCall <- prices - k_high - premium_high
payoffShortCall <- pmin(premium_high,-intrinsicValueShortCall)
# Strategy Payoff
payoff <- rowSums(cbind(payoffLongCall,payoffShortCall))
# Generate a dataframe with the payoffLongCall, payoffShortCall and payoff vectors
# in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(payoffLongCall,payoffShortCall,payoff))
ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
  geom_line(aes(y = payoffShortCall, color="ShortCall"))+
  geom_line(aes(y=payoff, color = 'Payoff')) +
  scale_colour_manual("", 
                      breaks = c("LongCall", "ShortCall", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Bull Call Spread Payoff") 






prices <- seq(14800, 16900,1) # Vector of prices
k_low = 15750  # Low Strike price for call
k_high = 15900 # High Strike Price for call 
premium_low = 87.35
premium_high = 25.05
# Intrinsic Value and Payoff long call
intrinsicValueLongCall <- prices - k_low - premium_low
payoffLongCall <- pmax(-premium_low,intrinsicValueLongCall)
# Intrinsic Value and Payoff short call
intrinsicValueShortCall <- prices - k_high - premium_high
payoffShortCall <- pmin(premium_high,-intrinsicValueShortCall)
# Strategy Payoff
payoff <- rowSums(cbind(payoffLongCall,payoffShortCall))
# Generate a dataframe with the payoffLongCall, payoffShortCall and payoff vectors
# in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(prices,payoffLongCall,payoffShortCall,payoff))

View(results)
results$shortline <- ifelse(results$payoff < 0 ,results$payoffShortCall,0)
results$longline <- ifelse(results$payoff >= 0,results$payoffLongCall,0)

ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) + 
  geom_line(aes(y = payoffShortCall, color="ShortCall"))+
  geom_line(aes(y=payoff, color = 'Payoff')) +
  geom_ribbon(data=results,aes(x = prices,ymin = shortline, ymax = payoff), inherit.aes = FALSE,fill = "red")+
  geom_ribbon(data=results,aes(x = prices,ymin = payoff, ymax = longline), inherit.aes = FALSE,fill = "green")+
  scale_colour_manual("", 
                      breaks = c("LongCall", "ShortCall", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Bull Call Spread Payoff") 




prices <- seq(14843, 16681,1) # Vector of prices
k_low = 15750  # Low Strike price for call
k_high = 15900 # High Strike Price for call 
premium_low = 87.35
premium_high = 25.05
# Intrinsic Value and Payoff long call
intrinsicValueLongCall <- prices - k_low - premium_low
payoffLongCall <- pmax(-premium_low,intrinsicValueLongCall)
payoff <- rowSums(cbind(payoffLongCall))

results <- data.frame(cbind(prices,payoffLongCall,payoff))
ggplot(results, aes(x=prices)) + 
  geom_line(aes(y = payoffLongCall, color = "LongCall")) +
  geom_line(aes(y=payoff, color = 'Payoff')) +
  scale_colour_manual("", 
                      breaks = c("LongCall", "Payoff"),
                      values = c("darkred", "darkgreen")) + ylab("Payoff")+
  ggtitle("Bull Call Spread Payoff") 

max_loss <- 50*min(results$payoff)
max_profit <- 50*max(results$payoff)




View(results)

profit_zone <- results[results$payoff >= 0,]$payoff
loss_zone <- results[results$payoff < 0,]$payoff

length(profit_zone)/(length(loss_zone)+length(profit_zone))




temp_data <- read.csv("~/Desktop/Options_Chain/indian_nifty_50_options_chain_raw.csv")
colnames(temp_data) <- c("ID","Call_askPrice","Call_askQty","Call_bidprice","Call_bidQty","Call_change","Call_changeinOpenInterest","Call_expiryDate","Call_identifier","Call_impliedVolatility","Call_lastPrice","Call_openInterest","Call_pChange","Call_pchangeinOpenInterest","Call_strikePrice","Call_totalBuyQuantity","Call_totalSellQuantity","Call_totalTradedVolume","Call_underlying","Call_underlyingValue","expiryDate","Put_askPrice","Put_askQty","Put_bidprice","Put_bidQty","Put_change","Put_changeinOpenInterest","Put_expiryDate","Put_identifier","Put_impliedVolatility","Put_lastPrice","Put_openInterest","Put_pChange","Put_pchangeinOpenInterest","Put_strikePrice","Put_totalBuyQuantity","Put_totalSellQuantity","Put_totalTradedVolume","Put_underlying","Put_underlyingValue","StrikePrice","current_time")

options_expiry <- "02-Sep-2021"
strike_price <- 16850

left_temp_data <- temp_data %>% dplyr::filter((Call_expiryDate == as.character(options_expiry)) &(Call_strikePrice == strike_price))
right_temp_data <- temp_data %>% dplyr::filter((Put_expiryDate == as.character(options_expiry)) &(Put_strikePrice == strike_price)) 

left_temp_data <- left_temp_data[complete.cases(left_temp_data), ]
right_temp_data <- right_temp_data[complete.cases(right_temp_data), ]

temp_data <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice,t1.current_time
                              from left_temp_data t1
                              inner join right_temp_data t2 on t1.Call_expiryDate = t2.Put_expiryDate and t1.Call_strikePrice = t2.Put_strikePrice and t1.current_time = t2.current_time
                                    ")


current_data <- temp_data[as.Date(temp_data$current_time) == Sys.Date(),]

rownames(current_data) <- NULL

current_data$`CE Interpretation` <- ifelse(current_data$Call_openInterest>0 & current_data$Call_pChange > 0,"Long Buildups",ifelse(current_data$Call_openInterest<0 & current_data$Call_pChange < 0,"Long Unwinding",ifelse(current_data$Call_openInterest>0 & current_data$Call_pChange < 0,"Shorts Buildups",ifelse(current_data$Call_openInterest<0 & current_data$Call_pChange > 0,"Short Coverings","-"))))
current_data$`PE Interpretation` <- ifelse(current_data$Put_changeinOpenInterest>0 & current_data$Put_pChange > 0,"Long Buildups",ifelse(current_data$Put_changeinOpenInterest<0 & current_data$Put_pChange < 0,"Long Unwinding",ifelse(current_data$Put_changeinOpenInterest>0 & current_data$Put_pChange < 0,"Shorts Buildups",ifelse(current_data$Put_changeinOpenInterest<0 & current_data$Put_pChange > 0,"Short Coverings","-"))))


formatted_data <- current_data %>% select(current_time,`CE Interpretation`,Call_lastPrice,Call_impliedVolatility,Call_openInterest,Call_strikePrice,expiryDate,`PE Interpretation`,Put_lastPrice,Put_impliedVolatility,Put_openInterest) %>% arrange(current_time)


View(formatted_data)
# current_Data <-temp_data %>% dplyr::filter((expiryDate == as.character(options_expiry)) &(strikePrice == strike_price))
# 
# tail(current_Data)
# 
# current_Data$exec_date <- as.Date(current_Data$current_time)
# tail(current_Data)
# 
# test <- current_Data %>% group_by(exec_date) %>% mutate(rnk = order(current_time,decreasing = TRUE), last_close = lag(exec_date))
# 
# test <- data.frame(test)
# 
# closing_data <- data.frame(test[test$rnk == 1,])
# 
# opening_data <- test[is.na(test$last_close),]
# 
# closing_data <- closing_data %>% mutate( next_close = lead(exec_date))
# 
# closing_data <- data.frame(closing_data)
# 
# final_dt <- sqldf("select distinct t1.Call_askPrice,t1.Call_askQty,t1.Call_bidprice,t1.Call_bidQty,t1.Call_change,t1.Call_changeinOpenInterest,t1.Call_expiryDate,t1.Call_identifier,t1.Call_impliedVolatility,t1.Call_lastPrice,t1.Call_openInterest,t1.Call_pChange,t1.Call_pchangeinOpenInterest,t1.Call_strikePrice,t1.Call_totalBuyQuantity,t1.Call_totalSellQuantity,t1.Call_totalTradedVolume,t1.Call_underlying,t1.Call_underlyingValue,t1.Call_expiryDate as expiryDate,t2.Put_askPrice,t2.Put_askQty,t2.Put_bidprice,t2.Put_bidQty,t2.Put_change,t2.Put_changeinOpenInterest,t2.Put_expiryDate,t2.Put_identifier,t2.Put_impliedVolatility,t2.Put_lastPrice,t2.Put_openInterest,t2.Put_pChange,t2.Put_pchangeinOpenInterest,t2.Put_strikePrice,t2.Put_totalBuyQuantity,t2.Put_totalSellQuantity,t2.Put_totalTradedVolume,t2.Put_underlying,t2.Put_underlyingValue,t1.Call_strikePrice as strikePrice,t1.current_time
#                               from opening_data t1
#                               inner join closing_data t2 on t1.exec_date = t2.next_close
#                                     ")


temp_data %>% 
  mutate(myHour = hour(current_time))



nse_data <- read.csv(paste0(getwd(),"/data/Nifty50_Stocks.csv", sep = ""))

for(i in 1:nrow(nse_data)){
  stock = nse_data[i,2]

  response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&useYfid=true&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
  
  
  stock_timestamp <- response_data$chart$result[[1]]$timestamp
  Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
  High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
  Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
  Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
  Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
  final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
  
  # browser()
  
  if(nrow(final_data) == 0){
    stock_timestamp <- response_data$chart$result[[2]][[1]]
    Close <- response_data$chart$result[[3]]$quote[[1]]$close
    High <- response_data$chart$result[[3]]$quote[[1]]$high
    Low <- response_data$chart$result[[3]]$quote[[1]]$low
    Open <- response_data$chart$result[[3]]$quote[[1]]$open
    Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
    
    
    final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
  }
  
  
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
    
    final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
    
    final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
  }else{
    final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
    
    final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
  }
  final_data <- na.omit(final_data)
  
  
  final_data <- final_data %>%
    select(dates, Open, High, Low, Close, Volume) %>%
    mutate(
      ema_50 = TTR::EMA(Close, 50)
    )
  
  
  if(weekdays(Sys.Date()) == "Sunday"){
    final_data <- final_data[as.Date(final_data$dates) == Sys.Date() -1 ,]
  }else if(weekdays(Sys.Date()) == "Saturday"){
    final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 2 ,]
  }else{
    if(hour(Sys.time()) >= 9){
      final_data <- final_data[as.Date(final_data$dates) == Sys.Date(),]
    }else{
      final_data <- final_data[as.Date(final_data$dates) == Sys.Date() - 1,]
    }
    
  }
  
  
  temp <- head(final_data,1)
  
  if(100*abs(temp[1,"Close"] - temp[1,"ema_50"])/temp[1,"ema_50"] >= 2){
    print(stock)
    print("Passed")
  }else{
  }
}






library("smartapi")
library("stringr")

login_params = list(api_key = 'LPUVlRxd')

login_object = create_connection_object(login_params)

session_data <- generate_session(login_object,"J95213","startteja123")

Spot_Price <- as.numeric("17150")
side_dir <- "CE"


angel_script <- read.csv('~/Downloads/angel_script.csv')

nifty_script_data <- angel_script[angel_script$name == "NIFTY" & angel_script$expiry == "09SEP2021",]
rownames(nifty_script_data) <- NULL
nifty_script_data$side_dir <- str_sub(nifty_script_data$symbol, start= -2)


current_calls_df = data.frame(current_symbol = character(0),current_token = character(0),ltp_price=numeric(0),quantity = numeric(0),expirydate=character(0),strikeprice=character(0),orderid=character(0),transactiontype=character(0),exchange=character(0),ordertype=character(0),producttype=character(0),duration=character(0),status=character(0))
counter = 1

#### Run this if the time is 10 on Thursday
for (ind in 1:nrow(nifty_script_data)) {
  
  current_Spot <- nifty_script_data[ind,"strike"]
  
  current_symbol <- nifty_script_data[ind,"symbol"]
  
  current_token <- nifty_script_data[ind,"token"]
  
  current_dir <- nifty_script_data[ind,"side_dir"]
  
  tryCatch(
    {
      ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
      
      ltp_price <- ltp_data$data$ltp
    },
    error = function(cond){
      print(cond)
      
      Sys.sleep(1)
      
      ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))

      ltp_price <- ltp_data$data$ltp
    }
  )
  
  if(between(ltp_price, 22, 27)){
    
    current_calls_df[counter,"current_symbol"] <- current_symbol
    current_calls_df[counter,"current_token"] <- current_token
    current_calls_df[counter,"ltp_price"] <- ltp_price
    current_calls_df[counter,"quantity"] <- 1
    current_calls_df[counter,"expirydate"] <- "09SEP2021"
    current_calls_df[counter,"strikeprice"] <- current_Spot
    current_calls_df[counter,"orderid"] <- 1
    current_calls_df[counter,"transactiontype"] <- "SELL"
    current_calls_df[counter,"exchange"] <- "NFO"
    current_calls_df[counter,"ordertype"] <- "LIMIT"
    current_calls_df[counter,"producttype"] <- "CARRYFORWARD"
    current_calls_df[counter,"duration"] <- "DAY"
    current_calls_df[counter,"side_dir"] <- current_dir
    current_calls_df[counter,"status"] <- "open"
    
    counter = counter + 1
    
  }
  
}

print(current_calls_df)


call_premium_price <- 0
put_premium_price <- 0

for (ord in 1:nrow(current_calls_df)) {

  ### Check oly the open orders from all of them
  if(current_calls_df[ord,"status"] == "open"){
    if(str_sub(current_calls_df[ord,"current_symbol"], start= -2) == "CE"){
      ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_calls_df[ord,"current_symbol"]),symboltoken = as.character(current_calls_df[ord,"current_token"]))
      call_premium_price <- ltp_data$data$ltp
    }else{
      ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_calls_df[ord,"current_symbol"]),symboltoken = as.character(current_calls_df[ord,"current_token"]))
      put_premium_price <- ltp_data$data$ltp
    }
  }else{
    print("order completed")
  }
}

temp_current_calls_df = data.frame(current_symbol = character(0),current_token = character(0),ltp_price=numeric(0),quantity = numeric(0),expirydate=character(0),strikeprice=character(0),orderid=character(0),transactiontype=character(0),exchange=character(0),ordertype=character(0),producttype=character(0),duration=character(0),status=character(0))
smallest_value <- 0

if((abs(call_premium_price-put_premium_price)/max(put_premium_price,call_premium_price)) >= 0.5){
  
  ## If market is Bullish
  if(put_premium_price < call_premium_price){
    ## Square off the Put premium
    
    current_calls_df[current_calls_df$side_dir == "PE",]$status <- "complete"
    
    ## Take the new Put premium near the Call premium price
    
    put_premium_data <- nifty_script_data[nifty_script_data$side_dir == "PE",]
    rownames(put_premium_data) <- NULL
    
    
    for (ind in 1:nrow(put_premium_data)) {
      
      current_Spot <- put_premium_data[ind,"strike"]
      
      current_symbol <- put_premium_data[ind,"symbol"]
      
      current_token <- put_premium_data[ind,"token"]
      
      tryCatch(
        {
          ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
          
          ltp_price <- ltp_data$data$ltp
        },
        error = function(cond){
          print(cond)
          
          Sys.sleep(1)
          
          ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
          
          ltp_price <- ltp_data$data$ltp
        }
      )
      
      if(ind == 1){
        smallest_value <- ltp_price
        
        temp_current_calls_df[1,"current_symbol"] <- current_symbol
        temp_current_calls_df[1,"current_token"] <- current_token
        temp_current_calls_df[1,"ltp_price"] <- ltp_price
        temp_current_calls_df[1,"quantity"] <- 1
        temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
        temp_current_calls_df[1,"strikeprice"] <- current_Spot
        temp_current_calls_df[1,"orderid"] <- 1
        temp_current_calls_df[1,"transactiontype"] <- "SELL"
        temp_current_calls_df[1,"exchange"] <- "NFO"
        temp_current_calls_df[1,"ordertype"] <- "LIMIT"
        temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
        temp_current_calls_df[1,"duration"] <- "DAY"
        temp_current_calls_df[1,"side_dir"] <- "PE"
        temp_current_calls_df[1,"status"] <- "open"
        
      }else{
        
        
        if(ltp_price >= call_premium_price & ltp_price <= smallest_value){
          
          print(ind)
          
          smallest_value <- ltp_price
          
          temp_current_calls_df[1,"current_symbol"] <- current_symbol
          temp_current_calls_df[1,"current_token"] <- current_token
          temp_current_calls_df[1,"ltp_price"] <- ltp_price
          temp_current_calls_df[1,"quantity"] <- 1
          temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
          temp_current_calls_df[1,"strikeprice"] <- current_Spot
          temp_current_calls_df[1,"orderid"] <- 1
          temp_current_calls_df[1,"transactiontype"] <- "SELL"
          temp_current_calls_df[1,"exchange"] <- "NFO"
          temp_current_calls_df[1,"ordertype"] <- "LIMIT"
          temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
          temp_current_calls_df[1,"duration"] <- "DAY"
          temp_current_calls_df[1,"side_dir"] <- "PE"
          temp_current_calls_df[1,"status"] <- "open"
          
        }
        
      }
      
      
    }
    
  }else{
    ## Bearish Market
    
    ## Square off the Call premium
    # print(nifty_script_data)
    
    current_calls_df[current_calls_df$side_dir == "CE",]$status <- "complete"
    
    ## Take the new Call premium near the Put premium price
    call_premium_data <- nifty_script_data[nifty_script_data$side_dir == "CE",]
    rownames(call_premium_data) <- NULL
    
    for (ind in 1:nrow(call_premium_data)) {
      
      current_Spot <- call_premium_data[ind,"strike"]
      
      current_symbol <- call_premium_data[ind,"symbol"]
      
      current_token <- call_premium_data[ind,"token"]
      
      tryCatch(
        {
          ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
          
          ltp_price <- ltp_data$data$ltp
        },
        error = function(cond){
          print(cond)
          
          Sys.sleep(1)
          
          ltp_data <- get_ltp_data(object = session_data,exchange = "NFO",tradingsymbol = as.character(current_symbol),symboltoken = as.character(current_token))
          
          ltp_price <- ltp_data$data$ltp
        }
      )
      
      if(ind == 1){
        smallest_value <- ltp_price
        
        temp_current_calls_df[1,"current_symbol"] <- current_symbol
        temp_current_calls_df[1,"current_token"] <- current_token
        temp_current_calls_df[1,"ltp_price"] <- ltp_price
        temp_current_calls_df[1,"quantity"] <- 1
        temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
        temp_current_calls_df[1,"strikeprice"] <- current_Spot
        temp_current_calls_df[1,"orderid"] <- 1
        temp_current_calls_df[1,"transactiontype"] <- "SELL"
        temp_current_calls_df[1,"exchange"] <- "NFO"
        temp_current_calls_df[1,"ordertype"] <- "LIMIT"
        temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
        temp_current_calls_df[1,"duration"] <- "DAY"
        temp_current_calls_df[1,"side_dir"] <- "CE"
        temp_current_calls_df[1,"status"] <- "open"
        
      }else{
        
        if(ltp_price <= put_premium_price & ltp_price >= smallest_value){
          
          smallest_value <- ltp_price
          
          temp_current_calls_df[1,"current_symbol"] <- current_symbol
          temp_current_calls_df[1,"current_token"] <- current_token
          temp_current_calls_df[1,"ltp_price"] <- ltp_price
          temp_current_calls_df[1,"quantity"] <- 1
          temp_current_calls_df[1,"expirydate"] <- "09SEP2021"
          temp_current_calls_df[1,"strikeprice"] <- current_Spot
          temp_current_calls_df[1,"orderid"] <- 1
          temp_current_calls_df[1,"transactiontype"] <- "SELL"
          temp_current_calls_df[1,"exchange"] <- "NFO"
          temp_current_calls_df[1,"ordertype"] <- "LIMIT"
          temp_current_calls_df[1,"producttype"] <- "CARRYFORWARD"
          temp_current_calls_df[1,"duration"] <- "DAY"
          temp_current_calls_df[1,"side_dir"] <- "CE"
          temp_current_calls_df[1,"status"] <- "open"
          
        }
        
      }
      
      
    }
    
  }
}

if(nrow(temp_current_calls_df) > 0){
  current_calls_df <- rbind(current_calls_df,temp_current_calls_df)
}