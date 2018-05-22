library(magrittr)
library(midasr)
library(zoo)
library(ggplot2)
library(forecast)
library(data.table)
library(FactoMineR)
library(tseries)

# "2016-12-28"

format_daily = function(rowdata, window_start_data = "2005-12-28", 
                        window_end_data = "2017-12-28", logdif = FALSE, StationaryTest = TRUE){
  
  
  dt.data <- data.table(rowdata)
  z.data <- zoo(rowdata$PX_LAST, order.by=as.Date(rowdata$date, format='%y-%m-%d'))
  s = start(z.data)#start date
  e = end(z.data)#end date
  
  ## getting the starting date of that mounth
  start_date = as.Date(as.yearmon(start(z.data)), frac = 0)
  end_date = e
  
  ######################
  #observed last day
  setkey(dt.data, date)
  all_dates <- seq(from = as.Date(start_date),
                   to = as.Date(e),
                   by = "days")
  
  dt.data2 <- dt.data[J(all_dates), roll=Inf ]
  
  z.data = zoo(dt.data2$PX_LAST, seq(from = as.Date(start_date), to = as.Date(end_date), by = 1))
  ###########Sample Code############
  # # given daily series keep only first point in each month at
  # # day 21 or more
  # z <- zoo(101:200, as.Date("2000-01-01") + seq(0, length = 100, by = 1))
  # zz <- z[as.numeric(format(time(z), "%d")) <= 21]
  # zz[!duplicated(as.yearmon(time(zz)))]
  
  ########## In Our Case #############
  # given daily series keep only first 28 days
  z.data2 <- z.data[as.numeric(format(time(z.data), "%d")) <= 28]
  #######################
  # length(z.data2)
  data.window <- window(z.data2,start = window_start_data ,end = window_end_data)#get the appropriate part
  ##
  adf_test <- adf.test(data.window,alternative = 'stationary')
  # print(adf_test)#Not Stationary
  # adf_test2 <- adf.test(dollar.decomposed,alternative = 'stationary')
  # print(adf_test2)#Stationary
  # 
  # print(adf_test$p.value)
  isStationarity <- ifelse(test = adf_test$p.value<0.05,yes = TRUE, no = FALSE)
  # print(isStationarity)
  ##
  if(StationaryTest == FALSE){
    ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    return(ts.data.final)
  }
  if(isStationarity == FALSE){
    
    if(logdif == TRUE){
      data.window.stat <- diff(log(data.window))
      #NAs
      if(anyNA(data.window.stat)){
        data.window.stat[is.na(data.window.stat)] <- 0
      }
      ##
      adf_test2 <- adf.test(data.window.stat,alternative = 'stationary')
      # print(adf_test2$p.value)
      isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
      # print(isStationarity2)
      ##
      ts.data.final=ts(data.window.stat)#return as time series
      return(ts.data.final)
    }
    
    # not logdif, decomposition method
    else{
      out <- tryCatch(
        {
          message("Trying Classical Seasonal Decomposition by Moving Averages...")
          
          ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
          dmon <- decompose_daily(x = ts.data.final)
          #
          adf_test2 <- adf.test(dmon,alternative = 'stationary')
          # print(adf_test2$p.value)
          isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
          # print(isStationarity2)
          #
          dmon
          #   
          # The return value is the actual value 
          # that will be returned in case there is no condition 
          # (e.g. warning or error). 
          # You don't need to state the return value via `return()` as code 
          # in the "try" part is not wrapped insided a function (unlike that
          # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
          message(paste("Na's created, will use logdiff instead..."))
          # message("Here's the original error message:")
          # message(cond)
          # Choose a return value in case of error
          data.window.stat <- diff(log(data.window))
          #NAs
          if(anyNA(data.window.stat)){
            data.window.stat[is.na(data.window.stat)] <- 0
          }
          ##
          adf_test2 <- adf.test(data.window.stat,alternative = 'stationary')
          # print(adf_test2$p.value)
          isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
          # print(isStationarity2)
          ##
          ts.data.final=ts(data.window.stat)#return as time series
          return(ts.data.final)
          
        }
      )
    }
    
  }
  else{
    ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    return(ts.data.final)
  }
  
}
#########
# format_weekly = function(rowdata, window_start_data = "2005-12-28", 
#                          window_end_data = "2016-12-28"){
#   
#  
#   
#   # create the index array comprised of date objects
#     ndx = seq(as.Date(window_start_data) ,as.Date(window_end_data) , by='weeks')
#     class(ndx)
#     
#     length(ndx)
#    
#     
#     # create a fake data array
#       x = 1:length(ndx)
#       mydata = sin(x/2)
#       
#       # import a time series library 
#         require(xts)
#       
#       # create the time series
#         myts = xts(rowdata$PX_LAST, order.by=ndx)
#         
#         
#   if(isStationarity == FALSE){
#     data.window.stat <- diff(log(myts))
#     ts.data.final=ts(data.window.stat)#return as time series
#     return(ts.data.final)
#   }
#   else{
#     ts.data.final=ts(myts[seq(2,length(myts))])#return as time series
#     return(ts.data.final)
#   }
# }
#########

format_monthly = function(rowdata, window_start_data = "2005-12-28", 
                          window_end_data = "2017-12-28", logdif = FALSE, StationaryTest = TRUE){
  
  dt.data <- data.table(rowdata)
  z.data <- zoo(rowdata$PX_LAST, order.by=as.yearmon(rowdata$date, format='%y-%m-%d'))
  s = start(z.data)#start date
  e = end(z.data)#end date
  
  z.data2 = na.locf(z.data)# fill na with last observed
  
  
  data.window <- window(z.data2,start = window_start_data ,end = window_end_data)#get the appropriate part 
  ##
  adf_test <- adf.test(data.window,alternative = 'stationary')
  # print(adf_test)#Not Stationary
  # adf_test2 <- adf.test(dollar.decomposed,alternative = 'stationary')
  # print(adf_test2)#Stationary
  # 
  # print(adf_test$p.value)
  isStationarity <- ifelse(test = adf_test$p.value<0.05,yes = TRUE, no = FALSE)
  # print(isStationarity)
  ##
  if(StationaryTest == FALSE){
    ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    return(ts.data.final)
  }
  if(isStationarity == FALSE){
    
    if(logdif == TRUE){
      data.window.stat <- diff(log(data.window))
      #NAs
      if(anyNA(data.window.stat)){
        data.window.stat[is.na(data.window.stat)] <- 0
      }
      ##
      adf_test2 <- adf.test(data.window.stat,alternative = 'stationary')
      # print(adf_test2$p.value)
      isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
      # print(isStationarity2)
      ##
      ts.data.final=ts(data.window.stat)#return as time series
      return(ts.data.final)
    }
    
    # not logdif, decomposition method
    else{
        out <- tryCatch(
          {
            message("Trying Classical Seasonal Decomposition by Moving Averages...")
            
            ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
            dmon <- decompose_monthly(x = ts.data.final)
            #
            adf_test2 <- adf.test(dmon,alternative = 'stationary')
            # print(adf_test2$p.value)
            isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
            # print(isStationarity2)
            #
            dmon
            #   
            # The return value is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped insided a function (unlike that
            # for the condition handlers for warnings and error below)
          },
          error=function(cond) {
            message(paste("Na's created, will use logdiff instead..."))
            # message("Here's the original error message:")
            # message(cond)
            # Choose a return value in case of error
            data.window.stat <- diff(log(data.window))
            #NAs
            if(anyNA(data.window.stat)){
              data.window.stat[is.na(data.window.stat)] <- 0
            }
            ##
            adf_test2 <- adf.test(data.window.stat,alternative = 'stationary')
            # print(adf_test2$p.value)
            isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
            # print(isStationarity2)
            ##
            ts.data.final=ts(data.window.stat)#return as time series
            return(ts.data.final)
            
          }
        )
    }
  
  }
  #if stationary, no transformation  
  else{
    ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    return(ts.data.final)
  }
}

format_quarterly = function(rowdata, window_start_data = "2005-12-28", 
                            window_end_data = "2017-12-28", logdif = FALSE, StationaryTest = TRUE){
  
  dt.data <- data.table(rowdata)
  z.data <- zoo(rowdata$PX_LAST, order.by=as.yearqtr(rowdata$date, format='%y-%m-%d'))
  s = start(z.data)#start date
  e = end(z.data)#end date
  
  z.data2 = na.locf(z.data)# fill na with last observed
  
  data.window <- window(z.data2,start = as.yearqtr(as.Date(window_start_data)) ,
                        end = as.yearqtr(as.Date(window_end_data)))#get the appropriate part 
  ##
  adf_test <- adf.test(data.window,alternative = 'stationary')
  # print(adf_test)#Not Stationary
  # adf_test2 <- adf.test(dollar.decomposed,alternative = 'stationary')
  # print(adf_test2)#Stationary
  # print(adf_test$p.value)
  isStationarity <- ifelse(test = adf_test$p.value<0.05,yes = TRUE, no = FALSE)
  # print(isStationarity)
  ##
  if(StationaryTest == FALSE){
    ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    return(ts.data.final)
  }
  if(isStationarity == FALSE){
    if(logdif == TRUE){
      data.window.stat <- diff(log(data.window))
      #NAs
      if(anyNA(data.window.stat)){
        data.window.stat[is.na(data.window.stat)] <- 0
      }
      ts.data.final=ts(data.window.stat)#return as time series
      ##
      adf_test2 <- adf.test(data.window.stat,alternative = 'stationary')
      # print(adf_test2$p.value)
      isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
      # print(isStationarity2)
      ##
      return(ts.data.final)
    }
    
    # else{
    #   ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    #   ##
    #   adf_test2 <- adf.test(decompose_quarterly(x = ts.data.final),alternative = 'stationary')
    #   print(adf_test2$p.value)
    #   isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
    #   print(isStationarity2)
    #   ##
    #   return(decompose_quarterly(x = ts.data.final) )
    # }
    # not logdif, decomposition method
    else{
      out <- tryCatch(
        {
          message("Trying Classical Seasonal Decomposition by Moving Averages...")
          
          ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
          dmon <- decompose_quarterly(x = ts.data.final)
          #
          adf_test2 <- adf.test(dmon,alternative = 'stationary')
          # print(adf_test2$p.value)
          isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
          # print(isStationarity2)
          #
          dmon
          #   
          # The return value is the actual value 
          # that will be returned in case there is no condition 
          # (e.g. warning or error). 
        },
        error=function(cond) {
          message(paste("Na's created, will use logdiff instead..."))
          # message("Here's the original error message:")
          # message(cond)
          # Choose a return value in case of error
          data.window.stat <- diff(log(data.window))
          #NAs
          if(anyNA(data.window.stat)){
            data.window.stat[is.na(data.window.stat)] <- 0
          }
          ##
          adf_test2 <- adf.test(data.window.stat,alternative = 'stationary')
          # print(adf_test2$p.value)
          isStationarity2 <- ifelse(test = adf_test2$p.value<0.05,yes = TRUE, no = FALSE)
          # print(isStationarity2)
          ##
          ts.data.final=ts(data.window.stat)#return as time series
          return(ts.data.final)
          
        }
      )
    }
    
  }
  else{
    ts.data.final=ts(data.window[seq(2,length(data.window))])#return as time series
    return(ts.data.final)
  }
  
  # length(data.window.logdif)
  # plot(data.window.logdif)
  
}

getPCA <- function( timeseries_vector, number0fPC = 3, showgraph = FALSE){
  # head(daily_series)
  pca = PCA(timeseries_vector, graph = showgraph)
  # pca$eig
  # head(pca$ind$coord)
  result = pca$ind$coord[,1:number0fPC]
  return(result)
}

decompose_daily = function(x, freq=336){
  # remainder, trend, seasonal
  set.seed(1)
  series <- ts(x,frequency=freq)
  decomposed <- stl(series,s.window="periodic")
  decomposed$time.series[,"remainder"]
  
}

decompose_weekly = function(x, freq=54){
  set.seed(1)
  series <- ts(x,frequency=54)
  decomposed <- stl(series,s.window="periodic")
  decomposed$time.series[,"remainder"]
  
}

decompose_monthly = function(x, freq=12){
  # seasonal, trend, random
  set.seed(1)
  ts_data=ts(x,frequency = 12)
  decomposed= decompose(ts_data,"multiplicative")
  decomposed$random
  # adjust_decomposed=ts_data/(decomposed$seasonal*decomposed$trend)
  
}

decompose_quarterly = function(x, freq=4){
  # seasonal, trend, random
  set.seed(1)
  ts_data=ts(x,frequency = 4)
  decomposed= decompose(ts_data,"additive")
  decomposed$random
  # adjust_decomposed=ts_data-decomposed$seasonal-decomposed$trend
  
}

is.weekend <- function(x){
  x <- as.POSIXlt(x) 
  res = x$wday > 5 | x$wday < 1
  return(res)}

funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  #names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}
######### Lags Plot #######
lags_plot = function(series){
  lags = data.frame(series)

  ggplot(lags, aes(1:length(lags[,1]))) +
    geom_line(aes(y = lags[,1], colour = "present")) +
    geom_line(aes(y = lags[,2], colour = "1st lag")) +
    labs(x = "Low Frequency Period") +
    labs(y = "Value")
    # geom_line(aes(y = X.2.m, colour = "2st lag"))
  # geom_line(aes(y = X.3.m, colour = "3st lag")) +
  # geom_line(aes(y = X.4.m, colour = "4st lag")) +
  # geom_line(aes(y = X.5.m, colour = "5st lag"))

}

######### Stationary Test #########


# library(urca)
# udf_test <- ur.df(spx_dollar[,2], type='trend', lags = 10, selectlags = "BIC")
# summary(udf_test)

# dollar.decomposed <- format_daily(rowdata = spx_dollar,logdif = FALSE)
# dollar.decomposed %>% plot(type='l')


# udf_test2 <- ur.df(dollar.decomposed, type='trend', lags = 10, selectlags = "BIC")
# summary(udf_test2)

# library(tseries)
# adf_test <- adf.test(spx_dollar[,2],alternative = 'stationary')
# print(adf_test)#Not Stationary
# adf_test2 <- adf.test(dollar.decomposed,alternative = 'stationary')
# print(adf_test2)#Stationary

# ifelse(test = adf_test2$p.value<0.05,yes) adf_test2$p.value
# 
####### Lag #########
# lags_plot(mls.gdp)
# lags_plot(mls.bist)
# lags_plot(mls.dollar)
# lags_plot(mls.unemployment)
# lags_plot(mls.total_export)


# plot = function(){
#   trend_unemp = ma(unemployment$PX_LAST, order = 12, centre = T)
# # autoplot.zoo(object = as.zoo(unemployment$PX_LAST)) + xlab('date') + ylab('unemployment')
# plot(as.ts(unemployment$PX_LAST),ylab='Unemployment', xlab = 'Date', lwd = 2)
# lines(trend_unemp, col = 'red', lwd = 3)
# # plot(as.ts(trend_unemp))
# 
# 
# unemp.ts <- ts(diff(log(unemployment$PX_LAST)), start=c(2005, 1), end=c(2017, 8), frequency=12)
# 
# # subset the time series (June 2014 to December 2014)
# unemp.ts.2 <- window(unemp.ts, start=c(2014, 6), end=c(2014, 12))
# 
# 
# plot(as.ts(realgdp$GDP),ylab='RealGdp', xlab = 'Date', lwd = 2)
# trend_gdp = ma(realgdp$GDP, order = 4, centre = T)
# lines(trend_gdp, col = 'red', lwd = 3)
# 
# plot(as.ts(bist100$PX_LAST),ylab='Bist100', xlab = 'Date', lwd = 2)
# trend_bist100 = ma(bist100$PX_LAST, order = 30, centre = T)
# lines(trend_bist100, col = 'red', lwd = 3)
# 
# plot(bist100$PX_LAST,type = 'l')
# # plot series
# plot(unemp.ts, lwd=2, ylab='Stationary Unemployment')
# # This will fit in a line
# abline(reg=lm(unemp.ts~time(unemp.ts)), col='orange', lwd=3)
# 
# cycle(unemp.ts)
# #This will print the cycle across years.
# 
# plot(aggregate(unemp.ts,FUN=mean),ylab='Aggregated Unemployment')
# #This will aggregate the cycles and display a year on year trend
# 
# 
# boxplot(unemp.ts~cycle(unemp.ts), col='orange' )
# #Box plot across months will give us a sense on seasonal effect
# 
# #acf(diff(log(unemployment$PX_LAST)))
# 
# #dt.bist2 <- na.locf(m)
# realgdp_sta <- diff(log(realgdp$NAEXKP01TRQ652S))[29:76] # y -> realgdp
# unemployment_sta <- diff(log(unemployment$PX_LAST))[1:144]  #monthly data x1 -> unemployment
# }