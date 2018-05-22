## Note that sources.R and maindatas.RData should be in your current directory
source("sources.R")
# load("Datalar/maindatas.RData")
####  New GDP ######
# remove(gdp)
load("Datalar/gdp.RData")
######## Converting Frequencies of data #######
gdp.ts.final = format_quarterly(gdp,StationaryTest = FALSE)
gdp.ts.final %>% plot()

y = gdp.ts.final

trend = seq(from = gdp$PX_LAST[1], to = gdp$PX_LAST[length(gdp$date)],
            length.out = length(gdp$date))/gdp$PX_LAST[1]

# trend = format_quarterly(gdp)

plot(trend)
########## PCA ########### 
load("Datalar/son/data2.RData")
###
# spx_Turkey_quarterly[2] %>% data.frame() %>% tail(1)
# spx_Turkey_daily[1] %>% data.frame() %>% tail(1)
# spx_Turkey_monthly[1] %>% data.frame() %>% tail(1)
# 
# a <- format_daily(spx_Turkey_daily[[1]],StationaryTest = FALSE, window_end_data = "2017-12-28")
# length(a)
# 
# g <- format_quarterly(gdp,StationaryTest = FALSE,window_end_data = "2017-12-28")
# length(g)
###

spx_Turkey_monthly <- spx_Turkey_monthly[2:29]
spx_Turkey_monthly <- spx_Turkey_monthly[-c(11,14)]

# format_quarterly(spx_Turkey_monthly[[1]]) %>% length()

daily_spxturkey = lapply(spx_Turkey_daily, FUN = format_daily)

monthly_spxturkey = lapply(spx_Turkey_monthly, FUN = format_monthly)

# quarterly_spxturkey = lapply(spx_Turkey_quarterly, FUN = format_quarterly)


# monthly_spxturkey[[1]] %>% plot
# quarterly_spxturkey$`BDSRTRP Index` %>% plot
# pca_daily = getPCA(daily_series)
# 

pca_daily_spxturkey = getPCA(daily_spxturkey, showgraph = TRUE, number0fPC = 5)

pca_monthly_spxturkey = getPCA(monthly_spxturkey, showgraph = TRUE, number0fPC = 5)

# pca_quarterly_spxturkey = getPCA(quarterly_spxturkey, showgraph = TRUE, number0fPC = 2)


pca_monthly_spxturkey_1 <- pca_monthly_spxturkey[,1]
pca_monthly_spxturkey_2 <- pca_monthly_spxturkey[,2]
pca_monthly_spxturkey_3 <- pca_monthly_spxturkey[,3]
pca_monthly_spxturkey_4 <- pca_monthly_spxturkey[,4]
pca_monthly_spxturkey_5 <- pca_monthly_spxturkey[,5]

pca_daily_spxturkey_1 <- pca_daily_spxturkey[,1]
pca_daily_spxturkey_2 <- pca_daily_spxturkey[,2]
pca_daily_spxturkey_3 <- pca_daily_spxturkey[,3]
pca_daily_spxturkey_4 <- pca_daily_spxturkey[,4]
pca_daily_spxturkey_5 <- pca_daily_spxturkey[,5]

# pca_quarterly_spxturkey_1 <- pca_quarterly_spxturkey[,1]
# pca_quarterly_spxturkey_2 <- pca_quarterly_spxturkey[,2]

############ Nowcast #############
########## Nowcasting Almon Models PCA  ###########
trend = trend[1:48]
{
  almon_model1_pca  = midas_r(y ~ 
                               trend +
                               
                               mls(gdp.ts.final ,k = 1:3, m = 1) + 
                               
                               mls(pca_monthly_spxturkey_1, 0:2, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_2, 0:2, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_3, 0:2, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_4, 0:2, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_5, 0:2, m = 3,nealmon) +
                               
                               mls(pca_daily_spxturkey_1, 0:2, m = 3*28,nealmon) +
                               mls(pca_daily_spxturkey_2, 0:2, m = 3*28,nealmon) + 
                               mls(pca_daily_spxturkey_3, 0:2, m = 3*28,nealmon) +
                               mls(pca_daily_spxturkey_4, 0:2, m = 3*28,nealmon) +
                               mls(pca_daily_spxturkey_5, 0:2, m = 3*28,nealmon) ,
                               
                               # fmls(pca_quarterly_spxturkey_1, 2, m = 1,nealmon) +
                               # fmls(pca_quarterly_spxturkey_2, 2, m = 1,nealmon), 
                             
                             start = list(
                               # gdp.ts.final = c(1,-0.5),
                               pca_monthly_spxturkey_1 = c(1, -0.5, -0.2),
                               pca_monthly_spxturkey_2 = c(1, -0.5, -0.2),
                               pca_monthly_spxturkey_3 = c(1, -0.5, -0.2),
                               pca_monthly_spxturkey_4 = c(1, -0.5, -0.2),
                               pca_monthly_spxturkey_5 = c(1, -0.5, -0.2),
                               
                               pca_daily_spxturkey_1 = c(1, -0.5, -0.2),
                               pca_daily_spxturkey_2 = c(1, -0.5, -0.2),
                               pca_daily_spxturkey_3 = c(1, -0.5, -0.2),
                               pca_daily_spxturkey_4 = c(1, -0.5, -0.2),
                               pca_daily_spxturkey_5 = c(1, -0.5, -0.2)
                               
                               # pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                               # pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                             )
                             
                             
)
  
  summary(almon_model1_pca)
  coef(almon_model1_pca, midas = TRUE)
  agk.test(almon_model1_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model1_pca$start_opt
  
  #forecast horizon
  s=47
  e=48
  
  almon_model1_pca_forecast <-forecast(almon_model1_pca, list(
    
    trend = trend[(s+1):e],
    
    gdp.ts.final = gdp.ts.final[(s):(e-1)],
    
    pca_monthly_spxturkey_1 = pca_monthly_spxturkey_1[(s*3+1):(e*3)],
    pca_monthly_spxturkey_2 = pca_monthly_spxturkey_2[(s*3+1):(e*3)],
    pca_monthly_spxturkey_3 = pca_monthly_spxturkey_3[(s*3+1):(e*3)],
    pca_monthly_spxturkey_4 = pca_monthly_spxturkey_4[(s*3+1):(e*3)],
    pca_monthly_spxturkey_5 = pca_monthly_spxturkey_5[(s*3+1):(e*3)],
    
    pca_daily_spxturkey_1 = pca_daily_spxturkey_1[(s*84+1):(e*84)],
    pca_daily_spxturkey_2 = pca_daily_spxturkey_2[(s*84+1):(e*84)],
    pca_daily_spxturkey_3 = pca_daily_spxturkey_3[(s*84+1):(e*84)],
    pca_daily_spxturkey_4 = pca_daily_spxturkey_4[(s*84+1):(e*84)],
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)]
    
    # pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    # pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model1_pca_forecast$mean %>% length()
  gdp_forecast=c()
  gdp_forecast[1]=almon_model1_pca_forecast$mean
  summary(almon_model1_pca_forecast)
  plot(x=1:48,y=y[1:48], type = 'b', xlim = c(0,53), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  abline(v = 48, lty = 3, col ='red')
  text(48, -6, '2017-Q4', col = 'red',lty = 3 )
  lines(x=(s+1):e, y=almon_model1_pca_forecast$mean, type = 'b', col = 'orange', lwd =2, pch=4)
  legend(40, 28, c('Almon-Nowcast'), col = c('orange'), lty = 8, pch = 4, pt.lwd = 2, cex = 0.9, bty = 'n')
  almon_model1_pca_forecast$mean %>% length()
  
  # ## Residuals ##
  # residuals1 <- y[(s+1):e] - almon_model1_pca_forecast$mean
  # residuals1 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model NowCasting Errors')
  # abline(h = 0, col='red')
  # text(38, 4.5, sprintf("MSE: %3.2f ", mean(residuals1^2)) )
  # mean(residuals1^2)
  ##
  
}

############ 1 step ahead forecast #############

########## Almon Models PCA  ###########
# trend = trend[1:48]
{
  almon_model2.1_pca  = midas_r(y ~ 
                                 trend +
                                 
                                 mls(gdp.ts.final ,k = 1:3, m = 1) + 
                                 
                                 mls(pca_monthly_spxturkey_1, 1:3, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_2, 1:3, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_3, 1:3, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_4, 1:3, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_5, 1:3, m = 3,nealmon) +
                                 
                                 mls(pca_daily_spxturkey_1, 1:3, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_2, 1:3, m = 3*28,nealmon) + 
                                 mls(pca_daily_spxturkey_3, 1:3, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_4, 1:3, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_5, 1:3, m = 3*28,nealmon) ,
                                 
                                 # mls(pca_quarterly_spxturkey_1, 1:3, m = 1,nealmon) +
                                 # mls(pca_quarterly_spxturkey_2, 1:3, m = 1,nealmon),
                               
                               start = list(
                                 # gdp.ts.final = c(1,-0.5),
                                 pca_monthly_spxturkey_1 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_2 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_3 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_4 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_5 = c(1, -0.5, -0.2),
                                 
                                 pca_daily_spxturkey_1 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_2 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_3 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_4 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_5 = c(1, -0.5, -0.2)
                                 
                                 # pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                                 # pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                               )
                            
                               
)
  
  summary(almon_model2.1_pca)
  coef(almon_model2.1_pca, midas = TRUE)
  agk.test(almon_model2.1_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model2.1_pca$start_opt
  
  #forecast horizon
  s=47
  e=48
  
  almon_model2.1_pca_forecast <-forecast(almon_model2.1_pca, list(
    
    trend = trend[(s+1):e],
    
    gdp.ts.final = gdp.ts.final[(s+1):e],
    
    pca_monthly_spxturkey_1 = pca_monthly_spxturkey_1[(s*3+1):(e*3)],
    pca_monthly_spxturkey_2 = pca_monthly_spxturkey_2[(s*3+1):(e*3)],
    pca_monthly_spxturkey_3 = pca_monthly_spxturkey_3[(s*3+1):(e*3)],
    pca_monthly_spxturkey_4 = pca_monthly_spxturkey_4[(s*3+1):(e*3)],
    pca_monthly_spxturkey_5 = pca_monthly_spxturkey_5[(s*3+1):(e*3)],
    
    pca_daily_spxturkey_1 = pca_daily_spxturkey_1[(s*84+1):(e*84)],
    pca_daily_spxturkey_2 = pca_daily_spxturkey_2[(s*84+1):(e*84)],
    pca_daily_spxturkey_3 = pca_daily_spxturkey_3[(s*84+1):(e*84)],
    pca_daily_spxturkey_4 = pca_daily_spxturkey_4[(s*84+1):(e*84)],
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)]
    
    # pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    # pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model2.1_pca_forecast$mean %>% length()
  gdp_forecast[2]=almon_model2.1_pca_forecast$mean
  summary(almon_model2.1_pca_forecast)
  # plot(x=1:44,y=y[1:44], type = 'b', xlim = c(0,50), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  lines(x=(s+2):(e+1), y=almon_model2.1_pca_forecast$mean, type = 'b', col = 'mediumpurple', lwd =2, pch=5)
  legend(40, 26, c('Almon-1step Ahead'), col = c('mediumpurple'), lty = 8, pch = 5, pt.lwd = 2, cex = 0.9, bty = 'n')
  
  ## Residuals ##
  # residuals2 <- y[(s+2):(e+1)] - almon_model2.1_pca_forecast$mean
  # residuals2 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model 1 Step Ahead Errors')
  # abline(h = 0, col='red')
  # text(35, 6, sprintf("MAPE: %3.2f ", mean(abs(residuals2/y[(s+1):e]))*100) )
  # text(35, 8, sprintf("MSE: %3.2f ", mean(residuals2^2)) )
  # mean(residuals2^2)
  ##
} 

############ 2 step ahead forecast #############

########## Almon Models PCA  ###########
# trend = trend[1:48]
{
  almon_model2.2_pca  = midas_r(y ~ 
                                 trend +
                                 
                                 mls(gdp.ts.final ,k = 2:4, m = 1) + 
                                 
                                 mls(pca_monthly_spxturkey_1, 2:4, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_2, 2:4, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_3, 2:4, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_4, 2:4, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_5, 2:4, m = 3,nealmon) +
                                 
                                 mls(pca_daily_spxturkey_1, 2:4, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_2, 2:4, m = 3*28,nealmon) + 
                                 mls(pca_daily_spxturkey_3, 2:4, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_4, 2:4, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_5, 2:4, m = 3*28,nealmon) ,
                                 
                                 # mls(pca_quarterly_spxturkey_1, 2:4, m = 1,nealmon) +
                                 # mls(pca_quarterly_spxturkey_2, 2:4, m = 1,nealmon),
                               
                               start = list(
                                 # gdp.ts.final = c(1,-0.5),
                                 pca_monthly_spxturkey_1 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_2 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_3 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_4 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_5 = c(1, -0.5, -0.2),
                                 
                                 pca_daily_spxturkey_1 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_2 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_3 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_4 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_5 = c(1, -0.5, -0.2)
                                 
                                 # pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                                 # pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                               )
                               
                               
)
  
  summary(almon_model2.2_pca)
  coef(almon_model2.2_pca, midas = TRUE)
  agk.test(almon_model2.2_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model2.2_pca$start_opt
  
  #forecast horizon
  s=47
  e=48
  
  almon_model2.2_pca_forecast <-forecast(almon_model2.2_pca, list(
    
    trend = trend[(s+1):e],
    
    gdp.ts.final = gdp.ts.final[(s+1):e],
    
    pca_monthly_spxturkey_1 = pca_monthly_spxturkey_1[(s*3+1):(e*3)],
    pca_monthly_spxturkey_2 = pca_monthly_spxturkey_2[(s*3+1):(e*3)],
    pca_monthly_spxturkey_3 = pca_monthly_spxturkey_3[(s*3+1):(e*3)],
    pca_monthly_spxturkey_4 = pca_monthly_spxturkey_4[(s*3+1):(e*3)],
    pca_monthly_spxturkey_5 = pca_monthly_spxturkey_5[(s*3+1):(e*3)],
    
    pca_daily_spxturkey_1 = pca_daily_spxturkey_1[(s*84+1):(e*84)],
    pca_daily_spxturkey_2 = pca_daily_spxturkey_2[(s*84+1):(e*84)],
    pca_daily_spxturkey_3 = pca_daily_spxturkey_3[(s*84+1):(e*84)],
    pca_daily_spxturkey_4 = pca_daily_spxturkey_4[(s*84+1):(e*84)],
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)]
    
    # pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    # pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model2.2_pca_forecast$mean
  gdp_forecast[3]=almon_model2.2_pca_forecast$mean
  summary(almon_model2.2_pca_forecast)
  # plot(x=1:44,y=y[1:44], type = 'b', xlim = c(0,50), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  lines(x=(s+3):(e+2), y=almon_model2.2_pca_forecast$mean, type = 'b', col = 'lightgoldenrod3', lwd =2, pch=6)
  legend(40, 24, c('Almon-2step Ahead'), col = c('lightgoldenrod3'), lty = 8, pch = 6, pt.lwd = 2, cex = 0.9, bty = 'n')
  
  ## Residuals ##
  # residuals3 <- y[(s+3):(e+2)] - almon_model2.2_pca_forecast$mean
  # residuals3 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model 2 Step Ahead Errors')
  # abline(h = 0, col='red')
  # text(32, 3, sprintf("MAPE: %3.2f ", mean(abs(residuals3/y[(s+1):e]))*100) )
  # text(32, 5, sprintf("MSE: %3.2f ", mean(residuals3^2)) )
  
  # mean(residuals3^2)
  ##
} 

############ 3 step ahead forecast #############
########## Almon Models PCA  ###########
# trend = trend[1:48]
{
  almon_model2.3_pca  = midas_r(y ~ 
                                 trend +
                                 
                                 mls(gdp.ts.final ,k = 3:5, m = 1) + 
                                 
                                 mls(pca_monthly_spxturkey_1, 3:5, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_2, 3:5, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_3, 3:5, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_4, 3:5, m = 3,nealmon) +
                                 mls(pca_monthly_spxturkey_5, 3:5, m = 3,nealmon) +
                                 
                                 mls(pca_daily_spxturkey_1, 3:5, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_2, 3:5, m = 3*28,nealmon) + 
                                 mls(pca_daily_spxturkey_3, 3:5, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_4, 3:5, m = 3*28,nealmon) +
                                 mls(pca_daily_spxturkey_5, 3:5, m = 3*28,nealmon) ,
                                 
                                 # mls(pca_quarterly_spxturkey_1, 3:5, m = 1,nealmon) +
                                 # mls(pca_quarterly_spxturkey_2, 3:5, m = 1,nealmon),
                               
                               start = list(
                                 # gdp.ts.final = c(1,-0.5),
                                 pca_monthly_spxturkey_1 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_2 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_3 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_4 = c(1, -0.5, -0.2),
                                 pca_monthly_spxturkey_5 = c(1, -0.5, -0.2),
                                 
                                 pca_daily_spxturkey_1 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_2 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_3 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_4 = c(1, -0.5, -0.2),
                                 pca_daily_spxturkey_5 = c(1, -0.5, -0.2)
                                 
                                 # pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                                 # pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                               )
                               
                               
)
  
  summary(almon_model2.3_pca)
  coef(almon_model2.3_pca, midas = TRUE)
  agk.test(almon_model2.3_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model2.3_pca$start_opt
  
  #forecast horizon
  s=47
  e=48
  
  almon_model2.3_pca_forecast <-forecast(almon_model2.3_pca, list(
    
    trend = trend[(s+1):e],
    
    gdp.ts.final = gdp.ts.final[(s+1):e],
    
    pca_monthly_spxturkey_1 = pca_monthly_spxturkey_1[(s*3+1):(e*3)],
    pca_monthly_spxturkey_2 = pca_monthly_spxturkey_2[(s*3+1):(e*3)],
    pca_monthly_spxturkey_3 = pca_monthly_spxturkey_3[(s*3+1):(e*3)],
    pca_monthly_spxturkey_4 = pca_monthly_spxturkey_4[(s*3+1):(e*3)],
    pca_monthly_spxturkey_5 = pca_monthly_spxturkey_5[(s*3+1):(e*3)],
    
    pca_daily_spxturkey_1 = pca_daily_spxturkey_1[(s*84+1):(e*84)],
    pca_daily_spxturkey_2 = pca_daily_spxturkey_2[(s*84+1):(e*84)],
    pca_daily_spxturkey_3 = pca_daily_spxturkey_3[(s*84+1):(e*84)],
    pca_daily_spxturkey_4 = pca_daily_spxturkey_4[(s*84+1):(e*84)],
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)]
    
    # pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    # pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model2.3_pca_forecast$mean
  gdp_forecast[4]=almon_model2.3_pca_forecast$mean
  summary(almon_model2.3_pca_forecast)
  # plot(x=1:44,y=y[1:44], type = 'b', xlim = c(0,50), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  lines(x=(s+4):(e+3), y=almon_model2.3_pca_forecast$mean, type = 'b', col = 'hotpink', lwd =2, pch=7)
  legend(40, 22, c('Almon-3step Ahead'), col = c('hotpink'), lty = 8, pch = 7, pt.lwd = 2, cex = 0.9, bty = 'n')
  
  ## Residuals ##
  # residuals4 <- y[(s+4):(e+3)] - almon_model2.3_pca_forecast$mean
  # residuals4 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model 3 Step Ahead Errors')
  # abline(h = 0, col='red')
  # text(30, 5, sprintf("MAPE: %3.2f ", mean(abs(residuals4/y[(s+1):e]))*100),adj = c(0,0) )
  # 
  # text(30, 7, sprintf("MSE: %3.2f ", mean(residuals4^2)) ,
  #      adj = c(0,0))
  # mean(residuals4^2)
  ##
} 

############ 4 step ahead forecast #############
########## Almon Models PCA  ###########
# trend = trend[1:48]
{
  almon_model2.4_pca  = midas_r(y ~ 
                                  trend +
                                  
                                  mls(gdp.ts.final ,k = 4:6, m = 1) + 
                                  
                                  mls(pca_monthly_spxturkey_1, 4:6, m = 3,nealmon) +
                                  mls(pca_monthly_spxturkey_2, 4:6, m = 3,nealmon) +
                                  mls(pca_monthly_spxturkey_3, 4:6, m = 3,nealmon) +
                                  mls(pca_monthly_spxturkey_4, 4:6, m = 3,nealmon) +
                                  mls(pca_monthly_spxturkey_5, 4:6, m = 3,nealmon) +
                                  
                                  mls(pca_daily_spxturkey_1, 4:6, m = 3*28,nealmon) +
                                  mls(pca_daily_spxturkey_2, 4:6, m = 3*28,nealmon) + 
                                  mls(pca_daily_spxturkey_3, 4:6, m = 3*28,nealmon) +
                                  mls(pca_daily_spxturkey_4, 4:6, m = 3*28,nealmon) +
                                  mls(pca_daily_spxturkey_5, 4:6, m = 3*28,nealmon) ,
                                
                                # mls(pca_quarterly_spxturkey_1, 3:5, m = 1,nealmon) +
                                # mls(pca_quarterly_spxturkey_2, 3:5, m = 1,nealmon),
                                
                                start = list(
                                  # gdp.ts.final = c(1,-0.5),
                                  pca_monthly_spxturkey_1 = c(1, -0.5, -0.2),
                                  pca_monthly_spxturkey_2 = c(1, -0.5, -0.2),
                                  pca_monthly_spxturkey_3 = c(1, -0.5, -0.2),
                                  pca_monthly_spxturkey_4 = c(1, -0.5, -0.2),
                                  pca_monthly_spxturkey_5 = c(1, -0.5, -0.2),
                                  
                                  pca_daily_spxturkey_1 = c(1, -0.5, -0.2),
                                  pca_daily_spxturkey_2 = c(1, -0.5, -0.2),
                                  pca_daily_spxturkey_3 = c(1, -0.5, -0.2),
                                  pca_daily_spxturkey_4 = c(1, -0.5, -0.2),
                                  pca_daily_spxturkey_5 = c(1, -0.5, -0.2)
                                  
                                  # pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                                  # pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                                )
                                
                                
  )
  
  summary(almon_model2.4_pca)
  coef(almon_model2.4_pca, midas = TRUE)
  agk.test(almon_model2.4_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model2.4_pca$start_opt
  
  #forecast horizon
  s=20
  e=41
  
  almon_model2.4_pca_forecast <-forecast(almon_model2.4_pca, list(
    
    trend = trend[(s+1):e],
    
    gdp.ts.final = gdp.ts.final[(s+1):e],
    
    pca_monthly_spxturkey_1 = pca_monthly_spxturkey_1[(s*3+1):(e*3)],
    pca_monthly_spxturkey_2 = pca_monthly_spxturkey_2[(s*3+1):(e*3)],
    pca_monthly_spxturkey_3 = pca_monthly_spxturkey_3[(s*3+1):(e*3)],
    pca_monthly_spxturkey_4 = pca_monthly_spxturkey_4[(s*3+1):(e*3)],
    pca_monthly_spxturkey_5 = pca_monthly_spxturkey_5[(s*3+1):(e*3)],
    
    pca_daily_spxturkey_1 = pca_daily_spxturkey_1[(s*84+1):(e*84)],
    pca_daily_spxturkey_2 = pca_daily_spxturkey_2[(s*84+1):(e*84)],
    pca_daily_spxturkey_3 = pca_daily_spxturkey_3[(s*84+1):(e*84)],
    pca_daily_spxturkey_4 = pca_daily_spxturkey_4[(s*84+1):(e*84)],
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)]
    
    # pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    # pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model2.4_pca_forecast$mean
  # gdp_forecast[5]=almon_model2.4_pca_forecast$mean
  summary(almon_model2.4_pca_forecast)
  # plot(x=1:44,y=y[1:44], type = 'b', xlim = c(0,50), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  lines(x=(s+5):(e+4), y=almon_model2.4_pca_forecast$mean, type = 'b', col = 'yellowgreen', lwd =2, pch=8)
  legend(40, 20, c('Almon-4step Ahead'), col = c('yellowgreen'), lty = 8, pch = 8, pt.lwd = 2, cex = 0.9, bty = 'n')
  
  ## Residuals ##
  # residuals4 <- y[(s+4):(e+3)] - almon_model2.4_pca_forecast$mean
  # residuals4 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model 4 Step Ahead Errors')
  # abline(h = 0, col='red')
  # text(30, 5, sprintf("MAPE: %3.2f ", mean(abs(residuals4/y[(s+1):e]))*100),adj = c(0,0) )
  # 
  # text(30, 7, sprintf("MSE: %3.2f ", mean(residuals4^2)) ,
  #      adj = c(0,0))
  # mean(residuals4^2)
  ##
} 

lines(x=(s+1):(s+length(gdp_forecast)), y=gdp_forecast, type = 'l', col = 'red', lwd =1, pch=1)
gdp_forecast
