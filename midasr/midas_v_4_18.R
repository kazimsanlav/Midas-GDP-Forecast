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
spx_Turkey_monthly <- spx_Turkey_monthly[2:29]

daily_spxturkey = lapply(spx_Turkey_daily, FUN = format_daily)

monthly_spxturkey = lapply(spx_Turkey_monthly, FUN = format_monthly)

quarterly_spxturkey = lapply(spx_Turkey_quarterly, FUN = format_quarterly)


# monthly_spxturkey[[1]] %>% plot
# quarterly_spxturkey$`BDSRTRP Index` %>% plot
# pca_daily = getPCA(daily_series)
# 
pca_daily_spxturkey = getPCA(daily_spxturkey, showgraph = TRUE, number0fPC = 5)

pca_monthly_spxturkey = getPCA(monthly_spxturkey, showgraph = TRUE, number0fPC = 5)

pca_quarterly_spxturkey = getPCA(quarterly_spxturkey, showgraph = TRUE, number0fPC = 2)


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

pca_quarterly_spxturkey_1 <- pca_quarterly_spxturkey[,1]
pca_quarterly_spxturkey_2 <- pca_quarterly_spxturkey[,2]

############ Nowcast #############
########## Nowcasting Almon Models PCA  ###########
trend = trend[1:44]
{almon_model1_pca  = midas_r(y ~ 
                               trend +
                               
                               mls(gdp.ts.final ,k = 1:2, m = 1) + 
                               
                               fmls(pca_monthly_spxturkey_1, 2, m = 3,nealmon) +
                               fmls(pca_monthly_spxturkey_2, 2, m = 3,nealmon) +
                               fmls(pca_monthly_spxturkey_3, 2, m = 3,nealmon) +
                               fmls(pca_monthly_spxturkey_4, 2, m = 3,nealmon) +
                               fmls(pca_monthly_spxturkey_5, 2, m = 3,nealmon) +
                               
                               fmls(pca_daily_spxturkey_1, 2, m = 3*28,nealmon) +
                               fmls(pca_daily_spxturkey_2, 2, m = 3*28,nealmon) + 
                               fmls(pca_daily_spxturkey_3, 2, m = 3*28,nealmon) +
                               fmls(pca_daily_spxturkey_4, 2, m = 3*28,nealmon) +
                               fmls(pca_daily_spxturkey_5, 2, m = 3*28,nealmon) +
                               
                               fmls(pca_quarterly_spxturkey_1, 2, m = 1,nealmon) +
                               fmls(pca_quarterly_spxturkey_2, 2, m = 1,nealmon), 
                             
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
                               pca_daily_spxturkey_5 = c(1, -0.5, -0.2),
                               
                               pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                               pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                             )
                             
                             
)
  
  summary(almon_model1_pca)
  coef(almon_model1_pca, midas = TRUE)
  agk.test(almon_model1_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model1_pca$start_opt
  
  #forecast horizon
  s=20
  e=44
  
  almon_model1_pca_forecast <-forecast(almon_model1_pca, list(
    
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
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)],
    
    pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model1_pca_forecast$fitted 
  summary(almon_model1_pca_forecast)
  plot(x=1:44,y=y[1:44], type = 'b', xlim = c(0,50), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  lines(x=(s+1):e, y=almon_model1_pca_forecast$mean, type = 'b', col = 'orange', lwd =2, pch=4)
  legend(40, 28, c('Almon-Nowcast'), col = c('orange'), lty = 8, pch = 4, pt.lwd = 2, cex = 0.9, bty = 'n')
  
  almon_model1_pca_forecast$mean
  
  # ## Residuals ##
  residuals1 <- y[(s+1):e] - almon_model1_pca_forecast$mean
  # residuals1 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model NowCasting Errors')
  # abline(h = 0, col='red')
  text(38, 14.5, sprintf("MSE: %3.2f ", mean(residuals1^2)) )
  # mean(residuals1^2)
  ##
  
}

############ Forecast #############
########## Forecast Almon Models PCA 1Step ###########
trend = trend[1:44]
{almon_model2_pca  = midas_r(y ~ 
                               trend +
                               
                               mls(gdp.ts.final ,k = 2:3, m = 1) + 
                               
                               mls(pca_monthly_spxturkey_1, k = 1:3, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_2, k = 1:3, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_3, k = 1:3, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_4, k = 1:3, m = 3,nealmon) +
                               mls(pca_monthly_spxturkey_5, k = 1:3, m = 3,nealmon) +
                               
                               mls(pca_daily_spxturkey_1, k = 1:3, m = 3*28,nealmon) +
                               mls(pca_daily_spxturkey_2, k = 1:3, m = 3*28,nealmon) + 
                               mls(pca_daily_spxturkey_3, k = 1:3, m = 3*28,nealmon) +
                               mls(pca_daily_spxturkey_4, k = 1:3, m = 3*28,nealmon) +
                               mls(pca_daily_spxturkey_5, k = 1:3, m = 3*28,nealmon) +
                               
                               mls(pca_quarterly_spxturkey_1, k = 1:3, m = 1,nealmon) +
                               mls(pca_quarterly_spxturkey_2, k = 1:3, m = 1,nealmon), 
                             
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
                               pca_daily_spxturkey_5 = c(1, -0.5, -0.2),
                               
                               pca_quarterly_spxturkey_1 = c(1, -0.5, -0.2),
                               pca_quarterly_spxturkey_2 = c(1, -0.5, -0.2)
                             )
                             
                             
)
  
  summary(almon_model2_pca)
  coef(almon_model2_pca, midas = TRUE)
  agk.test(almon_model2_pca)
  
  # plot(fmls(consumer_confidence.ts, 2, m = 3)[,1], type = 'b')
  # lines(nealmon(p = c( 1,-0.5, -0.2), d = 40), col = 'red')
  
  almon_model2_pca$start_opt
  
  #forecast horizon
  s=20
  e=44
  
  almon_model2_pca_forecast <-forecast(almon_model2_pca, list(
    
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
    pca_daily_spxturkey_5 = pca_daily_spxturkey_5[(s*84+1):(e*84)],
    
    pca_quarterly_spxturkey_1 = pca_quarterly_spxturkey_1[(s+1):e],
    pca_quarterly_spxturkey_2 = pca_quarterly_spxturkey_2[(s+1):e]
    
  ),
  method = 'static')
  
  
  almon_model2_pca_forecast$fitted 
  summary(almon_model1_pca_forecast)
  plot(x=1:44,y=y[1:44], type = 'b', xlim = c(0,50), ylim = c(-30,30), lwd =2.5 , ylab = 'GDP', xlab = 'Quarter')
  lines(x=(s+1):e, y=almon_model1_pca_forecast$mean, type = 'b', col = 'orange', lwd =2, pch=4)
  legend(40, 28, c('Almon-Nowcast'), col = c('orange'), lty = 8, pch = 4, pt.lwd = 2, cex = 0.9, bty = 'n')
  
  almon_model1_pca_forecast$mean
  
  # ## Residuals ##
  residuals1 <- y[(s+1):e] - almon_model1_pca_forecast$mean
  # residuals1 %>% plot(type='b',x = (s+1):e, xlab = 'Estimated Quarters', ylab = 'Residuals',main = 'Almon Model NowCasting Errors')
  # abline(h = 0, col='red')
  text(38, 14.5, sprintf("MSE: %3.2f ", mean(residuals1^2)) )
  # mean(residuals1^2)
  ##
  
}

