# Midas-GDP-Forecast

My senior project; Forecasting Turkey real GDP growth using mixed frequency data sampling regression model. Based on a recent [paper](https://www.jstatsoft.org/index.php/jss/article/view/v072i04 "Mixed Frequency Data Sampling Regression Models: The R Package midasr") on Journal of Statistical Software, I applied mixed data frequency regression to our problem using R. Also I made an connection between R and Bloomberg api to transfer economitric datas directly to my model in R. In terms of data selection(from bloomberg) and model creation(by created helper function) this model remains flexible and easy to interpret. I believe this work can be used for government and companies who need accurate gdp forecast. More importantly, concept of midas regression can be applied to all sort of problems where different frequency time-series data needed for the model. [Link to final presentation](../master/midasr/Presentation.pptx)

Not: You can use this study in your own work but please refer back to this study. 

## Final real GDP Growth Prediction
![alt text](https://github.com/kazimsanlav/Midas-GDP-Forecast/blob/master/midasr/plots/final_forecast.png)

## Nowcast Accuracy
![alt text](https://github.com/kazimsanlav/Midas-GDP-Forecast/blob/master/midasr/plots/nowcast.png)

## Nowcast Error Statistic
![alt text](https://github.com/kazimsanlav/Midas-GDP-Forecast/blob/master/midasr/plots/mape%20now.png)




