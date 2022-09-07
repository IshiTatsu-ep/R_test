library("zoo")
library("xts")
require("caretForecast")
library(tidyverse)
library(TSstudio)
library(MLmetrics)
library(forecast)

data <- readRDS(output_data)
data$datetime <- as.Date(data$datetime)
data <- data %>% arrange(datetime)
data$percentage <- NULL
#xts型に変換
data_xts <- xts(data[,1],order.by = data[,2])
ts_plot(data_xts)
#ts型も用意
data_ts <- ts(data[,-2],start=1,end=3167)
ts_plot(data_ts)
#ts系の関数は、xtsオブジェクトに対しては使用できないらしい。
#7日置きにラグがありそう
ts_cor(data_ts,lag.max = 60)
ts_lags(data_ts,lags = 1:12)


#data_ts_s <- ts_split(ts.obj = data_xts,sample.out =365)
#訓練データ、テストデータの分割
data_ts_s <- ts_split(ts.obj = data_ts,sample.out =30)
train <- data_ts_s$train
test <- data_ts_s$test

md <- auto.arima(train,max.P = 30,max.Q = 30,max.D = 30,)
fc <- forecast(md, h=30)
help(forecast)
plot_forecast(fc)
test_forecast(actual = data_ts2,forecast.obj = fc,test=test)
MAPE(fc$mean,test)     #MAPE
RMSE(fc$mean,test)     #RMSE
R2_Score(fc$mean,test) #R2
help("R2_Score")

#複数モデルでの評価
methods <- list(arima=list(method = "auto.arima",notes = "ARIMA"),
                ets = list(method = "ets", notes = "ETS"),
                hw = list(method = "HoltWinters", notes = "Holt-Winters method"),
                tslm = list(method = "tslm", method_arg = list(formula = input 
                                                               ~trend + season),
                            notes = "Time Series Linear Model"))

md <- train_model(input = data_ts,methods = methods,train_method = 
                    list( partitions = 30,
                          space = 12,
                          sample.in = 3000,
                          sample.out = 167),
                  horizon = 167,
                  error = "MAPE"
                  )


#data <- data[,-2]
#data_ts <- data[,c(2,1)]
#rownames(data_ts) <- data_ts[,1]
#data_ts = ts(data_ts,start = c(2014,9,4),frequency = 365)
#data_ts = xts(data_ts)
#data_ts <- data_ts[,-1]

apply.yearly(data_ts,mean)
ts_heatmap(data_ts,wday = TRUE)
ts_cor(data_ts,lag.max = 60)
ts_lags(data_ts,lags = 1:12)

data_ts_s <- ts_split(ts.obj = data_ts,sample.out =365)
data_ts_s <- ts_split(ts.obj = data_ts2,sample.out =30)
train <- data_ts_s$train
test <- data_ts_s$test

md <- auto.arima(train,max.P = 30,max.Q = 30,max.D = 30,)
fc <- forecast(md, h=30)
help(forecast)
plot_forecast(fc)
test_forecast(actual = data_ts2,forecast.obj = fc,test=test)



md$coef
help(auto.arima)
ts_plot(data_ts)
ts_decompose(data_ts)
ts_seasonal(data_ts,type="box")
ts_seasonal(data_ts,type="cycle")
train_data <- window(data_ts, end =(2018-12-31))

