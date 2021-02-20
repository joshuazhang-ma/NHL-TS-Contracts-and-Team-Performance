library(forecast)
library(fpp2)
library(tsoutliers)
library(nonlinearTseries)
library(TSclust)
library(TSEntropies)
library(readxl)

#### Just change the import file name and the sheet reference to run different teams
game_data <- read_excel("C:/Users/Joshua Zhang/Downloads/MA611/Josh and Ray/Boston Bruins History.xlsx", sheet = "Maple Leafs G")

# bruin = Bruin
ts.gf <- ts(game_data$GF, start = c(2005,1), frequency = 82)
# tail(bruingoal)

# goalbruin = bruingoal[,9]
# goalbruin

# ts.goalbruin = ts(goalbruin,start = 2005, frequency = 82)
# ts.goalbruin1 = ts(goalbruin,start = 2005, frequency = 1)
# autoplot(ts.goalbruin)

autoplot(ts.gf)
SampEn(ts.gf)
# Entropy : 1.731681
ndiffs(ts.gf)
# ndiffs = 1
nsdiffs(ts.gf)
# nsdfiss = 0

#Check ACF/PACF
ggAcf(ts.gf, lag.max = 82)
ggPacf(ts.gf, lag.max = 82)
# Toronto: White noise achieved!
# Boston: lag = 4

#Check outliers
tsoutliers(ts.gf, lambda = "auto")
## No outliers detected

#Check non-linearity
nonlinearityTest(ts.gf)
## Independent residuals since 5/6 tests have p-values > 0.05.

#Create a heatmap and find potential patterns
Time.Stamp <- seq(1, length(ts.gf), 1)
ggplot(ts.gf, aes(x = Time.Stamp, y = 1)) +
  geom_tile(aes(fill = game_data$GF)) +
  scale_fill_gradient2(low = "navy", mid = "yellow", high = "red")
# Time.Stamp = seq(1,nrow(bruingoal),1)
# ggplot(ts.goalbruin,aes(x = Time.Stamp, y = 1))+
#   geom_tile(aes(fill = goalbruin))+
#   scale_fill_gradient2(low = "navy", mid = "yellow",
#                        high = "red") 

#Create a season plot
ggseasonplot(ts.gf, continuous = TRUE, polar = TRUE)
# ggseasonplot(ts.goalbruin, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
#   ggtitle("Performance pattern for 82 games in a year")

#seperate the training set
ts.gf.train <- window(ts.gf, end = end(ts.gf)-c(0,82))
ts.gf.train
# train.bruingoal=window(ts.goalbruin,end=end(ts.goalbruin)-c(0,2))
# train.bruingoal

# train.bruingoal1=window(ts.goalbruin1,end=end(ts.goalbruin1)-c(0,2))
# train.bruingoal1
#STL decomposition
stl <- decompose(ts.gf.train)
plot(stl)
# decompose(train.bruingoal)
# plot(decompose(train.bruingoal))

#Naive model
naive <- diff(ts.gf.train)
naive
accuracy(forecast(naive), ts.gf)
# naive_model = (diff(train.bruingoal))
# accuracy(forecast(naive_model),ts.goalbruin)

#ETS model
ets <- ets(ts.gf.train)
ets
accuracy(forecast(ets), ts.gf)
# ets_model = ets(train.bruingoal)
# ets_model
# accuracy(forecast(ets_model),bruingoaldata)
# stlf(train.bruingoal)

#ARIMA Model
#arima <- auto.arima(ts.gf.train, max.p = 2, max.q = 2, max.d = 2, max.P = 2, max.Q = 2, max.D = 2, stepwise = T, trace = TRUE, seasonal = TRUE)
#arima <- auto.arima(ts.gf.train, stepwise = F, trace = TRUE, seasonal = TRUE)
arima <- Arima(ts.gf.train, order = c(0,0,0), seasonal = c(2,0,1))
arima
accuracy(forecast(arima), ts.gf)
# arima_model = auto.arima(train.bruingoal, stepwise = F, trace = TRUE,seasonal= TRUE)
# arima_model
# accuracy(forecast(arima_model),bruingoaldata)

#Neural Model
neural <- nnetar(ts.gf.train)
neural
accuracy(forecast(neural), ts.gf)
forecast(neural)
# neural_model = nnetar(train.bruingoal)
# neural_model
# accuracy(forecast(neural_model),bruingoaldata)

#bagging Model
bagging <- baggedModel(ts.gf.train, bootstrapped_series = bld.mbb.bootstrap(ts.gf.train, 15))
# bagging_model = baggedModel(train.bruingoal1,bootstrapped_series = bld.mbb.bootstrap(train.bruingoal1, 20))
plot(forecast(bagging))

bootstrapped_graph= bld.mbb.bootstrap(ts.gf.train, 15)
boot.ts = ts(as.data.frame(bootstrapped_graph),start = c(2005),frequency = 82)

autoplot(ts.gf.train) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(ts.gf.train, colour=FALSE)+  ylab("Bootstrapped_versions seasonality of 82")+guides(colour="none")+
  ggtitle("15 bootstrapped versions of the Bruin Score")

num <- 20
forecast_boot.ts = forecast(boot.ts, h = num)
forecast_boot.ts
forecast_boot.ts$forecast[1] # forecast for run #1
forecast_boot.ts$forecast[2] # forecast for run #2, etc.

# Getting the bagged average for high 95%
high <- 0
for (i in 1:num){
  high <- high + as.numeric(forecast_boot.ts$forecast[i][[1]]$upper[,2][1])
}
high/num

# Getting the bagged average for low 95%
low <- 0
for (i in 1:num){
  low <- low + as.numeric(forecast_boot.ts$forecast[i][[1]]$lower[,2][1])
}
low/num

# Boston : Average of upper and lower = 3.356535

#Accuracy Comparison
accuracy(forecast(naive),ts.gf)
accuracy(forecast(ets),ts.gf)
accuracy(forecast(arima),ts.gf)
accuracy(forecast(neural),ts.gf)
accuracy(forecast(bagging),ts.gf)

# forecast(neural)
# forecast(arima)










#Forecast comparison
autoplot(ts.gf) + autolayer(fitted(naive))+ autolayer(forecast(naive))

autoplot(train.bruin) + autolayer(fitted(arima_model))+  autolayer(forecast(arima_model))+
  autolayer(fitted(ets_model))+  autolayer(forecast(ets_model))+
  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

autoplot(bruingoaldata) + autolayer(fitted(bagging_model))+  autolayer(forecast(bagging_model))+
  autolayer(fitted(arima_model))+  autolayer(forecast(arima_model))+
  autolayer(fitted(ets_model))+  autolayer(forecast(ets_model))+
  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

autoplot(bruingoaldata) + autolayer(fitted(bagging_model)) +  autolayer(forecast_boot.ts)

autoplot(bruingoaldata) + autolayer(fitted(bagging_model)) +  autolayer(forecast(bagging_model))+
  autolayer(fitted(arima_model))+  autolayer(forecast(arima_model))+
  autolayer(fitted(ets_model))+  autolayer(forecast(ets_model))+
  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

autoplot(bruingoaldata) +  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

#Forecast comparison transformed
autoplot(train.bruin) + autolayer(fitted(arima_model_t))+  autolayer(forecast(arima_model_t))+
  autolayer(fitted(ets_model_t))+  autolayer(forecast(ets_model_t))+
  autolayer(fitted(neural_model_t))+  autolayer(forecast(neural_model_t))

autoplot(train.bruin) +autolayer(fitted(neural_model_t))+  autolayer(forecast(neural_model_t))

autoplot(train.bruin) + autolayer(fitted(arima_model_t))+  autolayer(forecast(arima_model_t))

autoplot(train.bruin) + autolayer(fitted(ets_model_t))+  autolayer(forecast(ets_model_t))

