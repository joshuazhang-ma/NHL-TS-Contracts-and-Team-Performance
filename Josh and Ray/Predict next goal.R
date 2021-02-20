bruin = Bruin
bruingoaldata
tail(bruingoal)

goalbruin = bruingoal[,9]
goalbruin

ts.goalbruin = ts(goalbruin,start = 2005, frequency = 82)
ts.goalbruin1 = ts(goalbruin,start = 2005, frequency = 1)
autoplot(ts.goalbruin)


autoplot(bruingoaldata)
entropy(bruingoaldata)
ndiffs(bruingoaldata)
nsdiffs(bruingoaldata)

#Check outliers
tsoutliers(bruingoaldata, lambda = "auto")

#Check non-linearity
nonlinearityTest(bruingoaldata)

#Create a heatmap and find potential patterns
Time.Stamp = seq(1,nrow(bruingoal),1)
ggplot(ts.goalbruin,aes(x = Time.Stamp, y = 1))+
  geom_tile(aes(fill = goalbruin))+
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red") 


#Create a season plot
ggseasonplot(ts.goalbruin, year.labels=FALSE, continuous=TRUE, polar = TRUE)+
  ggtitle("Performance pattern for 82 games in a year")

#seperate the training set
train.bruingoal=window(ts.goalbruin,end=end(ts.goalbruin)-c(0,2))
train.bruingoal

train.bruingoal1=window(ts.goalbruin1,end=end(ts.goalbruin1)-c(0,2))
train.bruingoal1
#STL decomposition
decompose(train.bruingoal)
plot(decompose(train.bruingoal))

#Naive model
naive_model = (diff(train.bruingoal))
accuracy(forecast(naive_model),ts.goalbruin)

#ETS model
ets_model = ets(train.bruingoal)
ets_model
accuracy(forecast(ets_model),bruingoaldata)
stlf(train.bruingoal)

#ARIMA Model

arima_model = auto.arima(train.bruingoal, stepwise = F, trace = TRUE,seasonal= TRUE)
arima_model
accuracy(forecast(arima_model),bruingoaldata)



#Neural Model
neural_model = nnetar(train.bruingoal)
neural_model
accuracy(forecast(neural_model),bruingoaldata)

#bagging Model
bagging_model = baggedModel(train.bruingoal1,bootstrapped_series = bld.mbb.bootstrap(train.bruingoal1, 20))
plot(forecast(bagging_model))

bootstrapped_graph= bld.mbb.bootstrap(train.bruingoal1, 20)
boot.ts = ts(as.data.frame(bootstrapped_graph),start = c(2005),frequency = 1)

autoplot(train.bruingoal1) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(train.bruingoal1, colour=FALSE)+  ylab("Bootstrapped_versions seasonality of 1")+guides(colour="none")+
  ggtitle("20 bootstrapped versions of the Bruin Score")


forecast_boot.ts = forecast(boot.ts, h = 8)
forecast_boot.ts
#Accuracy Comparison
accuracy(forecast(naive_model),ts.goalbruin)
accuracy(forecast(ets_model),ts.goalbruin1)#not working with seasonality greater than 24
accuracy(forecast(arima_model),ts.goalbruin)
accuracy(forecast(neural_model),ts.goalbruin)
accuracy(forecast(bagging_model),ts.goalbruin1)#not working with seasonality greater than 24

forecast(neural_model)
forecast(arima_model)
#Forecast comparison
autoplot(train.bruin) + autolayer(fitted(naive_model))+ autolayer(fitted(ets_model))+
  autolayer(fitted(arima_model))+ autolayer(fitted(neural_model)) + autolayer(forecast(naive_model))

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

