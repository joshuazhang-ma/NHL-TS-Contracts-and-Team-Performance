bruin = Bruin
bruin

value = bruin[,3]
value

tsbruin = ts(value,start = 2005, frequency = 4)
autoplot(tsbruin)
entropy(tsbruin)
ndiffs(tsbruin)
nsdiffs(tsbruin)


BoxCox.lambda(tsbruin)
lambda(bruin)
#Check outliers
tsoutliers(tsbruin, lambda = "auto")

#Check non-linearity
nonlinearityTest(tsbruin)

#Create a heatmap and find potential patterns
Time.Stamp = seq(1,nrow(bruin),1)
ggplot(bruin,aes(x = Time.Stamp, y = 1))+
  geom_tile(aes(fill = value))+
  scale_fill_gradient2(low = "navy", mid = "yellow",
                       high = "red") 

#seperate the training set
train.bruin=window(tsbruin,end=end(tsbruin)-c(0,3))
train.bruin

#STL decomposition
decompose(train.bruin)
plot(decompose(train.bruin))

#Naive model
naive_model = (diff(train.bruin))
accuracy(forecast(naive_model),tsbruin)

#ETS model
ets_model = ets(train.bruin)
ets_model
accuracy(forecast(ets_model),tsbruin)

#ETS model transformed
ets_model_t = ets(train.bruin, lambda = -1)
ets_model_t
accuracy(forecast(ets_model),tsbruin)

#ARIMA Model
arima_model = auto.arima(train.bruin, stepwise = F, trace = TRUE)
arima_model
accuracy(forecast(arima_model),tsbruin)

#ARIMA Model Transformed
arima_model_t = auto.arima(train.bruin, stepwise = F, trace = TRUE, lambda = -1)
arima_model_t
accuracy(forecast(arima_model_t),tsbruin)


#Neural Model
neural_model = nnetar(train.bruin)
neural_model
accuracy(forecast(neural_model),tsbruin)

#Neural Model transformed
neural_model_t = nnetar(train.bruin, lambda = -1)
neural_model_t
accuracy(forecast(neural_model_t),tsbruin)


#bagging Model
bagging_model = baggedModel(train.bruin,bootstrapped_series = bld.mbb.bootstrap(train.bruin, 50))
plot(bagging_model)

bootstrapped_graph= bld.mbb.bootstrap(train.bruin, 50)
boot.ts = ts(as.data.frame(bootstrapped_graph),start = c(2005,1),frequency = 4)

autoplot(train.bruin) +
  autolayer(boot.ts, colour=TRUE) +
  autolayer(train.bruin, colour=FALSE)+  ylab("Bootstrapped_versions")+guides(colour="none")+
  ggtitle("50 bootstrapped versions of the Bruin Score")

#bootstrapped_versions=bld.mbb.bootstrap(train.xuk.mod.ts,15)
#boot.ts=ts(as.data.frame(bootstrapped_versions),start = c(1970,1),frequency = 12)


#Accuracy Comparison
accuracy(forecast(naive_model),tsbruin)
accuracy(forecast(ets_model),tsbruin)
accuracy(forecast(arima_model),tsbruin)
accuracy(forecast(neural_model),tsbruin)
accuracy(forecast(bagging_model),tsbruin)



#Accuracy Comparison Transformed
accuracy(forecast(naive_model),tsbruin)
accuracy(forecast(ets_model_t),tsbruin)
accuracy(forecast(arima_model_t),tsbruin)
accuracy(forecast(neural_model),tsbruin)
accuracy(forecast(bagging_model),tsbruin)

#Forecast comparison
autoplot(train.bruin) + autolayer(fitted(naive_model))+ autolayer(fitted(ets_model))+
  autolayer(fitted(arima_model))+ autolayer(fitted(neural_model)) + autolayer(forecast(naive_model))

autoplot(train.bruin) + autolayer(fitted(arima_model))+  autolayer(forecast(arima_model))+
  autolayer(fitted(ets_model))+  autolayer(forecast(ets_model))+
  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))
  
autoplot(tsbruin) + autolayer(fitted(bagging_model))+  autolayer(forecast(bagging_model))+
  autolayer(fitted(arima_model))+  autolayer(forecast(arima_model))+
  autolayer(fitted(ets_model))+  autolayer(forecast(ets_model))+
  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

autoplot(tsbruin) + autolayer(fitted(bagging_model))+  autolayer(forecast(bagging_model))
autoplot(tsbruin) + autolayer(fitted(bagging_model))+  autolayer(forecast(bagging_model))+
  autolayer(fitted(arima_model))+  autolayer(forecast(arima_model))+
  autolayer(fitted(ets_model))+  autolayer(forecast(ets_model))+
  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

autoplot(tsbruin) +  autolayer(fitted(neural_model))+  autolayer(forecast(neural_model))

#Forecast comparison transformed
autoplot(train.bruin) + autolayer(fitted(arima_model_t))+  autolayer(forecast(arima_model_t))+
  autolayer(fitted(ets_model_t))+  autolayer(forecast(ets_model_t))+
  autolayer(fitted(neural_model_t))+  autolayer(forecast(neural_model_t))

autoplot(train.bruin) +autolayer(fitted(neural_model_t))+  autolayer(forecast(neural_model_t))

autoplot(train.bruin) + autolayer(fitted(arima_model_t))+  autolayer(forecast(arima_model_t))

autoplot(train.bruin) + autolayer(fitted(ets_model_t))+  autolayer(forecast(ets_model_t))

