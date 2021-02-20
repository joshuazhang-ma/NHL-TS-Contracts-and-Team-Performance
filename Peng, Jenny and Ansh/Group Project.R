summary(Total)
library(ggplot2)
library(forecast)
library(fpp2)
library(gridExtra)
library(readxl)
library(TSEntropies)
library(nonlinearTseries)


Total <- read_excel("Desktop/TOI2.xlsx", sheet = "time series")
View(Total)   

###############################################################################################
############ get the time series of each player's monthly +/- and modify outliers #############
###############################################################################################
BOS_C = Total[,3]
BOS_D = Total[,4]
BOS_LW = Total[,5]
BOS_RW = Total[,6]
TOR_C = Total[,7]
TOR_D = Total[,8]
TOR_LW = Total[,9]
TOR_RW = Total[,10]



BOS_C.ts = tsclean(ts(na.omit(BOS_C), start = c(1,1), frequency = 7))
BOS_D.ts = tsclean(ts(na.omit(BOS_D), start = c(1,1), frequency = 7))
BOS_LW.ts = tsclean(ts(na.omit(BOS_LW), start = c(1,1), frequency = 7))
BOS_RW.ts = tsclean(ts(na.omit(BOS_RW), start = c(1,1), frequency = 7))
TOR_C.ts = tsclean(ts(na.omit(TOR_C), start = c(1,1), frequency = 7))
TOR_D.ts = tsclean(ts(na.omit(TOR_D), start = c(1,1), frequency = 7))
TOR_LW.ts = tsclean(ts(na.omit(TOR_LW), start = c(1,1), frequency =7))
TOR_RW.ts = tsclean(ts(na.omit(TOR_RW), start = c(1,1), frequency = 7))


BOS_C.polar = ggseasonplot(BOS_C.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct. [Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Boston Centerman - Patrice Bergeron")
BOS_D.polar = ggseasonplot(BOS_D.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct. [Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Boston Defenseman - Zdeno Chara")
BOS_LW.polar = ggseasonplot(BOS_LW.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct. [Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Boston Left Wing - Brad Marchand")
BOS_RW.polar = ggseasonplot(BOS_RW.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct. [Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Boston Right Wing - David Pastrnak")
TOR_C.polar = ggseasonplot(TOR_C.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct. [Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Toronto Centerman - John Tavares")
TOR_D.polar = ggseasonplot(TOR_D.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct. [Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Toronto Defenseman - Morgan Reilly")
TOR_LW.polar = ggseasonplot(TOR_LW.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct.[Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Toronto Left Wing - Andreas Johnsson")
TOR_RW.polar = ggseasonplot(TOR_RW.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE, season.labels = c('Oct.[Season Start]', 'Nov.', 'Dec.', 'Jan.', 'Feb.', 'Mar.', 'Apr.'))+
  ggtitle("Toronto Right Wing - Mitchell Marner")
grid.arrange(BOS_C.polar, BOS_D.polar, BOS_LW.polar, BOS_RW.polar,
             TOR_C.polar, TOR_D.polar, TOR_LW.polar, TOR_RW.polar, ncol = 4, nrow = 2)
###############################################################################################
####################### plot the time series of each player's +/- #############################
###############################################################################################

BOS_C.plot = autoplot(BOS_C.ts, facets=T)+geom_smooth()+ggtitle("Boston Centerman - Patrice Bergeron") + ylim(-2, 2)
BOS_D.plot = autoplot(BOS_D.ts, facets=T)+geom_smooth()+ggtitle("Boston Defenseman - Zdeno Chara") + ylim(-2, 2)
BOS_LW.plot = autoplot(BOS_LW.ts, facets=T)+geom_smooth()+ggtitle("Boston Left Wing - Brad Marchand") + ylim(-2, 2)
BOS_RW.plot = autoplot(BOS_RW.ts, facets=T)+geom_smooth()+ggtitle("Boston Right Wing - David Pastrnak") + ylim(-2, 2)
TOR_C.plot = autoplot(TOR_C.ts, facets=T)+geom_smooth()+ggtitle("Toronto Centerman - John Tavares") + ylim(-2, 2)
TOR_D.plot = autoplot(TOR_D.ts, facets=T)+geom_smooth()+ggtitle("Toronto Defenseman - Morgan Reilly") + ylim(-2, 2)
TOR_LW.plot = autoplot(TOR_LW.ts, facets=T)+geom_smooth()+ggtitle("Toronto Left Wing - Andreas Johnsson") + ylim(-2, 2)
TOR_RW.plot = autoplot(TOR_RW.ts, facets=T)+geom_smooth()+ggtitle("Toronto Right Wing - Mitchell Marner") + ylim(-2, 2)
grid.arrange(BOS_C.plot, BOS_D.plot, BOS_LW.plot, BOS_RW.plot,
             TOR_C.plot, TOR_D.plot, TOR_LW.plot, TOR_RW.plot, ncol = 4, nrow = 2)

###############################################################################################
############################ get the pacf plot of each player #################################
###############################################################################################
BOS_C.pacf = ggPacf(BOS_C.ts,lag.max=20)+ggtitle("Boston Centerman - Patrice Bergeron")
BOS_D.pacf = ggPacf(BOS_D.ts,lag.max=20)+ggtitle("Boston Defenseman - Zdeno Chara")
BOS_RW.pacf = ggPacf(BOS_RW.ts,lag.max=20)+ggtitle("Boston Left Wing - Brad Marchand")
BOS_LW.pacf = ggPacf(BOS_LW.ts,lag.max=20)+ggtitle("Boston Right Wing - David Pastrnak")
TOR_C.pacf = ggPacf(TOR_C.ts,lag.max=20)+ggtitle("Toronto Centerman - John Tavares") 
TOR_D.pacf = ggPacf(TOR_D.ts,lag.max=20)+ggtitle("Toronto Defenseman - Morgan Reilly")
TOR_LW.pacf = ggPacf(TOR_LW.ts,lag.max=20)+ggtitle("Toronto Left Wing - Andreas Johnsson") 
TOR_RW.pacf = ggPacf(TOR_RW.ts,lag.max=20)+ggtitle("Toronto Right Wing - Mitchell Marner") 
grid.arrange(BOS_C.pacf, BOS_D.pacf, BOS_LW.pacf, BOS_RW.pacf,
             TOR_C.pacf, TOR_D.pacf, TOR_LW.pacf, TOR_RW.pacf, ncol = 4, nrow = 2)

###############################################################################################
################################# build model to each player ###################################
###############################################################################################

onlinearityTest(BOS_C.ts)

SampEn(BOS_C.ts)

BOS_C.arima = auto.arima(BOS_C.ts,D=NA,max.q=0,max.P = 0,max.Q = 0,seasonal = F,stepwise = F,trace=T)
BOS_C.ets = ets(BOS_C.ts)
BOS_C.NN = nnetar(BOS_C.ts)

forecast(BOS_C.arima)
forecast(BOS_C.ets)
forecast(BOS_C.NN)












