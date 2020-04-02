library(forecast)

#최초모형을 찾아내기위해
tsdisplay(Nile)
ts.plot(Nile)
plot(Nile)
#ACF -> P 결정 : 0으로 수렴
#PACF -> Q결정 : 1에서 뚝 떨어지므로 1 ??

(d<-ndiffs(Nile))
#차분을 몇번해야 추세성분을 제거할 수 있는지 알아냄



(fit <- arima(Nile, order = c(0,1,1)))


accuracy(fit)


dNile <- diff(Nile, lag = 1, differences = 1)
#차분함수의 lag는 원래 시계열 데이터의 주기에 따라 lag인자를 입력해야함.

tsp(Nile)
tsp(dNile) #차분하면서 밀린 시차의 간격이 '년' 단위임.
plot(dNile)

fit$aic # 여러개라면 aic가 작은게 낫다 
names(fit)

shapiro.test(fit$residuals)

Box.test(fit$residuals, type = 'Ljung-Box')
#H0 : no autocorrelation (변환 필요 없다)

par(mfrow=c(2,1))
Acf(fit$residuals)
Pacf(fit$residuals)


library(tseries)
fitted(fit)
adf.test(dNile) #residuals 가 아닌 diff 차분값을 넣어야함. 


pred<- forecast(fit, h = 10) 
plot(pred)


tsp(sunspots)
cycle(sunspots)
plot(sunspots)

fit<-auto.arima(sunspots)

(d <-ndiffs(sunspots))

dsunspots <-diff(sunspots)
plot(dsunspots)



#-------------------

dimnames(dd.ts) 

#단변량 시계열의 이름을 보여줌

#다변량 시계열 객체

df <- ts(sample(20), frequency = 4 , start= 1998)
df

library(ggplot2)
(mpg.ts <- ts(mpg[, c('hwy','cty')], frequency = 1 , start= c(2001,1)))


dimnames(mpg.ts)[[2]][1]#계열 이름 알려줌
class(mpg.ts)
frequency(mpg.ts)
cycle(mpg.ts)
start(mpg.ts)

df2<-window(x = df, start= c(2000,2),end=c(2002,2))

tsp(df2)




new.ts <- BoxCox(AirPassengers)
