#시계열 예측모형


alpha <- 1:100
C <- alpha * (1 - alpha)
plot(C)


# 60년간 연평균 기온 

dev.off()
nhtemp
plot(nhtemp)
ts.plot(nhtemp)

library(forecast)
fit <- ets(nhtemp, model = "ZNN")
fit

# AIC, BIC 값이 작을수록 조은 모델임
names(fit)
#fitted : 모형을 통해 나온 추정값
#예측값 : 앞으로 이 모형으로 미래를 예측할 값.
fit$fitted
fit$residuals
fit$aic
fit$series
fit$components

nhtemp
fit2<-forecast(fit, h = 1 )
plot(fit2)
#  Lo 80    Hi 80    : 80% 신뢰구간 안에 모 연평균기온이 있다
fit3<-forecast(fit, h = 3 )
fit3
plot(fit3)


accuracy(fit)
summary(fit)
#시계열은 검정통계량 내지 않는다
#RMSE - AIC를 동시에 볼 수 있음. 
# 5 시계열 예측모형의 정확도 / 신뢰도


AirPassengers
attributes(AirPassengers)

#tsp 속성들
tsp(AirPassengers) 
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers) #frequency:12가 실제 data 적용되는걸 보여줌

plot(AirPassengers)

fit <- ets(log(AirPassengers), model = "AAA")
(pred <- forecast(fit, h = 12))
plot(pred)

#지

JohnsonJohnson
tsp(JohnsonJohnson)
frequency(JohnsonJohnson)
cycle(JohnsonJohnson)
plot(JohnsonJohnson)

(fit <-ets(JohnsonJohnson))
plot(forecast(fit))
                                
#ARIMA 
Nile
lagNile <- lag(Nile, k = 1)

tsp(lagNile)
Acf(Nile)
#lag (시차)에 따른 자기상관성, 자기상관성이 없어야만ARIMA 모델 가능
#파란선 안쪽 : 자기상관성이 적거나 없다.

Pacf(Nile)

plot(AirPassengers)
ndiffs(AirPassengers)
dAir <- diff(AirPassengers, lag = 1, differences = 1)
plot(dAir)

tsdisplay(dAir)
