#

attributes(AirPassengers)

plot(AirPassengers)

logAir <- log(AirPassengers)
plot(logAir)


fit <-stl(logAir, s.window = 'periodic')
fit
names(fit)
fit$time.series
plot(fit)

install.packages('forecast')
library(forecast)

dev.off()

par(mfrow=c(2,1))


monthplot(AirPassengers) #해당 월의 추세와 평균
seasonplot(AirPassengers)
