
plot(AirPassengers)
plot(JohnsonJohnson)
plot(nhtemp)
plot(Nile)
plot(sunspots)

sales<-sample(1:99, 24, 1)
tsales <- ts(sales, start = c(2003, 1), frequency = 20)
tsales
tsales2 <-ts(sales, frequency = 365, start = c(2003, 1))
tsales2

tsales3 <- ts(1:8, frequency = 4, start = c(2010,1))
tsales3
nhtemp

tsales4<- ts(1:24, frequency = 8760)
tsales4

plot(tsales)
start(tsales)
end(tsales)
tsales.subset <- window(tsales, start= c(2003, 5), end= c(2004,4))
tsales.subset
