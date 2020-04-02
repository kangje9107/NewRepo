rawdata <-  analysis_total_Fixed
View(rawdata)

library(dplyr)


#분할 군집화 수행한 충북 - 강원 데이터의 시계열 회귀분석 실시
#분해 시계열 분석 

analysis_data <- rawdata %>% filter(시도=='충북' | 시도 == '강원') 
str(analysis_data)
View(analysis_data)

# analysis_data <- rawdata %>% filter(시도=='충북' | 시도 == '강원') %>% group_by(일시) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
#                                                                                                                    `최저기온(°C)` = mean(`최저기온(°C)`,na.rm=TRUE),
#                                                                                                                    `최고기온(°C)` = mean(`최고기온(°C)`,na.rm=TRUE),
#                                                                                                                    `평균 풍속(m/s)` = mean(`평균 풍속(m/s)`,na.rm=TRUE),
#                                                                                                                    `평균 현지기압(hPa)` = mean(`평균 현지기압(hPa)`,na.rm=TRUE),
#                                                                                                                    `일 최심신적설(cm)` = mean(`일 최심신적설(cm)`,na.rm=TRUE),
#                                                                                                                    `일강수량(mm)` = mean(`일강수량(mm)`,na.rm=TRUE),
#                                                                                                                    `강수 계속시간(hr)` = mean(`강수 계속시간(hr)`,na.rm=TRUE),
#                                                                                                                    SO2 = mean(SO2,na.rm=TRUE),
#                                                                                                                    CO = mean(CO,na.rm=TRUE),
#                                                                                                                    O3 = mean(O3,na.rm=TRUE),
#                                                                                                                    NO2 = mean(NO2,na.rm=TRUE),
#                                                                                                                    PM10 = mean(PM10,na.rm=TRUE),
#                                                                                                                    PM25 = mean(PM25,na.rm=TRUE),
#                                                                                                                    PM10등급 = mean(PM10등급,na.rm=TRUE),
#                                                                                                                    PM25등급 = mean(PM25등급,na.rm=TRUE),
#                                                                                                                    인구수 = sum(인구수,na.rm=TRUE),
#                                                                                                                    발생건수 = sum(발생건수),
#                                                                                                                    발병률 = sum(발생건수)/sum(인구수)*100)



#시계열 분석을 위해 안정적 시계열인지 확인 
plot(analysis_data)
str(analysis_data)
analysis_data <- as.data.frame(analysis_data)
analysis_data<- analysis_data[, - c(1,2,4)]

fit <- glm(발생건수 ~ .,data=analysis_data)

summary(fit)


ts <- ts(analysis_sido_day$발생건수,frequency = 365, start = c(2016,1))

fit <- stl(ts, s.window = 'periodic')

par(mfrow=c(3,6))