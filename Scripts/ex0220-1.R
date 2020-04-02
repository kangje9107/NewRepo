#연속성변수의 정규성 변환 방법 중 대표적인것 : 
# 1. 로그변환 --> lob()
# 2. 제곱근변환 --> sqrt()
# 3. 

install.packages('UsingR')
library(UsingR)
head(cfb)
shapiro.test(cfb$INCOME)
hist(cfb$INCOME, breaks = 100, freq=F)

density(cfb$INCOME)

# 정규분포를 따르는지 아닌지
qqnorm(cfb$INCOME)
qqline(cfb$INCOME)


cfb <-transform(cfb,INCOME_log = log(INCOME+1) )
cfb <-transform(cfb,INCOME_sqrt = sqrt(INCOME) )
cfb <-transform(cfb,INCOME_x = 1/(INCOME+1) )
  head(cfb)
summary(cfb$INCOME)
View(cfb$INCOME_log)


shapiro.test(cfb$INCOME_log)
shapiro.test(cfb$INCOME_sqrt)
shapiro.test(cfb$INCOME_x)

hist(cfb$INCOME_sqrt, breaks =200, freq = F)
lines(density(cfb$INCOME_sqrt), col='red')
