
#install.packages('Hmisc')
#install.packages('prettyR')
install.packages('moments')
library(moments)
library(Hmisc)
library(prettyR)
library(ggplot2)
test<- read.csv(file.choose())
shapiro.test(test$score)
hist(test$score, breaks = 5, freq = F)
lines(density(test$score), col = 'red', lwd = 2)

#범주형변수 분석
test2 <- read.csv(file.choose())
freq(test2)
describe(test2)

head(mpg)

freq(mpg[1:2])
freq(mpg[c(7,10)])
prettyR::describe(mpg[c(7,10)])

#카이제곱검정(~종속 +독립, )
xtab(~product+gender, data=test2)
xtab(~gender+area, data=test2)
xtab(gender~area, data=test2)

range(mpg$cty)
range(mpg$hwy)

skewness(mpg$cty) #0과 가까울수록 정규분포, 1이 안되면 나름 정규분포와 가까운것임. 
hist(mpg$cty, freq = F, breaks = 15)

kurtosis(mpg$cty)
quantile(mpg$cty)

cor(mpg$hwy, mpg$cty)
stem(mpg$cty)
scale(mpg$cty) #연속형 변수값을, 평균이 0이고 분산이 1인 값으로 표준화해줌.
shapiro.test(mpg$cty) #정규분포를 따르지 않는다. 
shapiro.test(scale(mpg$cty)) #표준화 한거나 안한거나 같음.
boxplot(mpg$cty) #왜 같나? 극단치가 있어서

#지수를 개발하려고 할 때는, 표준화를 쓰는게 아니라 삼각함수를 써라 ? 
#지수 : 0~x까지라고 설정, 


boxplot(mpg$cty ~ mpg$fl)
boxplot(cty~fl, data = mpg, col = 'grey')
result <- boxplot(mpg$cty)
class(result)
result

#chi sq test 성별에 따라 과자 판매량 

test_3<-read.csv(file.choose(), fileEncoding = 'CP949')
crossbl <- xtabs(data = test_3, formula = ~snack + gender)
chisq.test(crossbl)
