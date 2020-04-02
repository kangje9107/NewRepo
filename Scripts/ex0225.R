library(prettyR)
library(ggplot2)
chitest <-ch2_discrete
crosstbl<-table(chitest$product, chitest$gender)
crosstbl

# 교차분할표를 통한 카이제곱 검정
xtabs(~product + gender, data = chitest)
chisq.test(crosstbl)

fisher.test(crosstbl)

#독립성 검정을 위한 유의수준을 0.05로 정함. 

install.packages("MASS")
library(MASS)
data(survey)
data(mpg)
#교차표 생성
mpg <- as.data.frame(mpg)
xtab(formula = ~fl+drv, data = mpg)
crosstab<-xtabs(formula = ~fl+drv, data = mpg) #빈도수 만 보여줌.
crosstab
fisher.test(crosstab, alternative = 'greater')


crosstab2 <- table(survey$W.Hnd)
crosstab2

chisq.test(crosstab)
fisher.test(crosstab)

#적합성 검정, default양측검정
chisq.test(crosstab2, p = c(.3, .7)) #귀무가설을 기각 , 대립가설 채택.

#공분산
with(data = mpg, expr = {
  cov(x = cty, y = hwy)
}) 

t.test(df$Calories, mu = 140)

table(df$Type)
var.test(df$Calories)

## T- test 
# 등분산 검정 : var.test , H0 :두 집단간 분산은 같다. 
var.test(data = df, Calories ~ Type) # r두 그룹의 분산비ratio of var.를 보는것임. 이는 f dist.를 따름 
var.test(data = df, x = df$Calories, y = df$Type) 
? var
# 등분산 var.equal=F로 놓으면, Welch's t-test 가 나옴.
t.test(data= df, Calories ~ Type, var.equal =T)


t.test(df1$post - df1$pre) #두 평균의 차이가 없다. 

#정규성 검정
data(ToothGrowth)
my_data <- ToothGrowth
set.seed(1234)
require(dplyr)
sample_n(my_data, size = 10, replace = FALSE)   # random sampling , 비복원추출만.


install.packages('ggpubr')
library(ggpubr)
ggdensity(my_data$len, 
          add = 'mean', 
          color = 'red', 
          fill = 'lightblue', 
          alpha = .5, 
          title = 'Density plot of Tooth Length', 
          xlab = 'Tooth length')

require(ggpubr)
ggqqplot(my_data$len, color = 'red')

install.packages('car')
library(car)
car::qqPlot(my_data$len)

stats::qqnorm(my_data$len)
stats::qqline(my_data$len, col = 2)


#정규성검정 2
data(mpg)
my_data<-as.data.frame(mpg, stringAsFactors = FALSE)

install.packages('DT')
library(DT)
head(my_data)
datatable(sample_n(my_data, size =10, replace= FALSE))
ggdensity(my_data$cty, 
          add = 'mean', 
          color = 'red', 
          fill = 'lightblue', 
          alpha = .5, 
          title = 'Density plot of City Milege', 
          xlab = 'City Milege')

ggqqplot(my_data$cty, color = 'red')
qqPlot(my_data$cty)
shapiro.test(my_data$cty)
