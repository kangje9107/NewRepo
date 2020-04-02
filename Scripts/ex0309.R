#one-way ANOVA

ch8 
table(ch8$age) # unbalanced design
str(ch8)
ch8 <- as.data.frame(ch8)
ch8$age <- factor(ch8$age, levels = c('20대','30대','40대'), labels = c('20s','30s','40s') )

library(dplyr)
library(ggplot2)
freq(ch8$age)

tapply(ch8$score, ch8$age, mean)
#tapply(연속형변수, 그룹 범주형 변수, 적용할함수)

#매번 $ 쓰기 귀찬을 때 : global 변수에 ch8을 붙임
attach(ch8)
tapply(score, age, mean)

fit.aov <- aov(score ~ age, data = ch8)
fit.aov

summary(fit.aov)
anova(fit.aov)

detach(ch8)

# --------------------------------------------------------
# Assumptions for one-way ANOVA 
# --------------------------------------------------------

# 1. 종속변수 정규성 검정 : 대표본  by CLT , Q-Q plot 시각적 검정, shapiro-wilk

# 1-1. qqnorm{stats}, qqline{stats}, qqplot{stats} 이용

library(car)
qqnorm(cholesterol$response)
qqline(cholesterol$response)

qqPlot(lm(response ~ trt, data = cholesterol), 
       main = 'Q-Q Plot', labels = F, simulation = T) # 선 안에 있으면 정규성

# 1-2. shapiro

shapiro.test(cholesterol$response)

# 2. factor의 레벨별 등분산성 검정 : Bartlett, Flinger, Brown Forshy
# 2-1. 

bartlett.test(response ~ trt, data = cholesterol) #등분산성


#-----------------------------------------------------------
# One-way ANOVA (일원분산분석) 수행 및 평균의 다중비교 분석
#-----------------------------------------------------------

# cholesterol{multcomp} 데이터셋 사용.
library(multcomp)

# 데이터셋에 대한 기본적 탐색 수행.
cholesterol
View(cholesterol)

# trt 범주형변수(=요인, factor)의 그룹개수와 빈도수/비율확인
library(prettyR)

freq(cholesterol$trt)  # freq{prettyR}

# trt 요인의 집단별 평균(response) 구하기


# cholesterol 데이터 셋을, 현재 실행환경에 붙임
# --> dataframe$를 붙일 필요가 없이, 변수명만으로 접근가능
attach(cholesterol)
#-------------------------------------------------


#-------------------------------------------------
# 1. 각 집단별 평균 집계통례량 구하기
#-------------------------------------------------
# aggregate(분석대상연속형변수, by=groupby기준변수지정, FUN=각 그룹에 적용할 함수)
aggregate(response, by=list(trt), FUN=mean)

# tapply(분석대상연속형변수, 요인변수, 각 그룹에 적용할 함수)
tapply(response, trt, mean)


#-------------------------------------------------
# 2. One-way ANOVA 수행 (일원분산분석) - 요인이 하나!!!
#-------------------------------------------------
# aov{stats} function 으로 분산분석 수행
# 수행결과, 분석가가 지정한 formula대로 model fitting
fit.aov <- aov(response ~ trt, data = cholesterol)
fit.aov

# 적합된 모델객체에는 다양한 속성이 있음
names(fit.aov)
fit.aov$fitted.values #모델에 fitting한 결과  

# 특정 속성에 접근하는 방법: fitted_model$속성이름
fit.aov$coefficients


#-------------------------------------------------
# 3. 아래 둘 중 하나 함수로 검정통계량 검정수행
#-------------------------------------------------

# fitted model의 검정통계량 출력 - 1
anova(fit.aov)

# fitted model의 검정통계량 출력 - 2
summary(fit.aov)


#-------------------------------------------------
# 4. 집단별 평균 시각화 - 사후분석과 비슷
#-------------------------------------------------
# 시각화 결과는, 평균의 다중비교 수행결과와 동일
#-------------------------------------------------
library(gplots)

# formula 대로, 각 집단별 평균을 시각화해서 보여줌 ****
plotmeans(formula = response ~ trt, data = cholesterol)


#-------------------------------------------------
# 5. 사후분석 - 평균의 다중비교 수행 - 1 
#              (lwr 상한추정량, upr 상한추정량) 사이에 모평균이 있을 것이다 
#-------------------------------------------------
fit.mcp <- TukeyHSD(fit.aov)

fit.mcp
names(fit.mcp)

par(las=2) #plot상의 옵션 함수
par(mar=c(5,8,4,2))

plot(fit.mcp) # 평균의 차이 시각화
# 점선에 걸쳐있는 집단은 유의미하지않다. 그 외 집단간의 차이가 큰지 작은지 시각적으로 알 수 있다.

#-------------------------------------------------
# 6. 사후분석 - 평균의 다중비교 수행 - 2 
#-------------------------------------------------
library(multcomp)

tuk <- glht(fit.aov, linfct=mcp(trt='Tukey'))

dev.off() #plotting 영역을 리셋하는 함수

par(las=1)
par(mar=c(5,4,6,2))

plot( cld(tuk, level = .05), col='lightgray'   )
# 같은 알파벳이 적혀진 집단끼리의 차이는 무시할만하다  = 유의미하지않다, 나머지는 유의미하다 

#-------------------------------------------------
detach(cholesterol)

#-------------------------------------------------
# one-way ANCOVA
# factor 1,covariate 1
# ANCOVA  는 가정이 1개 더 필요함. 
# 회귀방정식의 기울기가 각 집단별로 동일해야한다. 


library(multcomp)
head(litter)
data("litter", package = 'multcomp')
attach(litter)

library(prettyR)

freq(dose)

aggregate(weight, by=list(dose),mean)

fit.aov <- aov(formula = weight ~ gesttime + dose , data = litter)
summary(fit.aov)

install.packages('effects')
library(effects)

effect("dose", fit.aov) # 공변량의 효과를 제거한 순수한 평균 구할 수 있음.

contrasts <- rbind("비투약그룹 vs 투약그룹" = c(3, -1, -1, -1))
contrasts

fit.glht <- glht(fit.aov, linfct = mcp(dose = contrasts))
summary(fit.glht)

fit2.aov <- aov( formula = weight ~ gesttime * dose, data = litter )
summary(fit2.aov)
#interaction effect는 통계적으로 '없다'고 나옴
# 상호작용효과가 없어야만 투약 vs 비투약 비교 가능
# 공변량이 체중에 미치는 영향은 있으나, 상호작용은 없음. 
# y ~ A + B + A:B    (둘중 하나는 교란요인)  
# A, B 의 영향은 유의미하지만  AB 의 상호작용 효과는 유의미하지않다 

#ancova 결과를 신뢰하려면 각 기울기가 같아야한다 
install.packages('HH')
library(HH)
ancova(weight ~ gesttime + dose, data = litter) 
#각 변수의 기울기가 같음을 확인 가능.


# two way Factorial ANOVA 
# 6개 그룹간의 기니피그 치아 길이
head(ToothGrowth)
attach(ToothGrowth)
aggregate(len, by= list(supp, dose), mean)

detach(ToothGrowth)
library(prettyR)

# 교차분할표 -> 균형설계임을 확인함
table(supp, dose) 
xtab(~supp + dose, data = ToothGrowth)
str(ToothGrowth)

dose <- as.factor(dose)
str(ToothGrowth)

fit.aov <- aov(formula = len ~ supp * dose)
fit.aov
#fit.aov <- aov(formula = len ~ supp + dose + supp:dose) -> 똑같은거임
anova(fit.aov) #각각의 factor 뿐 아니라 상호작용 효과도 있다

#상호작용 플롯
interaction.plot(dose,supp, len, type = 'b', 
            col = c('red', 'blue'), pch = c(16, 18))

