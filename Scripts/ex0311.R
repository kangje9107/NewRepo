

women

fit <-lm(formula = weight ~ height, data = women)
summary(fit)

names(fit)

fitted.values(fit)

fit$fitted.values
fit$residuals

coefficients(fit)
fit$model

attach(women)
plot(height, weight)
abline(fit)


fit2 <- lm(weight ~ height + I(height^2), data = women)
#I( ) : 괄호 안의 표현식을 산술적으로 계산한다는 의미
# -1 : 회귀계수에서 절편을 없애라는 의미
fit2

summary(fit2)
plot(height, weight)
abline(fit2) #곡선은 못만듦
lines(height, fitted(fit2), col='red')

#추정 회귀 방정식
coef(fit2)


#weight = 261.87818358 + (-7.34831933)*height + (0.08306399) height^2

fit3 <- lm(weight ~ height + I(height^2) + I(height^3), data = women)
fit3
summary(fit3)


#다차원 방정식 시각화
#car - scatterplot 함수는 식을 넣으면  polynomial regression(유의성검정은안함)해서 그림 그림
#회귀 분석 전에ㅓ 두 변수간에 가장 적합한 회귀 선의 모양이 어떤것인지 (단순, 다중, 다항 중..) 판별. 
library(car)
dev.off()
scatterplot(weight ~ height, data = women, 
            smoother.args = list(lty=1),
            spread = F,
            pch = 19)


#다중선형회귀
state.x77
str(state.x77)
class(state.x77)
df_states <- as.data.frame(state.x77[ , c('Murder','Population','Illiteracy','Income', 'Frost')])
View(df_states)

cor(df_states) #상관계수 행렬 

scatterplotMatrix(df_states) #산점도 행렬

covMatrix <-cov(df_states) #cov2cor 
cov2cor(covMatrix)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost , data = df_states)
fit <-lm(Murder ~ . , data = df_states)
fit
summary(fit)

dev.off()
plot(fit)
par(mfrow=c(2,2))
# 1번째 그림 : 선형성 - 주어진 x 값에 대한 y 값이 선형성을 가져야한다. 
# - Fitted value (추정을 통한 예측값)과 잔차(실제값-예측값) 사이의 산점도는, 잔차 = 0 선을 중심으로 나타난 잔차값이 어떤 패턴을 나타나면 안되며 위아래로 점들이 비슷하게 나와야한다.


#pop 1단위 증가시 & 다른 변수는 일정할 때  -> 살인율이 ~ 만큼 증가한다고 해석.

#상호작용이 있는 다중선형회귀
mtcars
fit<-lm(mpg ~ hp + wt, data = mtcars)
head(mtcars)
fit<-lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)
#interaction 해석 : 차의 마력hp는 차의 중량 wt 에 따라 변화한다..
#  A:B interaction :A 는 B에 의해 달려잇다. 


coef(fit)
library(effects)
plot(effect("hp:wt", fit), multiline = T)

install.packages('gvlma')
library(gvlma)

qqPlot(fit, labels = row.names(df_states), simulate = T, main = '-main Title')


#잔차 독립성 검증 test
durbinWatsonTest(fit)
# H0 : rho 모상관계수 == 0,무상관 즉 잔차는 서로 독립이다. 
# H1 : rho != 0 

#독립변수와 종속변수 간의 관계가 선형적인지 판단 
library(car)
crPlots(fit)
# 선형성 가정을 위배하는 경우, 현재 fitting model은 적합하지 않은 것으로 간주하고 다항회귀같은 곡선적 요인을 추가하거나 
# 하나 또는 둘 이상의 독립변수들을 변환해야함 
ncvTest(fit)
# H0 : 등분산성을 가진다ㅏ.