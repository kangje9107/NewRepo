# 회귀분석의 가정이 훼손되었을 때에, 해결방법
#
# (1) 관측치(이상치) 삭제
# (2) 종속변수의 변환
# (3) 독립변수의 가감
# (4) 다른 방식의 회귀분석


# *************
# (1) 관측치(이상치) 삭제
# *************
# 가. 이상치를 제거 ---> 정규성 개선
# 나. 영향치를 제거 ---> 정확성 개선
# 다. 위의 가,나를 제거해도, 여전히 이상치/영향치가 존재한다면, 적합성 적을 때까지 삭제과정 지속 ??????????????????
# 라. 바로, 왜? 이상치가 발생했는가?를 이해하는 것이 훨씬 중요함
#     이유: 측정장비의 오류, 관측자의 오류 등등......
#     이런 이유로 발생한 이상치라면 ----> 무조건 삭제!!
# 마. Serendipity(우연한 발견) ---> 중요한 발견!!!!
#     이때는, 왜 이 이상치가 다른 관측치아 다른지의 이유를 발견해낼수만 있다면..... ---> 위대한 발견이 될 수 도 잇음!!!


# *************
# (2) 종속변수의 변환
# *************
# 가. 적합시킨 회귀모델이 가정을 충족하지 못하는 경우,
#     하나 이상의 변수를 변환하면, 상황이 개선되기도 함
# 나. 변환은, 전형적으로, 변수 Y(종속) --> 변수 Yλ로 대체하는 방식
# 다. 만일, 모델이 정규성 가정에 위해된다면,
#     일반적으로 종속(반응)변수를 변환한다
# 라. 변환을 도와주는 패키지가 있음
#     powerTransmform{car} 함수 : 
#       MLE(최대우도법-Maximum Likelyhood Estimation)를 이용해서
#       변환지수값 도출

df_states<-state.x77

View(df_states)

df_states <- as.data.frame(df_states)

library(car)

( lambda <- powerTransform(df_states$Murder) )

# 통계적 가설:
#    H0: λ = 1 (No transformation needed)
#    H1: λ != 1 (Transformation needed)
summary(lambda)


# 마. 선형성 가정이 위배되는 경우, 역시 예측(독립)변수의 변환이 도움이 됩니다.
#     이때 역시 도움이 되는 함수는 아래와 같음:
#
#       boxTidwell{car} 함수 이용
#
#       -> 이 함수를 통해, 예측변수의 변환지수 산출에 이용

library(car)

# Usage: boxTidwell(formula, data = dataset)

# 통계적 가설:
#    H0: λ = 1 (No transformation needed)
#    H1: λ != 1 (Transformation needed)
boxTidwell(Murder ~ Population + Illiteracy, data = df_states)


# 바. 등분산성 개선을 위한 지수변환에 도움을 주는 함수가 있음:
#
#      spreadLevelPlot{car} 함수 시각화 결과와 제안 지수를 고려함

library(car)
spreadLevelPlot(fit)



# *************
# (3) 변수의 가감 (예측변수, 독립변수)
# *************
# 가. 회귀모델 내의 변수교체는 ---> 모델 적합성에 영향을 줌
# 나. 중요한 변수를 누락했다면 ---> 추가
# 다. 문제가 되는 변수가 있다면 ---> 삭제
# 라. 다중공선성 문제를 해결하는 중요한 방법 --> 변수의 삭제
# 마. 다중공선성과 관련된 중요한 포인트: (***주의사항***)
#     (1) 만일, 회귀분석을 통한, 회귀모델의 목적이 예측에만 있다면,
#         다중공선성은 전혀 문제가 안됨
#     (2) 만일, 회귀분석의 목적이, 기술(인과에 설명)이 목적이라면,
#         위의 '라' 고려할 것
#     (3) 만일, 회귀분석의 목적이 기술과 예측 둘다이고, 그래도, 다중공선성 문제가 해결되지 않는다면???
#         다른 회귀분석을 수행 --> 리지회귀(Ridge Regression)
#                                   (다중선형회귀의 변종)



# 검정력이 높으면서 AIC 값이 작으면서 변수의 개수가 작은걸로 

# *************
# (4) 다른 방식의 회귀분석을 고려해야 하는 상황을 정리
# *************
# 가. 다중공선성 문제 ---> 리지회귀
# 나. 이상치/영향치 등의 문제 ---> Robust 회귀
# 다. 정규성 가정 위배 ---> 비모수 회귀
# 라. 비선형성 문제    ---> 비선형 회귀
# 마. 오차의 독립성 위배 ---> 시계열 회귀 or 다차원 회귀
# 바. 일반적인 위의 모든 가정이 지속으로 위배 --->
#     일반화된 선형모델(Generalized Linear Regression) 회귀









#--- 최적의 회귀모델 선택 방법 ---
#
# 시도하여 만들어낸, 많은 회귀모형 중에, 선택을 해야 하는
# 상황에 직면하게 됨.
#
# 최소한의 선택기준:
#  가. 예측정확성 - 예측의 정확도를 높일 수 있는 모델 -> adjusted R^2
#  나. 검약성     - 단순하고 반복가능한 모델
#  다. 최종적인 결정은 바로, 분석가의 결정임!!!


# *********************
# (1) 회귀모형의 비교방법 - 1 (ANOVA)
# *********************
# 회귀분석의 가정을 충족시키고, 회귀모형의 통계적 유의성까
# 충족된 여러 모형이 있을 때, 이들 모형을 비교.
# 
# 두 개의 모델을 비교: 분산분석(ANOVA)
# 비교시 제약(조건):
#   가. 비교할 두 회귀모델이 소위 "Nested" 되어 있어야 함
#   나. 비교할 두 모델이, Nested되어있지 않으면 --> AIC비교를
#       수행해야 함
#

df_states

fit1 <- lm(
  Murder ~ Population + Illiteracy + Income + Frost,
  data = df_states
)

fit2 <- lm(Murder ~ Population + Illiteracy, data = df_states)

fit3 <- lm(Murder ~ Illiteracy, data = df_states)

# 회구분석 가정 충족, 다중공선성고려, 이상치 관리 등은 모두
# 만족시켰다고 가정.

# Multiple R-squared:  0.567,	Adjusted R-squared:  0.5285 
summary(fit1)

# Multiple R-squared:  0.5668,	Adjusted R-squared:  0.5484
summary(fit2)

# Multiple R-squared:  0.4942,	Adjusted R-squared:  0.4836
summary(fit3)

# 두 fitted model 비교 수행 by ANOVA
# 두 회귀모델의 차이가 없는 경우 ---> 두번째 기준인 검약성으로 선택 -> 즉, 예측변수가 적은 2번째 모델을 선택
anova(fit1, fit2)       # OK

anova(fit1, fit2, fit3)  # OK

# *********************
# (1) 회귀모형의 비교방법 - 2 
# (AIC: Akaike Information Criterion)
# *********************
# AIC 값이 작을 수록 --> 
#   **보다 적은 개수의 모회귀계수**가 적합하다 란 의미
#
# AIC{stats} 함수를 사용하여, AIC값 추출

AIC(fit1, fit2)        # OK

AIC(fit1, fit2, fit3)  # OK ****

# Income, Frost 예측변수를 뺀 모델이 더 좋은 모델임을
# 알려줍니다.








library(MASS)

#stepAIC - none : 최초 아무변수도 안뺏을때 AIC 값.


