library(ggplot2)
library(dplyr)
mpg <- as.data.frame(mpg)
mpg_diff <- mpg %>% 
    select(class,cty) %>% 
    filter(class %in% c('compact','suv'))

table(mpg_diff$class)

t.test(data=mpg_diff, cty ~ class , var.equal = T)

#유의확률 p value 값이 유의수준 0.05ㅇ 비해 매우 작으므로, 귀무가설을 기각하고 대립가설을 채택한다. 즉, 두 차종의 평균연비 차이가 통계적으로 유의미하다.


#'4. economics{ggplot2} 데이터셋을 이용하여, 미국의 실업자수(unemploy)와 개인소비경향(pce) 변수 간에 관계가 있는지 검정하고자 한다. 이때, 상관분석을 수행하여, 상관계수(r)를 도출하고, 두 변수간에 어떤 관계가 있는지 판단하고, 관계가 있는 경우, 그 관계가 통계적으로 유의미한 지 검정하시오. '

econ <- as.data.frame(ggplot2::economics)
head(econ)

cor.test(econ$unemploy, econ$pce)
#상관계수값 0.6145.... -> 두 변수간의 선형관계가 있음을 의미함. 
#유의확률 p value가 유의수준보다 매우 작으므로, 대립가설 채택, 즉 두 변수간의 상관관계가 통계적으로 유의미하다.

#두 차종 평균 연비 산출

library(ggplot2)
library(dplyr)

mpg <- as.data.frame(mpg)

head(mpg)

mpg_diff <- mpg %>% 
  select(class,cty) %>% 
  filter(mpg$class %in% c('compact', 'suv'))

table(mpg_diff$class)
#두 차종 평균 연비 차이에 대한 검증 수행
t.test(data = mpg_diff, cty ~ class, var.equal=T)
#검정 결과 :  유의확률 p value가 유의수준 0.05보다 매우 작은 값을 나타내므로 귀무가설을 기각하고 대립가설을 채택, 즉 두 차종 평균 연비의 차이가 통계적으로 유의미하다. 

#상관분석 시행 
econ <- as.data.frame(economics)
head(econ)

cor.test(econ$pce, econ$unemploy)
#상관계수  : 0.6147 , 두 변수 간에 상관성이 있음을 의미한다
#분석 결과 : p value가 유의수준보다 매우 낮게 도출되므로 귀무가설을 기각하고 대립가설을 채택한다




















