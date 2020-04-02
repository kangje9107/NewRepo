#install.packages('corrplot')
library(ggplot2)
library(dplyr)
library(corrplot)
mpg<-as.data.frame(ggplot2::mpg)

mpg_diff <- mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c("compact", "suv"))
View(mpg_diff)

t.test(data = mpg_diff, cty ~ class, var.equal  = T)

economics <- as.data.frame(ggplot2::economics)
View(economics)
cor.test(economics$unemploy, economics$pce)

head(mtcars)
car_cor <- cor(mtcars) #상관행렬
round(car_cor,2)
corrplot(car_cor)
corrplot(car_cor, method = 'number')
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
         method = "color",       # 색깔로 표현
         col = col(500),         # 색상 200개 선정
         type = "upper",         # 왼쪽 아래 행렬만 표시
         order = "hclust",       # 유사한 상관계수끼리 군집화
         addCoef.col = "black",  # 상관계수 색깔
         tl.col = "black",       # 변수명 색깔
         tl.srt = 45,            # 변수명 45도 기울임
         diag = F)               # 대각 행렬 제외

