#2020-03-19
#rattle  - ML 한번에 할 수 있는 패키지

install.packages('rattle.data')
library(rattle.data)
library(prettyR)
library(NbClust)

data(wine)


head(wine)
table(wine$Type)
freq(wine$Type)

wine <- wine[-1]
data(wine, package = 'rattle.data')

zWine<-scale(wine)
View(zWine)

nc <- NbClust(zWine, method = 'kmeans')
 # 파란 그래프가 급격히 떨어지는 '특이점' 직전의 값이 군집 개수 
 # ==     ***** Conclusion *****According to the majority rule, the best number of clusters is  3 

table(nc$Best.nc[1,])

(fit.kmeans <- kmeans(zWine,centers = 3))

install.packages('cluster')
clusplot(zWine, fit.kmeans$cluster)


table(
  wine$Type,
  fit.kmeans$cluster, 
  dnn = c('Actual', 'Predict')) # 범주형변수로 행 x 열 : 교차분할표 빈도수가 나옴

fit.kmeans$size



#ccc 지표 : 허위군집을 막을 수 잇는 지표. : 급격히 떨어지는 변곡점 지점의 직전에 있는 값이  CCC 가 제안하는 군집으 ㅣ개수 즉 0개. 

