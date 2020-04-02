# 조건추론나무 (예측변수와 결과변수간의 상관계수 높/낮으로 유의성 판단)
# 전통적 decision tree(비 동질성인 집단을 분류할 목적) 와 다름

install.packages('party')
library(party)


#load df_wbc
View(df_wbc)


df_wbc <- df_wbc[-1]
df_wbc <- as.data.frame(df_wbc)

df_wbc$Class <- factor( df_wbc$Class, levels = c(2, 4), 
                        labels= c('benign', 'malignant'))
str(df_wbc)




fit.ctree <- ctree(formula = Class ~ ., data = df_wbc)

plot(fit.ctree)
#해석 :
# shadow 영역이 없을땐 양성(2)란 얘기


attributes(fit.ctree)


ctree.pred <-  predict(fit.ctree, newdata = df_valid, type = 'response')
class(ctree.pred)
View(ctree.pred)
# 에측 결과값은 2또는 4가 아님. factor 를 숫자값으로 변환해서 이렇게 나오는 것임. 
# factor 조건추론에서는 factor 를 숫자 변환하지마라


cutoff <- 3
pred <- ifelse(ctree.pred>cutoff, 4, 2)


#혼동행렬 : table (실측치, 예측지)
(ctree.cm <- table(df_valid$Class, pred))


table(df_train$Class)
library(prettyR)
freq(pred)
freq(df_train)


install.packages('partykit')
library(partykit)


install.packages('randomForest')
library(randomForest)

attach

randomForest(...)
args(randomForest)
fit.forest <- randomForest(Class)

fit.forest <- randomForest(
  Class ~ .,
  data = df_train, 
  na.action = na.roughfix,
  importance = T)


fit.forest
#number : 500개이므로 볼 수 없다 -> blackbox 접근법


# 예측 변수의 중요도 파악
importance(fit.forest, type = 2)


(fit.pred <- predict(fit.forest, df_valid))

freq(fit.pred)


