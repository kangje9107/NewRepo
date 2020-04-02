str(ch8)
#분산분석 위해chr > factor type 변환
as.data.frame(ch8)
ch8$age<-as.factor(ch8$age)
str(ch8)
?aov

anova<-aov(formula = score ~ age , data = ch8)
#Estimated effects may be unbalanced > 비균형설계.
#factor 별 개체수가 다름. 
table(ch8$age)
summary(anova)
#Pr : 집단간의 평균이 다르다. 뭐가 다른지는 사후분석 ..
