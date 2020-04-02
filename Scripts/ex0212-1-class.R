#load(file='Data/df_raw.original.rda')
library(dplyr)

df_raw <- data.frame(var1 = c(1,2,1), 
                     var2 = c(2,3,2))
df_new <- df_raw

#munging 1단계 -- 변수명바꾸기
df_new <- rename(df_new , v2 = var2)
help(rename) #function의 소속 package 알고싶으면 help 하면됨
?rename


df_mpg <- as.data.frame(ggplot2::mpg)

df_mpg
copy_df_mpg <- df_mpg
copy_df_mpg <- rename(copy_df_mpg ,city = cty, highway=hwy)
copy_df_mpg

df_mpg$total <- (df_mpg$cty + df_mpg$hwy)/2  
summary(df_mpg$total)
hist(df_mpg$total)
#ifelse() : T, F 한번에 처리
df_mpg$test <-ifelse(df_mpg$total>=20, 'pass', 'NA')
#df 내의 변수 한개로 빈도표 생성하기 
table(df_mpg$test) 
df_mpg$test <- c(1,2,3)
df_mpg$grade <- ifelse(df_mpg$total >=30, "A",
                       ifelse(df_mpg$total>=20 , "B", "C"))

head(df_mpg,20)
table(df_mpg$grade)

library(ggplot2)
library(dplyr)

midwest<-as.data.frame(ggplot2::midwest)
midwest
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

midwest <-rename(midwest, total = poptotal, asian = popasian)
names(midwest)
midwest$asianpercent <- (midwest$asian/midwest$total)*100
hist(midwest$asianpercent)

midwest$asianrate <-ifelse(midwest$asianpercent>=avg_asian, "large","small")
avg_asian <- mean(midwest$asianpercent)
table(midwest$asianrate)
qplot(midwest$asianrate)



