library(dplyr)

df_exam <- read.csv('./Data/csv_exam.csv')
df_exam

#chain, pipe operator : ctrl + shift + m 
df_exam %>% filter(class != 3 & math >30)
df_exam %>% filter(class %in% c(2,4))

mpg <- ggplot2::mpg
mpg_a<- mpg %>% filter(displ <=4)
mpg_b <- mpg %>% filter(displ >5)
mean(mpg_a$hwy)
mean(mpg_b$hwy)
mpg_d <- mpg %>% filter(manufacturer=="chevolt" | manufacturer =='ford' | manufacturer =='honda')
mpg_d

df_exam %>% select(class, math, science) %>% head
df_exam %>% select(-math, -english)
df_exam %>% 
  filter(class ==1) %>% 
  head
df_exam
#내림차순
df_exam %>% arrange(desc(math))
df_exam %>% arrange(class, math)
#파생변수 추가
df_exam<-df_exam %>% mutate(total = math + english+science,
                   mean = total/3)
df_exam
df_exam <- df_exam %>% mutate(test = ifelse(science >= 60, "pass", "fail"))
df_exam %>% arrange(total)
df_exam %>% group_by(class) %>% summarise(mean_math = mean(math)) #그룹별로 summarize 
df_exam %>% group_by(class) %>% summarise(mean_math = mean(math),
                                          sum_math = sum(math),
                                          median_math = median(math),
                                          n = n())
df_mpg <- mpg
head(df_mpg)
df_mpg[, !names(df_mpg) %in% c("trans", "drv")] #언급행 제외하고 추출
df_mpg %>% group_by(manufacturer, drv) %>% summarise(mean_cty = mean(cty))
table(df_mpg$drv)
df_mpg %>% filter(class =="suv") %>% mode
#예제풀이 : 회사별로 "suv" 자동차의 도시 및 고속도로 통합 연비 평균을
#구해 내림차순으로 정렬하고, 1~5위까지 출력하기
df_mpg %>% 
  group_by(manufacturer) %>%
  filter(class=='suv') %>%
  mutate(mean_mpg = (cty + hwy)/2) %>% 
  summarise(mean_total = mean(mean_mpg)) %>% 
  arrange(desc(mean_total))
data.frame(df_mpg)

