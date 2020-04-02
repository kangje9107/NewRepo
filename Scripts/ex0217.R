install.packages("dplyr")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
#raw_welfare <-read.spss(file="./Data/Koweps_hpc10_2015_beta1.sav")
raw_welfare <- load(file='Data/raw_welfare.rda')
raw_welfare <- as.data.frame(raw_welfare)
df_welfare <- raw_welfare #copy 
#save(raw_welfare, file = 'Data/raw_welfare.rda')

welfare <- rename(raw_welfare,
                  sex = h10_g3,            # 성별
                  birth = h10_g4,          # 태어난 연도
                  marriage = h10_g10,      # 혼인 상태
                  religion = h10_g11,      # 종교
                  income = p1002_8aq1,     # 월급
                  code_job = h10_eco9,     # 직종 코드
                  code_region = h10_reg7)  
class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Data/Koweps_Codebook.xlsx", col_names = T, sheet =2)
head(list_job)
welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income <- welfare %>% 
  filter(!is.na(code_job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income)) %>% 
  arrange(desc(mean_income))
View(job_income)

#x/y 축을 뒤집어 표현하기
ggplot(data = job_income, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

job_male <- welfare %>% 
  filter(!is.na(job) & sex == 1) %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

View(job_male)
ggplot(data = job_male, aes(x = job, y = n )) + geom_col() + coord_flip()

       