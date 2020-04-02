library(dplyr)
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,70,80,90,85))
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70, 83, 65, 95, 80))
test1
test2
total <- left_join(test1, test2, by="id")
total

exam <- read.csv('./Data/csv_exam.csv')
exam

name <- data.frame(class = c(1, 2, 3, 4, 5),
                teacher = c("kim", "lee", "park", "choi", "jung"))
name
exam_new <- left_join(exam, name, by= "class")
exam_new

group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))

group_a
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_b

group_all <- bind_cols(group_a, group_b)  
group_all
group_all <- bind_rows(group_a, group_b) 
group_all

#chap.7 결측치 처리
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
is.na(df)
table(is.na(df))
table(is.na(df$score))
mean(df$sex) #결측지 있으면 안됨.
df %>% filter(!is.na(score))
#결측치 제거하기
df_nomiss <- df %>%  filter(!is.na(score)& !is.na(sex))
df_nomiss
df
na.omit(df)
mean(df$score, na.rm = T)

head(exam)
exam[c(3,8,15), "math"] <- NA #NA 삽입
exam %>%  summarise(mean_math = mean(math, na.rm = T))
exam
na.omit(exam)
na.exclude(exam)
na.pass(exam)
na.fail(exam)
quantile(exam, na.rm = TRUE) 

exam$math <- ifelse(is.na(exam$math), 55, exam$math)
exam
table(is.na(exam$math))


#이상치 처리
mpg <- ggplot2::mpg

mpg <- as.data.frame(mpg)

#min 극단치, 1Q, median , 3Q, MAX 극단치
hwy_stats <- boxplot(mpg$hwy)$stats
hwy_stats
mpg$hwy <- ifelse(mpg$hwy < hwy_stats[1]  | mpg$hwy >hwy_stats[5], NA, mpg$hwy)
table(is.na(mpg$hwy))
