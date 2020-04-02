#install_github('haven-jeon/KoNLP')
#install.packages('wordcloud')
#Sys.getenv('JAVA_HOME')
#Sys.getenv(JAVA_HOME = '/usr/lib/jvm/default-java')
library(dplyr)
library(rJava)
library(devtools)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
useNIADic()

"acorn@ubuntu:~/Documents/r_workspace/Data$ file -bi hiphop.txt
text/plain; charset=iso-8859-1
acorn@ubuntu:~/Documents/r_workspace/Data$ iconv -c -f iso-8859-1 -t utf-8

acorn@ubuntu:~/Documents/r_workspace/Data$ iconv -c -f euc-kr -t utf-8 hiphop.txt > hiphop.csv
"


txt<-readLines('Data/hiphop.csv')
head(txt)

#특문 제거
install.packages('stringr')
library(stringr)
txt <- str_replace_all(txt, '\\W', " ") 

#가장 많이 사용된 단어
#한국어 명사 추출 함수  KoNLP
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")
nouns <- extractNoun(txt)
View(nouns)
# 빈도표 보기 위한 list -> set  함수 :  unlist
wordcount<-table(unlist(nouns))
View(wordcount)

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
View(df_word)

df_word <- rename(df_word, word = Var1, freq = Freq)
df_word <-filter(df_word, nchar(word) >=2)
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% head(20)


#word cloud 

pal<-brewer.pal(8,"Dark2")
pal

set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .01, 
          scale = c(4, 0.3),
          colors = pal)

pal <- brewer.pal(9, "Blues")[5:9]
set.seed(1234)
wordcloud(words = df_word$word,
           freq = df_word$freq,
           min.freq = 2,
           max.words = 200,
           random.order = F,
           rot.per = .01, 
           scale = c(4, 0.3),
           colors = pal)
twitter<-read.csv('Data/twitter.csv', 
                  header = T, 
                  stringsAsFactors = F,
                  fileEncoding = 'UTF-8')
View(twitter)
twitter <- rename(twitter, 
                  no = 번호,
                  id =  계정이름,
                  date = 작성일,
                  tw =내용)
twitter$tw<-str_replace_all(twitter$tw, "\\W", " ")

nouns <- extractNoun(twitter$tw)
wordcount <- table(unlist(nouns))
View(wordcount)
df_word<-as.data.frame(wordcount, stringsAsFactors = F)
View(df_word)
df_word <- rename(df_word, word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word)>=2)
top20 <- df_word %>% arrange(desc(freq)) %>% head(20)
top20

library(ggplot2)
order <- arrange(top20, freq)$word
ggplot(data = top20, aes(x = word, y = freq)) + geom_col() + coord_flip() +
  scale_x_discrete(limit = order) + geom_text(aes(label = freq), hjust= -0.3)

pal <- brewer.pal(8, "Dark2")
set.seed(1200)
wordcloud(words = df_word$word, 
          freq = df_word$freq,
          min.freq = 10, 
          max.words = 200,
          random.order = F,
          scale = c(8,0.3),
          colors = pal)
