#-------------------------------------
# 한국어 텍스트 마이닝 - 2
#-------------------------------------

# {KoNLP} 패키지용 이용하여,
# 한글 테그스트의 word cloud 만들기

# (1) 한글 텍스트 수집 (영문 위키결과와 같은 내용으로)
#     한글 위키피디아에서 "데이터 사이언스" 검색어로 검색한 페이지의
#     내용을 수집
install.packages('rvest')
library(rvest)
library(dplyr)
library(tm)

library(XML)



html <- read_html(as.character('https://news.naver.com/main/ranking/read.nhn?mid=etc&sid1=111&rankingType=popular_day&oid=025&aid=0002989464&date=20200401&type=1&rankingSeq=5&rankingSectionId=103'))


(news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8'))
(news_content <- repair_encoding(html_nodes(html,'#articleBodyContents')),from = 'utf-8'))
(clean_doc <- xpathApply(news_content, '//p', xmlValue))

class(news_content)
nouns <- extractNoun(news_content)

( mnouns <- unlist(nouns) )

(mnouns.freq <- table(mnouns))
View(mnouns.freq)
(v <- sort(mnouns.freq, decreasing = T))
View(v)
wordcloud2(v)

wordcloud2(v[1:100])

