library(readr)
library(dplyr)
library(data.table)

#-----------------------------------------
# 기상 데이터 가져오기
#----------------------------------------
df_2016 <- read_csv("lawdata/weather/기상관측2016real_Fixed.csv",locale=locale('ko',encoding='euc-kr'))
df_2017 <- read_csv("lawdata/weather/기상관측2017real_Fixed.csv",locale=locale('ko',encoding='euc-kr'))
df_2018 <- read_csv("lawdata/weather/기상관측2018real_Fixed.csv",locale=locale('ko',encoding='euc-kr'))

df_concat <- rbind(df_2016,df_2017,df_2018)




#-----------------------------------------
# 최다풍향 및 기타 널값 채우기
#----------------------------------------
View(df_concat %>% filter(is.na(`최다풍향(16방위)`)))  #위의 data table을 사용하여 직접 채워넣음
table(is.na(df_concat))

table(is.na(df_concat$ `평균 현지기압(hPa)`)) 
View(df_concat %>% filter(is.na(`평균 현지기압(hPa)`)))
temp <-df_concat %>%  filter(시도코드==26 & substr(일시,1,6)=='201612'&(!is.na(`평균 현지기압(hPa)`))) 
temp <- mean(temp$`평균 현지기압(hPa)`)
df_concat$`평균 현지기압(hPa)` <-ifelse(is.na(df_concat$`평균 현지기압(hPa)`),temp,df_concat$`평균 현지기압(hPa)`)

table(is.na(df_concat$ `평균 풍속(m/s)`))
View(df_concat %>% filter(is.na(`평균 풍속(m/s)`)))
temp <-df_concat %>%  filter(시도코드==11 & substr(일시,1,6)=='201710'&(!is.na(`평균 풍속(m/s)`))) 
temp <- mean(temp$`평균 풍속(m/s)`)
df_concat$`평균 풍속(m/s)` <-ifelse(((df_concat$시도코드==11)&(df_concat$일시=='20171014')),temp,df_concat$`평균 풍속(m/s)`)
temp <-df_concat %>%  filter(시도코드==11 & substr(일시,1,6)=='201712'&(!is.na(`평균 풍속(m/s)`))) 
temp <- mean(temp$`평균 풍속(m/s)`)
df_concat$`평균 풍속(m/s)` <-ifelse(((df_concat$시도코드==11)&(df_concat$일시=='20171205')),temp,df_concat$`평균 풍속(m/s)`)
df_concat$`평균 풍속(m/s)` <-ifelse(((df_concat$시도코드==11)&(df_concat$일시=='20171206')),temp,df_concat$`평균 풍속(m/s)`)

table(is.na(df_concat$ `최고기온(°C)`))
View(df_concat %>% filter(is.na(`최고기온(°C)`)))
temp <-df_concat %>%  filter(시도코드==11 & substr(일시,1,6)=='201710'&(!is.na(`최고기온(°C)`))) 
temp <- mean(temp$`최고기온(°C)`)
df_concat$`최고기온(°C)` <-ifelse(((df_concat$시도코드==11)&(df_concat$일시=='20171012')),temp,df_concat$`최고기온(°C)`)

table(is.na(df_concat$ `평균기온(°C)`))
View(df_concat %>% filter(is.na(`평균기온(°C)`)))
temp <-df_concat %>%  filter(시도코드==27 & substr(일시,1,6)=='201707'&(!is.na(`평균기온(°C)`))) 
temp <- mean(temp$`평균기온(°C)`)
df_concat$`평균기온(°C)` <-ifelse(((df_concat$시도코드==27)&(df_concat$일시=='20170729')),temp,df_concat$`평균기온(°C)`)









df_concat <- df_concat %>% arrange(일시)
df_concat$일시 <- as.character(df_concat$일시)
df_concat$일시 <- as.Date(df_concat$일시,tryFormats = c("%Y%m%d", "%Y/%m/%d"))
#-----------------------------------------
# 16방위 변환
#----------------------------------------

df_concat$`최다풍향(16방위)` <- df_concat$`최다풍향(16방위)` %/% 22.5 
df_concat$`최다풍향(16방위)`<- df_concat$`최다풍향(16방위)` + 1
df_concat$`최다풍향(16방위)`<- df_concat$`최다풍향(16방위)` %% 16
df_concat




df_concat
df_concat <- df_concat %>% select(-X1,-기상번호)
View(df_concat)
write.csv(df_concat,"refinedata/weather/weather_data.csv")




#-----------------------------------------
#데이터 가져오기
#----------------------------------------

weather_concat<- read_csv("refinedata/weather/weather_data.csv",locale=locale('ko',encoding='euc-kr'))
dust_total <- read_csv("refinedata/weather/dust_data.csv",locale=locale('ko',encoding='euc-kr'))

dust_total <- dust_total %>% select(-X1)
weather_concat  <- weather_concat  %>% select(-X1)

dust_total <- rename(dust_total,일시=측정일시,시도코드=code)

#-----------------------------------------
#미세먼지와 날씨 결합
#----------------------------------------

weather_concat$일시 <- as.character(weather_concat$일시)
weather_concat$일시 <- as.Date(weather_concat$일시,tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

dust_total$일시 <- as.character(dust_total$일시)
dust_total$일시 <- as.Date(dust_total$일시,tryFormats = c("%Y%m%d", "%Y/%m/%d"))

climate_total<- inner_join(weather_concat,dust_total,by=c("일시","시도코드"))



table(is.na(climate_total))


#-----------------------------------------
#데이터 가져오기
#----------------------------------------

df_medical_2016 <- read_csv("lawdata/medical/NHIS_2016_J_Cases.csv",locale=locale('ko',encoding='euc-kr'))
df_medical_2017 <- read_csv("lawdata/medical/NHIS_2017_J_Cases.csv")
df_medical_2017 <- rename(df_medical_2017,가입자일련번호=`가입자 일련번호`)
df_medical_2018 <- read_csv("lawdata/medical/NHIS_2018_J_Cases.csv")
df_medical_total <- rbind(df_medical_2016,df_medical_2017,df_medical_2018)
df_medical_total <- rename(df_medical_total,일시=요양개시일자)
df_medical_total <- df_medical_total %>% select(-X1)

table(is.na(df_medical_total))

#-----------------------------------------
# 데이터 전처리
#----------------------------------------

df_medical_total <- df_medical_total %>% select(성별코드,연령대코드,시도코드,일시)



#발생건수 카운트
df_medical_total <- df_medical_total %>% group_by(성별코드,연령대코드,시도코드,일시) %>% summarize(발생건수 = n())

df_medical_total$일시 <- as.character(df_medical_total$일시)
df_medical_total$일시 <- as.Date(df_medical_total$일시,tryFormats = c("%Y%m%d", "%Y/%m/%d"))

#기상데이터와 진료데이터 연결
analysis_total<- inner_join(df_medical_total,climate_total,by=c("일시","시도코드"))
str(analysis_total)



#-----------------------------------------
#시도별 총인구수 데이터 가져오기
#----------------------------------------
population <- read_csv("refinedata/medical/연령_및_성별_인구__읍면동_2015___시군구_2016__20200313101827.csv",locale=locale('ko',encoding='euc-kr'))




#-----------------------------------------
#발병률 데이터 넣기
#----------------------------------------
population
population$name <- as.factor(population$`행정구역별(읍면동)`)


code <- c('42','41','43','44','30','47','48','45','46','11','28','27','31','29','26','49','36') 
name <- c('강원도','경기도','충청북도','충청남도','대전광역시','경상북도','경상남도','전라북도','전라남도','서울특별시','인천광역시','대구광역시','울산광역시','광주광역시','부산광역시','제주특별자치도','세종특별자치시')
df_sido <- data.frame("code"=code,"name"=name)

population <- inner_join(population,df_sido,by='name')

population

population <- population %>% select(-`행정구역별(읍면동)`,-name)

population
#pop <- melt(population, id.vars = c(1:2))
pop <- melt(population, id.vars = c("연령별","code"))
pop
pop <- pop %>% mutate(년도=as.factor(substr(variable,1,4)),
                        성별코드 = as.factor(substr(variable,6,8)),
                        연령대코드 = as.factor(연령별),
                        인구수 = value  )
pop
pop <-pop %>% select(-연령별,-variable,-value)

pop <- rename(pop,시도코드=code)

str(pop)
str(analysis_total)

analysis_total$년도 <- substring(analysis_total$일시,1,4)



analysis_total$시도코드 <- as.factor(analysis_total$시도코드)

analysis_total <- inner_join(analysis_total, pop, by=c("시도코드","년도","연령대코드","성별코드"))

View(analysis_total)

analysis_total$발병률 <- analysis_total$발생건수/analysis_total$인구수*100

View(analysis_total)

write.csv(analysis_total,"refinedata/analysis/analysis_total.csv")




#-----------------------------------------
# 일별 16방위 빈도수 계산
#----------------------------------------


windlist <- df_analysis$`최다풍향(16방위)`
daylist <- df_analysis$일시
volumelist <- df_analysis$`평균 풍속(m/s)`

beforeday <- substr(daylist[1],1,10)

beforewind <- windlist[1]
beforevolume <- volumelist[1]

windfreq <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
volumefreq <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

windfreq[beforewind+1]<-windfreq[beforewind+1] + 1
volumefreq[beforewind+1]<-beforevolume

maxlist <- c();
end <- TRUE

for(i in (2:length(df_analysis$`최다풍향(16방위)`))){
  nextday <- substr(daylist[i],1,10)
  nextwind <- windlist[i]
  nextvolume <- volumelist[i]
  
  if(!is.na(nextwind)){
    if(beforeday == nextday){
      windfreq[nextwind+1] <- windfreq[nextwind+1] + 1
      volumefreq[nextwind+1] <- volumefreq[nextwind+1] + nextvolume
    }else{
      
      if(i == length(df_analysis$`최다풍향(16방위)`)){
        end <- FALSE
      }
      windfreq[nextwind+1] <- windfreq[nextwind+1] + 1
      volumefreq[nextwind+1] <- volumefreq[nextwind+1] + nextvolume
      if(length(which(windfreq==max(windfreq)))>1){
        maxwind <- -1
        for(i in which(windfreq==max(windfreq))){
          if(maxwind<volumefreq[i]){
            maxwind <- volumefreq[i]
          }
        }
        maxlist <- append(maxlist,which(volumefreq==maxwind)[1])
      }else{
        maxlist <- append(maxlist,which(windfreq==max(windfreq))[1])
      }
      
      volumefreq <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
      windfreq <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
    
  }
  
  beforevolume <- nextvolume
  beforeday <- nextday 
  beforewind <- nextwind 
}



if(end){
  if(length(which(windfreq==max(windfreq)))>1){
    maxwind <- -1
    for(i in which(windfreq==max(windfreq))){
      if(maxwind<volumefreq[i]){
        maxwind <- volumefreq[i]
      }
    }
    maxlist <- append(maxlist,which(volumefreq==maxwind)[1])
  }else{
    maxlist <- append(maxlist,which(windfreq==max(windfreq))[1])
  }
}



data <- analysis_total %>% group_by(substr(일시,1,10)) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
                                                             `최저기온(°C)` = mean(`최저기온(°C)`,na.rm=TRUE),
                                                             `최고기온(°C)` = mean(`최고기온(°C)`,na.rm=TRUE),
                                                             `평균 풍속(m/s)` = mean(`평균 풍속(m/s)`,na.rm=TRUE),
                                                             `평균 현지기압(hPa)` = mean(`평균 현지기압(hPa)`,na.rm=TRUE),
                                                             `일 최심신적설(cm)` = mean(`일 최심신적설(cm)`,na.rm=TRUE),
                                                             `일강수량(mm)` = mean(`일강수량(mm)`,na.rm=TRUE),
                                                             `강수 계속시간(hr)` = mean(`강수 계속시간(hr)`,na.rm=TRUE),
                                                             SO2 = mean(SO2,na.rm=TRUE),
                                                             CO = mean(CO,na.rm=TRUE),
                                                             O3 = mean(O3,na.rm=TRUE),
                                                             NO2 = mean(NO2,na.rm=TRUE),
                                                             PM10 = mean(PM10,na.rm=TRUE),
                                                             PM25 = mean(PM25,na.rm=TRUE),
                                                             PM10등급 = mean(PM10등급,na.rm=TRUE),
                                                             PM25등급 = mean(PM25등급,na.rm=TRUE),
                                                             인구수 = sum(인구수,na.rm=TRUE),
                                                             발생건수 = sum(발생건수),
                                                             발병률 = sum(발생건수)/sum(인구수)*100)


data $ `최다풍향(16방위)` <- maxlist
str(analysis_total)

fit <- aov(formula=발병률~시도코드,data=analysis_total)

