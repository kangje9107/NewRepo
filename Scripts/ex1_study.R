
rm(list=ls()) #변수 clear
cat('\f') #console clear

a <-1
a 
#---탐색적분석의 필수단계
class(a) #변수 type 검사
mode(a) #변수 값의 type 검사
str(a) #변수 structure = pandas info비슷
dim(a)
summary(a)
#----------------------

mpg
class(mpg) 
mode(mpg)


#범주형변수 factor 연산자
sex <- factor('m', c('m','f'))
sex
nlevels(sex)
is.factor(sex)
ordered(sex)
is.ordered(sex)

### vector 
x <- c(1,2,3)
names(x) <-c("1","2","3")
y <- c(1,2,100)
z <- seq(3,7)
h <- seq(1,10,2)
r <- 3:7
#indexing : 1부터 시작
x[1]
x[-1] #1번 빼고 다추출
x[2:3]
x[c(1,3)]
x["2"]
#vector 길이 
length(z)
NROW(z)
#벡터연산
identical(x,y) #한개bool값만 도출 -> if 문에 사용가능
x != y
x == y
100 %in% y #포함여부 연산
union(x, y)
intersect(x,y)
setdiff(x,y)
setequal(x,y)
seq_along(r)
vec_rep <-rep( 1:3 , times=5, each = 2 )
#list == Dictionary와 같음 
x<-matrix(1:12, ncol = 2, nrow = 2)
y<-matrix(1:12, nrow = 3, byrow = TRUE)
nrow = x[2,]
x %*% y
x
t(x)
x%*%solve(x)
array(1:12, dim = c(3,4))


#NA 처리 관련 함수 -> 수업자료 0213-1에 
#함수 
f <- function(...){
  args <- list(...)
  for (a in args){
    print(a)
  } 
}

f(3,4)
f <- function(x){
  
  return(function(y){return(x + y)})
  
}
g <- f(1)
g(2)

#메모리상 객체 관련 함수
ls() # 메모리상 모든 객체
rm() #지정 환경에서 객체를 삭제

#R 기본 데이터셋 정보 
library(help = datasets)


# "NIL"이라는 값을 R의 NA로 바꾸라
exam <- read.csv('./Data/csv_exam.csv', na.strings = c("NIL"))

exam

























