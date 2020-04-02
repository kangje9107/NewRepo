install.packages("corrplot", quiet =TRUE)
library(dplyr, quietly = TRUE)

rs <- sample_n(tbl = mtcars, size = 10, replace = FALSE)

library(knitr, quietly = TRUE)
kable(x = rs, caption = 'The random sampling of mtcars data set')
M <- cor(mtcars)
head( round(M, 2))
library(corrplot)

corrplot(M, type = "upper", order = "hclust", col = c("black", "white"), bg= "lightblue")
install.packages("R")
corrplot(M, type="upper", order="hclust", tl.col="blue", tl.srt=45)

#논문시에는 상관계수말고 p value 를 값으로 하기도함 

cor.mtest <- function(mat, ...){
  mat <- as.matrix(mat)
  n<- ncol(mat)
  p.mat <- matrix(NA,n,n)
  diag(p.mat)<-0
  
  for (i in 1:(n-1)){
    for(j in (i + 1):n){
      tmp <- cor.test(mat[,i], mat[,j],...)
      p.mat[i,j] <- p.mat[j,i]<- tmp$p.value
    }
  }
  
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(mtcars)

corrplot(M, type = "upper", order="hclust", p.mat=p.mat, sig.level = 0.01)



#5-4

mcor <- round(cor(mtcars),2)
mcor

upper.tri(mcor)

upper<-mcor

upper[upper.tri(mcor)] <- ""
upper
(upper<-as.data.frame(upper))

class(mcor)

library(xtable)
print(xtable(upper), type = "html")
cov(1:5, 5:1)

