#correlation test 
library(ggpubr)
library(dplyr)
library(knitr)
my_data <- mtcars 
my_data
kable(sample_n(tbl=my_data, size =10, replace = FALSE ), caption = "A Test Data Set ")

ggscatter(my_data, x = "mpg", y = "wt", add = "reg.line", 
          conf.int = TRUE,
          cor.coef = TRUE,
          cor.method = "pearson", 
          xlab = "Miles/US gallon", 
          ylab = "Weight")
shapiro.test(my_data$wt)
ggqqplot(my_data$wt, ylab = "WT")

result<-cor.test(my_data$wt, my_data$mpg, method = "pearson")
w 
result$p.value
result$estimate

#correl matrix
data("mtcars")

my_data <- mtcars[,c(1,3,4,5,6,7)]
head(my_data)
res <- cor(my_data)
res
round(res, 2)

install.packages("Hmisc")
library(Hmisc)

res2 <- rcorr(as.matrix(my_data))
round(res2$P,2)

?rcorr

symnum(res, abbr.colnames = FALSE)
library(corrplot)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mtcars, pch = 9)
heatmap(x)

