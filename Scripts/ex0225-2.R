# 정규성검정
library(ggpubr, quiet = TRUE)

set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10,20,3), 1)
  )
my_data

library(dplyr, quietly = TRUE)
sample_n(tbl = my_data, size = 10, replace = FALSE)

summary(my_data$weight)

ggboxplot(
  my_data$weight, 
  ylab = 'Weight (g)',
  xlab = FALSE,
  ggthem = theme_minimal()
)
shapiro.test(my_data$weight)
ggqqplot(my_data$weight, ggtheme = theme_minimal())

res <- t.test(my_data$weight, mu = 25, alternative = 'less')
res
t.test(my_data$weight, mu = 25, alternative = 'greater')

res$p.value
res$estimate
res$conf.int

women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
my_data <- data.frame(
  group = rep(c("Woman","Man"), each = 9),
  weight = c(women_weight, men_weight)

  )
my_data
group_by(my_data, group) %>% 
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE), 
    IQR = IQR(weight, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(my_data, 
          x = "group", 
          y = "weight", 
          color = 'group', 
          ylab = "Weight")

with(my_data, shapiro.test(weight[group == "Man"]))


wilcox.test(women_weight, men_weight)
wilcox.test(weight~group, data=my_data, exact = FALSE)
