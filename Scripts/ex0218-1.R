install.packages("ggiraphExtra")
library(ggiraphExtra)
library(tibble)
library(ggplot2)
install.packages('maps')
install.packages('mapproj')

X<-rnorm(1000)
X
hist(X)
hist(X, breaks =300, freq = F)
lines(density(X), col = 'blue')

str(USArrests)
View(USArrests)

head(USArrests)

crime<-rownames_to_column(USArrests, var = 'state')
crime$state<-tolower(crime$state)
str(crime)

states_map<- map_data("state")
str(states_map)

ggChoropleth(data = crime, 
             aes(fill = Murder, map_id = state),
             map = states_map, 
             insteractive = T)



remove.packages(c("ggiraphExtra", "tibble", "ggplot2"))
