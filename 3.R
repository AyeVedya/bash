# PRACTICAL 03

install.packages('readxl')
library(readxl)
df = read_excel('Data_User_Modeling_Dataset_Hamdi Tolga KAHRAMAN (1).xlsx', sheet = 'Training_Data')
head(df)
dim(df)
str(df)
summary(df)

library(ggplot2)
ggplot(df, aes(x = UNS, fill = UNS)) + geom_bar()


install.packages('GGally')
library(GGally)
ggpairs(df, columns = 1:5, aes(col = UNS))


df1 = subset(df[, c('STG', 'PEG')])
df1 = as.matrix(df1)
maximumClusters = 10


scal = scale(df1)

wss = sapply(1:maximumClusters, function(k){
  kmeans(scal, k, nstart = 50, iter.max = 10)$tot.withinss
})

plot(1:maximumClusters, wss, type = 'b')
abline(v = 3, col = 'red')


km3 = kmeans(df1, 3, iter.max = 10)
km3
km3$withinss
km3$tot.withinss

df %>% ggplot(aes(STG,PEG, col = km3$cluster)) + geom_point()

km4 = kmeans(df1, 4, iter.max = 10)
km4
km4$withinss
km4$tot.withinss
km4$iter

km4$cluster = as.factor(km4$cluster)
df %>% ggplot(aes(STG, PEG, color = km4$cluster)) + geom_point() + ggtitle('K means clustering with k = 4') + xlab('Study Time (STG)') + ylab('Exam Performance(PEG)')

