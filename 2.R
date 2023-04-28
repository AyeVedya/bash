# PRACTICAL 02

classes = c('factor', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
data = read.csv('abalone.csv')
str(data)
summary(data)
levels(data$Sex)
table(data$Sex, data$Rings)
install.packages('dplyr')
library(dplyr)

data %>% group_by(Sex) %>% summarise(No_of_observations = n())
library(ggplot2)
ggplot(data = data, aes(x = Sex, fill = Sex)) + geom_bar()

cor(data$Length, data$Rings)
cor(data$Diameter, data$Rings)

install.packages('corrplot')
library(corrplot)
cor_mat = cor(data[, c(2:9)])
cor_mat

corrplot(cor_mat, type = 'upper', order = 'hclust', tl.col = 'black', tl.srt = 45)


heatmap(cor_mat, margins = c(10,10))

ggplot(data = data, aes(x = Shell.weight, y = Rings, col = Sex)) + geom_point() +geom_smooth(method = 'lm')
ggplot(data = data, aes(x = Length, y = Rings, col = Sex)) + geom_point() +geom_smooth(method = 'lm')


library(caTools)
cleaned = data %>% mutate(age = case_when(
  Rings %in% 1:5 ~ 'Young', 
  Rings %in% 6:13 ~ 'Adult',
  Rings %in% 14:30 ~ 'Old'
))

cleaned = cleaned %>% select(c(2,3,5,8,10)) %>% na.omit()
head(cleaned)

sample = sample.split(cleaned, SplitRatio = 0.8)
traindata = subset(cleaned, sample = T)
testdata = subset(cleaned, sample = F)


library(rpart)
model = rpart(formula = age~., data = traindata, method = 'class')
summary(model)

install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(model)

predicted = predict(object = model, newdata = traindata, type = 'class')
cm = table(traindata$age, predicted)
cm

accuracy = sum(diag(cm))/ sum(cm)
accuracy


