# PRACTICAL 05 - REGRESSION MODEL

library(readr)
wifi_train = read_csv('wifi_train_rm_outliers.csv')
head(wifi_train)
str(wifi_train)

wifi_train$room = as.factor(wifi_train$room)
wifi_test = read.csv('wifi_testfile.csv')


library(ggplot2)
nbplotter = function(i){
  a = ggplot(data = wifi_train, aes(unlist(wifi_train[, i]), fill = room)) + geom_density(alpha = 0.5)  + xlab(names(wifi_train)[i])
  return (a)
  
}
library(gridExtra)
grid.arrange(
  nbplotter(1),
  nbplotter(2),
  nbplotter(3),
  nbplotter(4),
  nbplotter(5),
  nbplotter(6),
  nrow = 3
)


library(rpart)
fit.rpart = rpart(room~., data = wifi_train)
install.packages('rattle')
library(rattle)
fancyRpartPlot(fit.rpart)


pred.train.rpart = predict(fit.rpart, newdata = wifi_train[, 1:7], type = 'class')
accuracy.train = mean(pred.train.rpart == wifi_train$room)
cat('Train Accuracy: ', accuracy.train)

pred.test.rpart = predict(fit.rpart, newdata = wifi_test[, 1:7], type = 'class')
accuracy.test = mean(pred.test.rpart == wifi_test$room)
cat('Test Accuracy: ', accuracy.test)

