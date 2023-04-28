# PRACTICAL 04 - CLASSIFICATION

car_df = read.csv('car_evaluation.csv', sep = ',', header = FALSE)
head(car_df)
str(car_df)
summary(car_df)
anyNA(car_df)


set.seed(3033)
library(caret)

intrain = createDataPartition(y = car_df$V7, p = 0.7, list = FALSE)
intrain
training = car_df[intrain,]
testing = car_df[-intrain,]


dim(training)
dim(testing)


trctrl = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
set.seed(3333)


dtree = train(V7~., data = training, method = 'rpart',
              parms = list(split = 'information'),
              trControl = trctrl, 
              tuneLength =10)
dtree

library(rpart.plot)
prp(dtree$finalModel, box.palette = 'Reds', tweak = 1.2)



testing[1,]
predict(dtree, newdata = testing[1,])


test_pred  = predict(dtree, newdata = testing)
test_pred


test_pred = factor(test_pred, levels = levels(testing$V7))
confusionMatrix(test_pred, testing$V7)

