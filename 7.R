#Practical 7
echo_df=read.csv("echocardiogram.csv",na.strings = c("?","NA"))
str(echo_df)
View(echo_df)
library(DataExplorer)
plot_str(echo_df)
plot_missing(echo_df)
echo_df1=na.omit(echo_df)
str(echo_df1)
echo_df1$aliveat1=as.factor(echo_df1$aliveat1)
library(ggplot2)
library(rpart)
install.packages(rpart.plot)
library(rpart.plot) #Decision Tree Display
library(caret)
ggplot(echo_df1,aes(x=age,y=survival,color=aliveat1,size=age))+geom_point(alpha=0.9)
set.seed(111)
index=sample(1:nrow(echo_df1),nrow(echo_df1)*0.8)
training=echo_df1[index,]
testing=echo_df1[-index,]
dim(training)
dim(testing)
model_echo=rpart(aliveat1~age+pericardialeffusion+fractionalshortening+epss+lvdd+wallmotion.score+wallmotion.index,data = training,method = "class")
prediction=predict(model_echo,newdata = testing,type = "class")
confusionMatrix(testing$aliveat1,prediction)
rpart.plot(model_echo)

