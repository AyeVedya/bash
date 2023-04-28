#Practical 8
PRSA_df=read.csv("PRSA_data_2010.1.1-2014.12.31.csv",na.strings = c("?","NA"))
str(PRSA_df)
View(PRSA_df)
library(DataExplorer)
plot_str(PRSA_df)
plot_missing(PRSA_df)
new_df1=na.omit(PRSA_df)
str(new_df1)
View(new_df1)                	
model_df = new_df1 %>% select(pm2.5, DEWP, TEMP, PRES, cbwd, Iws, Is, Ir)
model_df
set.seed(123)
training.samples <- model_df$pm2.5 %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- model_df[training.samples, ]
test.data <- model_df[-training.samples, ]
model <- train(pm2.5 ~ ., data = train.data, method = "lm")
predictions <- predict(model, test.data)


ggplot(test.data, aes(x = pm2.5, y = predictions)) +
  geom_point() +
  geom_abline(col='red',lwd=2) +
  xlab("Actual PM2.5 levels") +
  ylab("Predicted PM2.5 levels") +
  ggtitle("Regression Model for PM2.5 Levels")



