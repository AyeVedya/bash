data = read.csv('Daily_Demand_Forecasting_Orders.csv',header = TRUE, sep = ';')
head(data)
dim(data)
attach(data)
plot(x = Urgent.order, y = Target..Total.orders., main = 'Urgent and Total Orders')
model = lm(Target..Total.orders. ~ Urgent.order, data = data)
summary(model)
abline(model, col = 'red')


new_data = data.frame(Urgent.oreder = c(100, 150, 200))
forecast = predict(model, newdata = new_data, interval = 'none', level = 0.95)
forecast  

set.seed(1234)
index = sample(1:nrow(data), nrow(data) * 0.7)
training = data[index,]
testing = data[-index,]
dim(training)
dim(testing)

new_model = lm(Target..Total.orders.~Urgent.order, data = training)
predicted = predict(new_model, newdata = testing, interva = 'none')
df = testing[, c('Urgent.order', 'Target..Total.orders.')]
df['predicted'] = predicted
df

