# loading the dataset in the environment:
dataset <- read.csv("Tesla_stock_Price.csv")
View(dataset)
# Data pre processing and removing the unwanted items
dataset$Chg. <- gsub("[-]", "", dataset$Chg.)
View(dataset)
dataset$Chg.<- gsub("%", "", dataset$Chg.)
View(dataset)
dataset$Date <- gsub("[-]", "/", dataset$Date)
View(dataset)
dataset$Volume <- gsub("M", "", dataset$Volume)
View(dataset)
# Data Visualization using ggplots to know better about insights
library(ggplot2)
library(dplyr)
#comparing the price over the market is high
ggplot(data = dataset, aes(x = Price, y = High))+
  geom_bar(stat = "identity", fill = 'black')+
  labs(x = "Price", y = "High", title = "COMPRING THE PRICE OVER MARKET HIGH")
# comparing with barGraph
ggplot(dataset, aes(x = Price))+
  geom_histogram(binwidth = 5, color= "black", fill = "blue")
#comparing with lineplot
ggplot(data = dataset, aes(x = Price, y = High))+
  geom_line(color = "black", size = 1)+
  labs(x = "date", y = "Expenses", title = "TOTAL EXPENSES")
# creating the MachineLearning MOdel regrading linear regression
dataset2 <- dataset
View(dataset2)
dataset <- dataset2[,!names(dataset2) %in% c("Date", "Open", "Low", "Volume", "Chg.")]
View(dataset)
str(dataset)
price_mar=dataset[,2]
high_mar=dataset[,1]
library(caTools)
# generating random numbers
set.seed(100)


#data splitting
split=sample.split(dataset$High,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
View(training_set)
testing_set=subset(dataset,split==FALSE)
View(testing_set)
# applying regression
regressor=lm(formula = price_mar~high_mar,
             data=dataset)

#prediction
y_pred=predict(regressor,newdata=testing_set)

View(y_pred)

library(ggplot2)

#
plot(testing_set$Price,testing_set$High,
     type="p",col="blue" ,xlab="price",
     ylab="high")
lines(testing_set$Price,y_pred,type="o",
      col="red")
ggplot(testing_set, aes(x = h))+
  geom_point(aes(y = High, color = 'Actual'))+
  geom_line(aes(y = y_pred, color =  "Predicted"))


