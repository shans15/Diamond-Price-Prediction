library(readxl)
library(dplyr)
library(ggplot2)


# 1(a)
diamonds <- data.frame(read_excel("C:/Users/sarth/Downloads/Case_Study_HW4_Spreadsheet.xls"))
View(Case_Study_HW4_Spreadsheet)

str(diamonds)

# 1(b)
diamonds <- na.omit(diamonds)
#Check Correlations
cor(diamonds$ID, diamonds$Price)
cor(diamonds$Carat.Weight, diamonds$Price)
#plot(x = diamonds$Carat.Weight, y = diamonds$Price)
ggplot(diamonds, aes(x = Carat.Weight, y = Price)) + 
  geom_point() + 
  labs(x = "Carat Weight", y = "Price") + 
  ggtitle("Correlation between Carat Weight and Price") +  geom_smooth(method = "lm")

#Check group means
diamonds %>% group_by(Cut) %>% summarize(avg_Price = mean(Price), freq = n())
boxplot(diamonds$Price ~ diamonds$Cut)
diamonds %>% group_by(Color) %>% summarize(avg_Price = mean(Price), freq = n())
boxplot(diamonds$Price ~ diamonds$Color)
diamonds %>% group_by(Clarity) %>% summarize(avg_Price = mean(Price), freq = n())
boxplot(diamonds$Price ~ diamonds$Clarity)
diamonds %>% group_by(Polish) %>% summarize(avg_Price = mean(Price), freq = n())
boxplot(diamonds$Price ~ diamonds$Polish)
diamonds %>% group_by(Symmetry) %>% summarize(avg_Price = mean(Price), freq = n())
boxplot(diamonds$Price ~ diamonds$Symmetry)
diamonds %>% group_by(Report) %>% summarize(avg_Price = mean(Price), freq = n())
boxplot(diamonds$Price ~ diamonds$Report)

model <- lm(Price ~ factor(Cut) + factor(Color) + factor(Clarity) + factor(Polish) + 
              factor(Symmetry) + factor(Report), data = diamonds)
summary(model)

# # calculate the interquartile range (IQR) for Price
# Q1 <- quantile(diamonds$Price, 0.25, na.rm = TRUE)
# Q3 <- quantile(diamonds$Price, 0.75, na.rm = TRUE)
# IQR <- Q3 - Q1
# 
# # identify values below Q1 - 1.5*IQR or above Q3 + 1.5*IQR
# outliers <- diamonds$Price < (Q1 - 1.5*IQR) | diamonds$Price > (Q3 + 1.5*IQR)
# 
# # remove the outliers from the diamonds dataset
# diamonds <- diamonds[!outliers, ]
# 
# summary(diamonds)

#1(c)

# Changing the data types
diamonds$Cut <- as.factor(diamonds$Cut)
diamonds$Color <- as.factor(diamonds$Color)
diamonds$Clarity <- as.factor(diamonds$Clarity)
diamonds$Polish <- as.factor(diamonds$Polish)
diamonds$Symmetry <- as.factor(diamonds$Symmetry)
diamonds$Report <- as.factor(diamonds$Report)

# set seed for reproducibility
set.seed(123) 
train_index <- sample(2, nrow(diamonds), prob = c(0.7,0.3), replace = T)

train <- diamonds[train_index == 1, ]
test <- diamonds[train_index == 2, ]

# Print the summary of the outcome variable in both train and test data
cat("Summary of Price variable in train data:\n")
summary(train$Price)

cat("\nSummary of Price variable in test data:\n")
summary(test$Price)

# 1(d)
#Train a linear regression model
linereg_model <- lm(formula = Price ~ ., data = train)

# 1(e)
#Check summary of significant variables and R^2
summary(linereg_model)

# 1(f)
# MAPE of training data

linreg_trainpreds <- linereg_model$fitted.values
error <- linreg_trainpreds - train$Price
absError <- abs(error)
percError <- absError / train$Price
mape <- mean(percError)


# 1(g)
# Predict the prices of test data using the linear regression model
linreg_testpreds <- predict(linereg_model, newdata = test)

# Calculate the difference between predicted and actual values
diff <- linreg_testpreds - test$Price

# Calculate the magnitude of overprediction (absolute difference)
abs_diff <- abs(diff)

# Find the index of the most overpriced diamond
most_overpriced <- which.max(abs_diff)

# Print the row of the most overpriced diamond
cat("The most overpriced diamond is:\n")
print(test[most_overpriced, ])

# 1(h)
linreg_testpreds <- predict(linereg_model, test)
errorTest <- linreg_testpreds - test$Price
absErrorTest <- abs(errorTest)
percErrorTest <- absErrorTest / test$Price
mapeTest <- mean(percErrorTest)


# 1(i)
# Filter training dataset to include only diamonds with a price less than or equal to $12000
affordable_diamonds <- subset(train, Price <= 12000)

# Sort by carat weight in descending order
affordable_diamonds <- affordable_diamonds[order(affordable_diamonds$Carat.Weight, decreasing = TRUE),]

# Select the diamond with the highest carat weight from the filtered dataset as our recommendation
recommended_diamond <- affordable_diamonds[1,]

# Print recommended diamond information
cat("Based on Greg's preferences, we recommend the following diamond:\n")
print(recommended_diamond)

#2(a)
library(dplyr)
library(fastDummies)
library(neuralnet)
library(readxl)

diamonds <- data.frame(read_excel("C:/Users/sarth/Downloads/Case_Study_HW4_Spreadsheet.xls"))
diamonds <- na.omit(diamonds)

diamonds$Cut <- as.factor(diamonds$Cut)
diamonds$Color <- as.factor(diamonds$Color)
diamonds$Clarity <- as.factor(diamonds$Clarity)
diamonds$Polish <- as.factor(diamonds$Polish)
diamonds$Symmetry <- as.factor(diamonds$Symmetry)
diamonds$Report <- as.factor(diamonds$Report)

set.seed(123)
train_index <- sample(2, 6000, prob = c(0.7, 0.3), replace = T)

diamonds2 <- fastDummies:: dummy_cols(diamonds)
diamonds2 <- diamonds2 %>% select(-Cut, -Color, -Clarity, -Polish, -Symmetry, -Report)
diamonds2 <- diamonds2 %>% select(Price, ID:Carat.Weight, Cut_Fair:Report_GIA)

mins <- apply(diamonds2, 2, min)
maxs <- apply(diamonds2, 2, max)

diamonds3 <- scale(diamonds2, mins, maxs-mins)

train_nn <- diamonds3[train_index == 1, ]
test_nn <- diamonds3[train_index == 2, ]

library(nnet)
nn_model = nnet(Price ~ ., data = train_nn, linout = F, size = 25, decay = 0.01, maxit = 1000)
train_nn_preds <- nn_model$fitted.values
train_nn_preds <- train_nn_preds * (maxs[1] - mins[1]) + mins[1]

test_nn_preds = predict(nn_model, test_nn)
test_nn_preds = test_nn_preds * (maxs[1] - mins[1]) + mins[1]

#MAPE of training data
err <- train_nn_preds - diamonds[train_index == 1, ]$Price
abserr <- abs(err)
percabserr <- abserr / diamonds[train_index == 1, ]$Price
trainmape_nn <- mean(percabserr)

#MAPE of testing data
err <- test_nn_preds - diamonds[train_index == 2, ]$Price
abserr <- abs(err)
percabserr <- abserr / diamonds[train_index == 2, ]$Price
testmape_nn <- mean(percabserr)

# 2(d)
# Add predicted prices to the training dataset
train_data <- diamonds[train_index == 1, ]
train_data$Predicted_Price <- train_nn_preds

# Filter diamonds with a predicted price less than or equal to $12,000
affordable_diamonds <- subset(train_data, Predicted_Price <= 12000)

# Find the diamond with the highest carat value
best_diamond <- affordable_diamonds[which.max(affordable_diamonds$Carat.Weight), ]

