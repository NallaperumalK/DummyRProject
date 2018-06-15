# Template code
# Step 1: Build Logit Model on Training Dataset
#logitMod <- glm(Y ~ X1 + X2, family="binomial", data = trainingData)

# Step 2: Predict Y on Test Dataset
#predictedY <- predict(logitMod, testData, type="response") 



# Load data
install.packages('mlbench')
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # keep complete rows

bc
# remove id column
bc <- bc[,-1]

# convert to numeric
#we have to convert into character first then into numerica variables
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}

# Change Y values to 1's and 0's
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
bc

# Prep Training and Test data.
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)

?createDataPartition
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

# Class distribution of train data
table(trainData$Class)

#since sample size is 

# Down Sample
set.seed(100)
?downSample
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)


# Up Sample (optional)
set.seed(100)
?upSample
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

table(up_train$Class)

# Build Logistic Model
?glm
#family = "binomial" for logistic regression
#Let us take data from the down_sample first and then see for trainin data set.
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)
summary(logitmod)

pred <- predict(logitmod, newdata = testData, type = "response")
pred

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

# Accuracy
mean(y_pred == y_act)  # 94%


# Why handling with class imbalance is important?
# why we need to take care of class imbalance earlier.
# To understand that lets assume you have a dataset where 95% of the Y values belong to benign class and 5% belong to malignant class.
# 
# If we just blindly predicted all the data points as benign, I would achieve an accuracy percentage of 95%. Which sounds pretty high. But obviously that is flawed. What matters is how well you predict the malignant classes.
# 
# So that requires the benign and malignant classes are balanced AND on top of that I need more refined accuracy measures and model evaluation metrics to improve my prediction model*/