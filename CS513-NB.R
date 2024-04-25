#######################################
# CS 513 - B
# Orlando Osorio Garcia
# 20006482
# Using the Naive Bayes methodology
# to develop a classification model.
######################################

# CLEARING ENVIRONMENT VARIABLES
rm(list=ls())

# IMPORTING THE REQUIRED LIBRARIES
library(e1071)
library(caret)

# IMPORTING THE REQUIRED DATASET
dataSet <- read.csv("C:\\Users\\orlan\\Desktop\\CS 513 Project\\HeartFailure.csv", na.string = "?")



# DATA CLEANING AND PREPROCESSING
# Removing unnecessary columns
dataSet <- dataSet[, !(names(dataSet) %in% c("Others", "Family.History", "CO", "Diagnosis", "Life.Style", "Sleep", "Category", "Age.Group"))]

# Removing rows with missing values
dataSet <- na.omit(dataSet)

# Factoring data
dataSet$Mortality <- factor(dataSet$Mortality, levels = c("0", "1"), labels = c("Died", "Alive"))
dataSet$Gender <- as.integer(factor(dataSet$Gender, levels = c("Female", "Male")))
dataSet$Locality <- as.integer(factor(dataSet$Locality, levels = c("RURAL", "URBAN")))
dataSet$Marital.status <- as.integer(factor(dataSet$Marital.status, levels = c("SINGLE", "MARRIED")))
dataSet$Depression <- as.integer(factor(dataSet$Depression, levels = c("YES", "NO")))
dataSet$Hyperlipi <- as.integer(factor(dataSet$Hyperlipi, levels = c("YES", "NO")))
dataSet$Smoking <- as.integer(factor(dataSet$Smoking, levels = c("YES", "NO")))
dataSet$HTN <- as.integer(factor(dataSet$HTN, levels = c("YES", "NO")))
dataSet$Allergies <- as.integer(factor(dataSet$Allergies, levels = c("YES", "NO")))
dataSet$Hypersensitivity <- as.integer(factor(dataSet$Hypersensitivity, levels = c("YES", "NO")))
dataSet$SK.React <- as.integer(factor(dataSet$SK.React, levels = c("NO", "COUGH.BLEEDING", "SKIN.BLEEDING", "LUNGS", "BODY.PAIN", "NAUSEA.TEMP", "STOMACH.BLEEDING")))



# DATA NORMALIZATION
# Excluding 'Mortality' column
attr <- dataSet[, -ncol(dataSet) + 1]

# Normalize the features
numericColumns <- sapply(attr, is.numeric)
attrScaled <- scale(attr[, numericColumns])
attr[, numericColumns] <- as.data.frame(attrScaled)



# DATASET TRAIN - TEST SPLIT
index <- sample(1:nrow(attr), 0.7 * nrow(attr))

# Train set
trainData <- attr[index, ]
targetTrain <- dataSet$Mortality[index]

# Test set
testData <- attr[-index, ]
targetTest <- dataSet$Mortality[-index]



# IMPLEMENTATION OF NAIVE BAYES
nbModel <- naiveBayes(targetTrain ~ ., data = trainData)
nbPredict <- predict(nbModel, testData)

# Generate the confusion matrix
cm <- confusionMatrix(nbPredict, targetTest)

# Print the confusion matrix
print(cm)

# Accessing overall statistics
overallStats <- cm$overall
print(overallStats)

# Accessing class-specific statistics
classStats <- cm$byClass
print(classStats)

# Getting accuracy, precision, recall, and F1-Score
accuracy <- cm$overall['Accuracy']
precision <- cm$byClass['Precision']
recall <- cm$byClass['Recall']
F1 <- cm$byClass['F1']

# Printing metrics
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", F1))


