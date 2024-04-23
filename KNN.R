#######################################
# CS 513 - B
# Orlando Osorio Garcia
# 20006482
# Using the knn methodology to develop a
# classification models for Mortality
######################################

# CLEARING ENVIRONMENT VARIABLES
rm(list=ls())

# IMPORTING THE REQUIRED LIBRARIES
library(class)
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



# DATA SET TRAIN - TEST SPLIT (70% train, 30% test)
index <- sample(1:nrow(dataSet), 0.7 * nrow(dataSet))

# Train set
trainData <- attr[index, ]
targetTrain <- dataSet$Mortality[index]

# Test set
testData <- attr[-index, ]
targetTest <- dataSet$Mortality[-index]



# IMPLEMENTATION OF KNN
# Function to calculate accuracy
calculateAccuracy <- function(clf, targetTest) {
  accuracy <- confusionMatrix(clf, targetTest)$overall["Accuracy"]
  return(round(accuracy * 100, 2))
}

# Evaluate knn for different k values
kValues <- c(3, 5, 10)

for (k in kValues) {
  # Run knn function
  clf <- knn(trainData, testData, cl = targetTrain, k = k)
  
  # Confusion Matrix
  conf_matrix <- confusionMatrix(clf, targetTest)$table
  print("Confusion Matrix:")
  print(conf_matrix)
  
  # Calculate and print accuracy
  accuracy <- calculateAccuracy(clf, targetTest)
  print(paste("Accuracy of model with k =", k, ":", accuracy, "%"))
  
  # Classification Report
  class_report <- confusionMatrix(clf, targetTest)$byClass
  print("Classification Report:")
  print(class_report)
  
  cat("\n")
}


