# Installing all required packages 
# install.packages("tidyverse")
# install.packages("dummy")
# install.packages("corrplot")
# install.packages("smotefamily")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("class")
# install.packages("neuralnet")

# Loading all the required packages
library(tidyverse)
library(dummy)
library(corrplot)
library(smotefamily)
library(e1071)
library(rpart)
library(rpart.plot)
library(class)
library(neuralnet)

# Setting the working directory
setwd("C:/Users/ual-laptop/Documents/545_DataMining/Coding/Project")
getwd()

# Read the diabetesData.csv into a tibble called diabetesData
diabetesData <- read_csv(file = "diabetesdata.csv",
                         col_types = "nnnnnnnnl",
                         col_names = TRUE)

# Display diabetesData in the console
print(diabetesData)

# Display the structure of diabetesData in the console
str(diabetesData)

# Display the summary of diabetesData in the console
summary(diabetesData)

# Some features in the dataset such as Glucose, BloodPressure, SkinThickness, 
# Insulin, BMI contain 0s. These are incorrect for that feature and 
# indicate missing value for that column.Converting such values to 'NA'
diabetesData['Glucose'][diabetesData['Glucose'] == 0] <- NA 
diabetesData['BloodPressure'][diabetesData['BloodPressure'] == 0] <- NA 
diabetesData['SkinThickness'][diabetesData['SkinThickness'] == 0] <- NA 
diabetesData['Insulin'][diabetesData['Insulin'] == 0] <- NA 
diabetesData['BMI'][diabetesData['BMI'] == 0] <- NA 

# Display the summary of diabetesData in the console
summary(diabetesData)

# Replacing missing values with their mean. (Mean imputation)
diabetesData <- diabetesData %>%
  mutate(Glucose = ifelse(is.na(Glucose), mean(Glucose, na.rm = TRUE), Glucose),
         BloodPressure = ifelse(is.na(BloodPressure), 
                                mean(BloodPressure, na.rm = TRUE), 
                                BloodPressure),
         SkinThickness = ifelse(is.na(SkinThickness), 
                                mean(SkinThickness, na.rm = TRUE), 
                                SkinThickness),
         Insulin = ifelse(is.na(Insulin), mean(Insulin, na.rm = TRUE), Insulin),
         BMI = ifelse(is.na(BMI), mean(BMI, na.rm = TRUE), BMI))


# Creating a function to display boxplots for all variables
displayAllBoxplots <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_boxplot(mapping = aes(x = value), color = "#0C234B",
                            fill = "#AB0520")  +
    facet_wrap (~ key, scales = "free") +
    theme_minimal()
}

# Diplaying all Boxplots to check for presence of outliers
displayAllBoxplots(diabetesData)

# Interesting Query 1: Calculating Average BMI and Age
diabetesDataAverageBmiAge <- filter(.data = diabetesData,Outcome == TRUE) %>%
  summarize(AverageAge = mean(Age), AverageBMI = mean(BMI))

diabetesDataAverageBmiAge

# Interesting Query 2: Finding average Glucose value for diabetic and
# non-diabetic patient
diabetesDataMeanGlucose <- diabetesData %>%
  group_by(Outcome) %>%
  summarize(meanGlucose = mean(Glucose))

diabetesDataMeanGlucose

# Interesting Query 3: Finding count of each outcome for each pregnancy value
diabetesDataPregnancies <- diabetesData %>%
  select(Pregnancies, Outcome) %>%
  group_by(Pregnancies, Outcome) %>% count()

diabetesDataPregnancies

# Outliers exist in our dataset which we are not removing. However, we are
# maintaining separate tibbles where we store these outliers for future research
# Pregnancies Outliers
outlierMin <- quantile(diabetesData$Pregnancies, 0.25) -
  (IQR(diabetesData$Pregnancies) * 1.5)
outlierMax <- quantile(diabetesData$Pregnancies, 0.75) +
  (IQR(diabetesData$Pregnancies) * 1.5)
print(paste (outlierMin, outlierMax))

pregnanciesOutliers <- diabetesData %>%
  filter(Pregnancies < outlierMin | Pregnancies > outlierMax)

# BloodPressure Outliers
outlierMin <- quantile(diabetesData$BloodPressure, 0.25) -
  (IQR(diabetesData$BloodPressure) * 1.5)
outlierMax <- quantile(diabetesData$BloodPressure, 0.75) +
  (IQR(diabetesData$BloodPressure) * 1.5)
print(paste (outlierMin, outlierMax))

bloodPressureOutliers <- diabetesData %>%
  filter(BloodPressure < outlierMin | BloodPressure > outlierMax)

# BMI Outliers
outlierMin <- quantile(diabetesData$BMI, 0.25) -
  (IQR(diabetesData$BMI) * 1.5)
outlierMax <- quantile(diabetesData$BMI, 0.75) +
  (IQR(diabetesData$BMI) * 1.5)
print(paste (outlierMin, outlierMax))

BMIOutliers <- diabetesData %>%
  filter(BMI < outlierMin | BMI > outlierMax)

# DiabetesPedigreeFunction Outliers
outlierMin <- quantile(diabetesData$DiabetesPedigreeFunction, 0.25) -
  (IQR(diabetesData$DiabetesPedigreeFunction) * 1.5)
outlierMax <- quantile(diabetesData$DiabetesPedigreeFunction, 0.75) +
  (IQR(diabetesData$DiabetesPedigreeFunction) * 1.5)
print(paste (outlierMin, outlierMax))

DiabetesPedigreeFunctionOutliers <- diabetesData %>%
  filter(DiabetesPedigreeFunction < outlierMin | DiabetesPedigreeFunction > outlierMax)

# Insulin Outliers
outlierMin <- quantile(diabetesData$Insulin, 0.25) -
  (IQR(diabetesData$Insulin) * 1.5)
outlierMax <- quantile(diabetesData$Insulin, 0.75) +
  (IQR(diabetesData$Insulin) * 1.5)
print(paste (outlierMin, outlierMax))

InsulinOutliers <- diabetesData %>%
  filter(Insulin < outlierMin | Insulin > outlierMax)

# SkinThickness Outliers
outlierMin <- quantile(diabetesData$SkinThickness, 0.25) -
  (IQR(diabetesData$SkinThickness) * 1.5)
outlierMax <- quantile(diabetesData$SkinThickness, 0.75) +
  (IQR(diabetesData$SkinThickness) * 1.5)
print(paste (outlierMin, outlierMax))

SkinThicknessOutliers <- diabetesData %>%
  filter(SkinThickness < outlierMin | SkinThickness > outlierMax)

# Creating a function to display histograms for all variables
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal()
}

# Displaying all Histograms
displayAllHistograms(diabetesData)

# Display a correlation matrix of diabetesData rounded to two decimal places
round(cor(diabetesData),2)

# Display a correlation plot using the ＂number＂ method and
# limit output to the bottom left
corrplot(cor(diabetesData),
         method = "number", 
         type = "lower")

# Normalizing data using min-max normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

diabetesData <- diabetesData %>%
  mutate(Pregnancies = normalize(Pregnancies)) %>%
  mutate(Glucose = normalize(Glucose)) %>%
  mutate(BloodPressure = normalize(BloodPressure)) %>%
  mutate(SkinThickness = normalize(SkinThickness)) %>%
  mutate(Insulin = normalize(Insulin)) %>%
  mutate(BMI = normalize(BMI)) %>%
  mutate(DiabetesPedigreeFunction = normalize(DiabetesPedigreeFunction)) %>%
  mutate(Age = normalize(Age))


# Splitting data set in training and testing data set
set.seed(1234)

sampleSet <- sample(nrow(diabetesData),
                    round(nrow(diabetesData) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into diabetesDataTraining 
diabetesDataTraining <- diabetesData[sampleSet,]

# Put the remaining 25% of records into diabetesDataTesting
diabetesDataTesting <- diabetesData[-sampleSet,]

# Logistic Regression Model Creation
# Do we have class imbalance
summary(diabetesDataTraining$Outcome)

# Store class imbalance magnitutde
classImbalanceMagnitude <- 383/193

# SMOTE
diabetesDataTrainingGlmSmoted <- 
  tibble(SMOTE(X = data.frame(diabetesDataTraining),
               target = diabetesDataTraining$Outcome,
               dup_size = 3)$data)

summary(diabetesDataTrainingGlmSmoted)

# Converting Outcome back to logical datatype
diabetesDataTrainingGlmSmoted <- diabetesDataTrainingGlmSmoted %>%
  mutate(Outcome = as.logical(Outcome))

# Get rid of "class" column in tibble
diabetesDataTrainingGlmSmoted <- diabetesDataTrainingGlmSmoted %>%
  select(-class)

# Check on class imbalance on the smoted dataset
summary(diabetesDataTrainingGlmSmoted)

# Logistic Regression model creation
diabetesDataGlmModel <- glm(data = diabetesDataTrainingGlmSmoted,
                            family = binomial,
                            formula = Outcome ~ .)

# Display output of the logistic regression model.
summary(diabetesDataGlmModel)

# The significant variables in our dataset is Pregnancies, Glucose, 
# BloodPressure, BMI, DiabetesPedigreeFunction

# Calculating odds ratio for each coefficient. Above 1 represents increase in
# independent variable will increase odds of increasing diabetes
exp(coef(diabetesDataGlmModel)['Pregnancies'])
exp(coef(diabetesDataGlmModel)['Glucose'])
exp(coef(diabetesDataGlmModel)['BloodPressure'])
exp(coef(diabetesDataGlmModel)['SkinThickness'])
exp(coef(diabetesDataGlmModel)['Insulin'])
exp(coef(diabetesDataGlmModel)['BMI'])
exp(coef(diabetesDataGlmModel)['DiabetesPedigreeFunction'])
exp(coef(diabetesDataGlmModel)['Age'])

# Use model to predict outcomes in the testing dataset
diabetesDataGlmPrediction <- predict(diabetesDataGlmModel,
                                     diabetesDataTesting,
                                     type = "response")

# Display diabetesDataPrediction on the console
print(diabetesDataGlmPrediction)

# Treat anything below or equal to 0.5 as 0, anything above 0.5 as 1
diabetesDataGlmPrediction <- 
  ifelse(diabetesDataGlmPrediction >= 0.5, 1, 0)

# Display diabetesDataPrediction on the console
print(diabetesDataGlmPrediction)

# Create confusion matrix 
diabetesDataGlmConfusionMatrix <- table(diabetesDataTesting$Outcome,
                                        diabetesDataGlmPrediction)

# Display confusion matrix 
print(diabetesDataGlmConfusionMatrix)

# Calculate false positive rate 
diabetesDataGlmConfusionMatrix[1, 2] /
  (diabetesDataGlmConfusionMatrix[1, 2] + 
     diabetesDataGlmConfusionMatrix[1, 1])

# Calculate false negative rate 
diabetesDataGlmConfusionMatrix[2, 1] /
  (diabetesDataGlmConfusionMatrix[2, 1] + 
     diabetesDataGlmConfusionMatrix[2, 2])

# Calculate prediction accuracy 
sum(diag(diabetesDataGlmConfusionMatrix) / nrow(diabetesDataTesting))

# K Nearest Neighbors Model Creation
# Serparating label and other variables
diabetesDataLabelsKnn <- diabetesData %>%
  select(Outcome)

diabetesDataKnn <- diabetesData %>%
  select(-Outcome)

# Splitting dataset in training and testing dataset
set.seed(1234)

sampleSetKnn <- sample(nrow(diabetesDataKnn),
                       round(nrow(diabetesDataKnn) * 0.75),
                       replace = FALSE)

# Put the records from the 75% sample into diabetesDataTrainingKnn and 
# diabetesDataTrainingLabelsKnn
diabetesDataTrainingKnn <- diabetesDataKnn[sampleSetKnn,]
diabetesDataTrainingLabelsKnn <- diabetesDataLabelsKnn[sampleSetKnn,]

# Put the records from the 25% sample into diabetesDataTrainingKnn and 
# diabetesDataTrainingLabelsKnn
diabetesDataTestingKnn <- diabetesDataKnn[-sampleSetKnn,]
diabetesDataTestingLabelsKnn <- diabetesDataLabelsKnn[-sampleSetKnn,]

# Generate the k-nearest neighbors model 
diabetesDataKnnPrediction <- knn(train = diabetesDataTrainingKnn,
                                 test = diabetesDataTestingKnn,
                                 cl = diabetesDataTrainingLabelsKnn$Outcome,
                                 k = 24)

# Display the predictions from the testing dataset on the console
print(diabetesDataKnnPrediction)

# Display summary of the predictions from the testing dataset
print(summary(diabetesDataKnnPrediction))

# Evaluate the model by forming a confusion matrix
diabetesDataKnnConfusionMatrix <- table(diabetesDataTestingLabelsKnn$Outcome,
                                        diabetesDataKnnPrediction)

# Display the confusion matrix on the console
print(diabetesDataKnnConfusionMatrix)

# Calculate the model predictive accuracy and store into predictiveAccuracy
predictiveAccuracyKnn <- sum(diag(diabetesDataKnnConfusionMatrix)) / 
  nrow(diabetesDataTestingLabelsKnn)

# Display the predictive accuracy on the console
print(predictiveAccuracyKnn)

# Create a matrix of k-values with their predictive accuracy
# Store the matrix into an object called kValueMatrix.
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names "k value" and "Predictive accuracy" to the kValueMatrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Loop through odd values of k from 1 up to the number of records in the 
# training dataset. With each pass through the loop, store the k-value along 
# with its predictive accuracy
for (kValue in 1:499) {
  
  # Only calculate predictive accuracy if k value is odd
  if(kValue %% 2 != 0) {
    
    # Generate the model
    diabetesDataKnnPrediction <- knn(train = diabetesDataTrainingKnn,
                                     test = diabetesDataTestingKnn,
                                     cl = diabetesDataTrainingLabelsKnn$Outcome,
                                     k = kValue)
    
    # Generate the confusion matrix
    diabetesDataKnnConfusionMatrix <- 
      table(diabetesDataTestingLabelsKnn$Outcome, 
            diabetesDataKnnPrediction)
    
    # Calculate the predictive accuracy
    predictiveAccuracyKnn <- sum(diag(diabetesDataKnnConfusionMatrix)) / 
      nrow(diabetesDataTestingKnn)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracyKnn))
  }
}

# Display the kValueMatrix on the console to determine the best k-value
print(kValueMatrix)

# With k =5, generate the k-nearest neighbors model 
diabetesDataKnnPrediction <- knn(train = diabetesDataTrainingKnn,
                                 test = diabetesDataTestingKnn,
                                 cl = diabetesDataTrainingLabelsKnn$Outcome,
                                 k = 5)

# Display the predictions from the testing dataset on the console
print(diabetesDataKnnPrediction)

# Display summary of the predictions from the testing dataset
print(summary(diabetesDataKnnPrediction))

# Evaluate the model by forming a confusion matrix
diabetesDataKnnConfusionMatrix <- table(diabetesDataTestingLabelsKnn$Outcome,
                                        diabetesDataKnnPrediction)

# Display the confusion matrix on the console
print(diabetesDataKnnConfusionMatrix)

# Calculate false positive rate 
diabetesDataKnnConfusionMatrix[1, 2] /
  (diabetesDataKnnConfusionMatrix[1, 2] + 
     diabetesDataKnnConfusionMatrix[1, 1])

# Calculate false negative rate 
diabetesDataKnnConfusionMatrix[2, 1] /
  (diabetesDataKnnConfusionMatrix[2, 1] + 
     diabetesDataKnnConfusionMatrix[2, 2])


# Calculate the model predictive accuracy and store into predictiveAccuracy
predictiveAccuracyKnn <- sum(diag(diabetesDataKnnConfusionMatrix)) / 
  nrow(diabetesDataTestingLabelsKnn)

# Display the predictive accuracy on the console
print(predictiveAccuracyKnn)

# Naive Bayes Model Creation
# Train the naive Bayes model
diabetesDataNbModel <- naiveBayes(formula = Outcome ~ .,
                                  data = diabetesDataTraining,
                                  laplace = 1)

# Build probabilities for each record in the testing dataset
diabetesDataNbProbability <- predict(diabetesDataNbModel,
                                     diabetesDataTesting,
                                     type = "raw")

# Display probabilities on the console
print(diabetesDataNbProbability)

# Predict classes for each record in the testing dataset 
diabetesDataNbPrediction <- predict(diabetesDataNbModel,
                                    diabetesDataTesting,
                                    type = "class")

# Display prediction on the console
print(diabetesDataNbPrediction)

# Evaluate the model by forming a confusion matrix
diabetesDataNbConfusionMatrix <- table(diabetesDataTesting$Outcome,
                                       diabetesDataNbPrediction)

# Display confusion matrix on the console
print(diabetesDataNbConfusionMatrix)

# Calculate false positive rate 
diabetesDataNbConfusionMatrix[1, 2] /
  (diabetesDataNbConfusionMatrix[1, 2] + 
     diabetesDataNbConfusionMatrix[1, 1])

# Calculate false negative rate 
diabetesDataNbConfusionMatrix[2, 1] /
  (diabetesDataNbConfusionMatrix[2, 1] + 
     diabetesDataNbConfusionMatrix[2, 2])


# Calculate the model predictive accuracy 
predictiveAccuracyNb <- sum(diag(diabetesDataNbConfusionMatrix)) / 
  nrow(diabetesDataTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyNb)

# Decision Tree model creation
# Train the decision tree model using the training dataset. 
# Note the complexity parameter of 0.01 is the default value
diabetesDecisonTreeModel <- rpart(formula = Outcome ~ .,
                                  method = "class",
                                  cp = 0.01,
                                  data = diabetesDataTraining)

# Display the decision tree plot
rpart.plot(diabetesDecisonTreeModel)


# Predict classes for each record in the testing dataset
diabetesDecisonTreePrediction <- predict(diabetesDecisonTreeModel,
                                         diabetesDataTesting,
                                         type = "class")

# Display the predictions from diabetesPrediction on the console
print(diabetesDecisonTreePrediction)


# Evaluate the model by forming a confusion matrix 
diabetesDecisonTreeConfusionMatrix <- table(diabetesDataTesting$Outcome,
                                            diabetesDecisonTreePrediction)

# Display the Confusion Matrix on the console
print(diabetesDecisonTreeConfusionMatrix)

# Calculate false positive rate 
diabetesDecisonTreeConfusionMatrix[1, 2] /
  (diabetesDecisonTreeConfusionMatrix[1, 2] + 
     diabetesDecisonTreeConfusionMatrix[1, 1])

# Calculate false negative rate 
diabetesDecisonTreeConfusionMatrix[2, 1] /
  (diabetesDecisonTreeConfusionMatrix[2, 1] + 
     diabetesDecisonTreeConfusionMatrix[2, 2])

# Calculate the model predictive accuracy
predictiveAccuracyDecisonTree <- sum(diag(diabetesDecisonTreeConfusionMatrix)) /
  nrow(diabetesDataTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyDecisonTree)

# Neural Networks Model Creation
# Generate the neural network model to predict Diabetes 
diabetesDataNeuralNet <- neuralnet(
  formula = Outcome ~.,
  data = diabetesDataTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
) 

# Display the neural network numeric results
print(diabetesDataNeuralNet$result.matrix)

# Visualize the neural network
plot(diabetesDataNeuralNet)

# Use diabetesDataNeuralNet to generate probabilities 
diabetesDataNeuralNetProbability <- compute(diabetesDataNeuralNet,
                                            diabetesDataTesting)

# Display the probabilities from the testing dataset on the console
print(diabetesDataNeuralNetProbability$net.result)

# Convert probability predictions into 0/1 predictions 
diabetesDataNeuralNetPrediction <-
  ifelse(diabetesDataNeuralNetProbability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(diabetesDataNeuralNetPrediction)

# Evaluate the model by forming a confusion matrix
diabetesDataNeuralNetConfusionMatrix <- table(diabetesDataTesting$Outcome,
                                              diabetesDataNeuralNetPrediction)

# Display the confusion matrix on the console
print(diabetesDataNeuralNetConfusionMatrix)

# Calculate false positive rate 
diabetesDataNeuralNetConfusionMatrix[1, 2] /
  (diabetesDataNeuralNetConfusionMatrix[1, 2] + 
     diabetesDataNeuralNetConfusionMatrix[1, 1])

# Calculate false negative rate 
diabetesDataNeuralNetConfusionMatrix[2, 1] /
  (diabetesDataNeuralNetConfusionMatrix[2, 1] + 
     diabetesDataNeuralNetConfusionMatrix[2, 2])

# Calculate the model predictive accuracy
predictiveAccuracyNeuralNet <- sum(diag(diabetesDataNeuralNetConfusionMatrix)) /
  nrow(diabetesDataTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyNeuralNet)

