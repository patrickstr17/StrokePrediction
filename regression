# Group 9 Project
# Professor = Anne VANHEMS
# Group 9 Member :
#   Patricia Fonseca GOMES
#   Ngoc Uyen PHUNG
#   Renzo RAUSCHENBERG
#   Evan Brahma Hughie AZHABUR

#--------------------------------Load packages--------------------------------
library(ISLR2)
library(dplyr)
library(ggplot2)
library(splines) # to use bs or ns functions
library(gam) # to use gam.plot or gam function
library(PRROC)

data <- read.csv('healthcare-dataset-stroke-data.csv')

#--------------------------------Structure data--------------------------------
dim(data)
str(data)
attach(data)
summary(data)

## Changing data type
data$gender <- as.factor(data$gender)
data$hypertension <- factor(data$hypertension, levels=c(0, 1), 
                            labels=c('No Hypertension', 'Hypertension'))
data$heart_disease <- factor(data$heart_disease, levels=c(0, 1), 
                             labels=c('No Heart Disease', 'Heart Disease'))
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$stroke <- as.factor(data$stroke)
data$bmi <- as.numeric(data$bmi)

#--------------------------------Data Cleaning--------------------------------
summary(data)

# dealing with NA-values from bmi column
sum(is.na(data)) # 201 NA

# Drop the NA
cleandata <- data[complete.cases(data),]

# deleting ID column
cleandata <- subset(cleandata, select = -id)

# drop row of gender = "other"
cleandata <- subset(cleandata, gender != "Other")
cleandata$gender <- factor(cleandata$gender, levels = c("Male", "Female"))

dim(cleandata)
View(cleandata)
# the final data: 4908 rows, 11 columns

#----------------------------Descriptive Statistics----------------------------

summary(cleandata)
str(cleandata)

##--------------------------Target variable--------------------------
par(mfrow = c(1, 1))
# Plot No Stroke and Stroke Distribution
plot(cleandata$stroke, col = "skyblue",
     ylab = 'Count',
     xlab = 'Stroke or not (0 = No, 1 = Yes)',
     main='Patients who suffered stroke')

## Category variables descriptive
categorical_variables <- c('gender', 
                           'hypertension', 
                           'heart_disease', 
                           'ever_married', 
                           'work_type', 
                           'Residence_type', 
                           'smoking_status')

for (var in categorical_variables) {
  cate_var <- ggplot(cleandata, aes_string(x = var, fill = var)) +
    geom_bar(stat="count", position = 'dodge') +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    ggtitle(paste('Descriptive of', var))
  print(cate_var)
}

##--------------------------Categorical variables--------------------------
# BMI vs stroke
ggplot(cleandata, aes(x = factor(stroke), y = bmi, fill = factor(stroke))) +
  geom_boxplot() +
  labs(title = 'Box plot of BMI by Stroke Status',
       x = 'Stroke Status (0 = No, 1 = Yes)',
       y = 'BMI') + 
  ggtitle(paste('BMI and Stroke'))

df_stroke_0 <- subset(cleandata, stroke == 0)
df_stroke_1 <- subset(cleandata, stroke == 1)

bmi<- ggplot() +
  geom_density(data = df_stroke_0, aes(x = bmi), 
               fill = "#ec756b", color = NA, alpha = 1) +
  geom_density(data = df_stroke_1, aes(x = bmi), 
               fill = "#4cbfc5", color = NA, alpha = 0.8) +
  labs(x = "BMI", 
       title = "BMI and Stroke Distribution") +
  theme_minimal()
print(bmi)

# Glocose vs stroke
ggplot(cleandata, aes(x = factor(stroke), y = avg_glucose_level, fill = factor(stroke))) +
  geom_boxplot() +
  labs(title = 'Box plot of Glucoso Level by Stroke Status',
       x = 'Stroke Status (0 = No, 1 = Yes)',
       y = 'Glucose Level') +
  ggtitle(paste('Glucose Level and Stroke'))

glu<- ggplot() +
  geom_density(data = df_stroke_0, aes(x = avg_glucose_level), 
               fill = "#ec756b", color = NA, alpha = 1) +
  geom_density(data = df_stroke_1, aes(x = avg_glucose_level), 
               fill = "#4cbfc5", color = NA, alpha = 0.8) +
  labs(x = "Average glucose level", 
       title = "Glucose and Stroke Distribution") +
  theme_minimal()
print(glu)

# Age vs stroke
age <- ggplot() +
  geom_density(data = df_stroke_0, aes(x = age), 
               fill = "#ec756b", color = NA, alpha = 1) +
  geom_density(data = df_stroke_1, aes(x = age), 
               fill = "#4cbfc5", color = NA, alpha = 0.8) +
  labs(x = "Age", 
       title = "Age and Stroke Distribution") +
  theme_minimal()
print(age)

# Plot smoking status vs stroke
ggplot(cleandata, aes(x = factor(stroke), y = smoking_status, fill = smoking_status)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Smoking Status Distribution')+
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle(paste('Smoking status and Stroke'))

# Categorical VS. stoke
categorical_columns <- c('gender', 
                         'ever_married',
                         'work_type',
                         'Residence_type',
                         'smoking_status')
par(mfrow = c(length(categorical_columns), 1), mar = c(4, 4, 2, 1))

for (col in categorical_columns) {
  # Create a ggplot
  p <- ggplot(cleandata, aes(x = cleandata[[col]], 
                             fill = factor(cleandata$stroke))) +
    geom_bar(position = 'dodge') +
    labs(title = paste('Countplot of', col, 'vs Stroke'),
         x = col,
         y = 'Count') +
    theme_minimal()
  print(p + facet_wrap(~., scales = 'free'))
}

# Histogram for Age
hist(cleandata$age, breaks = 30, col = 'skyblue', main = 'Age Distribution',
     xlab = 'Age', ylab = 'Frequency')

# Histogram for Average Glucose Level
hist(cleandata$avg_glucose_level, breaks = 30, col = 'salmon', main = 'Average Glucose Level Distribution',
     xlab = 'Average Glucose Level', ylab = 'Frequency')

# Histogram for BMI
hist(cleandata$bmi, breaks = 30, col = 'lightgreen', main = 'BMI Distribution',
     xlab = 'BMI', ylab = 'Frequency')

# bar plot for work_type
work_type_counts <- table(cleandata$work_type)
barplot(work_type_counts, main = 'Work Type Distribution', xlab = 'Work Type', ylab = 'Frequency', col = 'yellow')

#------------------------Model Preparation------------------------

# Create train and test data
set.seed(123)  # Set seed for reproducibility
# 80% for data training
train_indices <- sample(1:nrow(cleandata), 0.8 * nrow(cleandata))
train_data <- cleandata[train_indices, ]
test_data <- cleandata[-train_indices, ]


#------------------------Classical logistic regression------------------------


##--------------------------Logistic model building--------------------------
mod.1 <- glm(stroke~
               gender + age + hypertension + heart_disease + avg_glucose_level+
               bmi + smoking_status, family=binomial, data=cleandata)
summary(mod.1)

# Drop the gender, heart_disease, bmi, smoking_status since their pvalues > 0.5

mod.2 <- glm(stroke~
               age + hypertension + avg_glucose_level, family=binomial, 
             data=cleandata)

summary(mod.2)

# AIC: Akaike criteria = measure of the variance of the residuals
# Allows to compare between two models: the best is with the lowest value for the AIC
## AIC = 1386.4, lower than mod.1 (AIC = 1391.4) so it's good!

##------------------------Predictive power of the model------------------------

# Model training
mod_train <- glm(stroke ~ age + hypertension + avg_glucose_level, 
                 family = binomial, 
                 data = train_data)

# Prediction model
pred_test <- predict(mod_train, newdata = test_data, type = 'response')
head(pred_test)

#--------------------------Threshold value--------------------------

# ROC
roc_obj <- roc(test_data$stroke, pred_test)
plot(roc_obj, main="ROC Curve", col="#ec756b", xlab="False Positive", 
     ylab="Sensitivity")

# Find the threshold according to ROC
roc_coords <- coords(roc_obj, "best", 
                     ret=c("threshold", "sensitivity", "specificity"), 
                     best.method="youden")
threshold_roc <- roc_coords$threshold
youden_index <- roc_coords$sensitivity + roc_coords$specificity - 1

# Print the threshold ROC and Youden's index
print(paste("Threshold ROC:", threshold_roc)) # 0.037
print(paste("Youden's index:", youden_index)) # 0.619

# Convert predicted probabilities to binary predictions
pred_test <- ifelse(pred_test > 0.037, 1, 0)

# Accuracy calculation
misclass.err <- mean(pred_test != test_data$stroke)
misclass.err
print(paste('Prediction accuracy =', 1-misclass.err))
# accuracy = 73.93%

# Confusion matrix
conf_matrix <- table(test_data$stroke, pred_test)
print(conf_matrix)

# Calculate metrics
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- sensitivity

# Print metrics
print(paste("Sensitivity (True Positive Rate):", sensitivity)) # 88.63%
print(paste("Specificity (True Negative Rate):", specificity)) # 73.24%
print(paste("Precision (Positive Predictive Value):", precision)) # 13.44%
print(paste("Recall (Sensitivity):", recall)) # 88.63%



# Finding a better threshold
thresholds <- seq(0.005, 0.3, by = 0.005)
misclass.err <- rep(NA, length(thresholds))
false_negatives <- rep(NA, length(thresholds))
false_positives <- rep(NA, length(thresholds))
sensitivity <- rep(NA, length(thresholds))
specificity <- rep(NA, length(thresholds))
precision <- rep(NA, length(thresholds))
recall <- rep(NA, length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  
  # Train model
  mod_train <- glm(stroke ~ age + avg_glucose_level, 
                   family = binomial, 
                   data = train_data)
  
  # Make predictions
  pred_test <- predict(mod_train, newdata = test_data, type = 'response')
  
  # Binary predictions based on the current threshold
  pred_test_binary <- ifelse(pred_test > threshold, 1, 0)
  
  # Calculate errors
  misclass.err[i] <- mean(pred_test_binary != test_data$stroke)
  
  # Confusion Matrix
  conf_matrix <- table(test_data$stroke, pred_test_binary)
  
  # False Positives and False Negatives
  false_positives[i] <- conf_matrix[1, 2]
  false_negatives[i] <- conf_matrix[2, 1]
  
  # Calculate metrics
  sensitivity[i] <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  specificity[i] <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
  precision[i] <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall[i] <- sensitivity
}

# Create a dataframe with the values
results_df <- data.frame(FalsePositives = false_positives,
                         FalseNegatives = false_negatives,
                         Threshold = thresholds)

# Create a line chart
ggplot(results_df, aes(x = FalsePositives, y = FalseNegatives)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = sprintf("%.3f", Threshold)), vjust = -0.5) + # Textlabels für Threshold-Werte (3 Nachkommastellen)
  labs(x = "False Positives", y = "False Negatives", title = "False Positives vs. False Negatives") +
  theme_minimal()


# Create a dataframe with the values
results_df <- data.frame(Threshold = thresholds,
                         Sensitivity = sensitivity,
                         Specificity = specificity,
                         Precision = precision)

# Create a line chart
ggplot(results_df, aes(x = Threshold)) +
  geom_line(aes(y = Sensitivity, color = "Sensitivity"), linetype = "solid") +
  geom_line(aes(y = Specificity, color = "Specificity"), linetype = "solid") +
  geom_line(aes(y = Precision, color = "Precision"), linetype = "solid") +
  labs(x = "Threshold", y = "Value", title = "Sensitivity, Specificity, and Precision vs. Threshold") +
  scale_color_manual(values = c("Sensitivity" = "blue", "Specificity" = "red", "Precision" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Best model for our purpose: threshold = 0.01

# Try with threshold = 0.01
pred_test <- ifelse(pred_test > 0.01, 1, 0)

# Accuracy calculation
misclass.err <- mean(pred_test != test_data$stroke)
misclass.err
print(paste('Prediction accuracy =', 1-misclass.err))
# accuracy = 43.99%

# Confusion matrix
conf_matrix <- table(test_data$stroke, pred_test)
print(conf_matrix)

# Calculate metrics
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- sensitivity

# Print metrics
print(paste("Sensitivity (True Positive Rate):", sensitivity)) # 92.85%
print(paste("Specificity (True Negative Rate):", specificity)) # 41.80%
print(paste("Precision (Positive Predictive Value):", precision)) # 6.65%
print(paste("Recall (Sensitivity):", recall)) # 92.85%

#----------------------------GAM Logistic Regression---------------------------

##--------------------------GAM model building--------------------------
# GAM
mod.gam <- gam(stroke ~ s(age) + hypertension + s(avg_glucose_level), 
               family = binomial, data = cleandata)
summary(mod.gam)
# AIC: 1390.273
# Logistics model is preferred according to the AIC.

par(mfrow=c(1,3))
plot.Gam(mod.gam, se=TRUE, col="#ec756b")

##------------------------Predictive power of the model------------------------
mod.gamt <- gam(stroke ~ s(age) + hypertension + s(avg_glucose_level), 
                family = binomial,
                data=train_data)

# prediction on the test set
pred.gam <- predict(mod.gamt, newdata=test_data)
head(pred.gam)

test_data$stroke <- as.numeric(as.character(test_data$stroke))
# generate the PR curve
pr <- pr.curve(scores.class0 = pred.gam, 
               weights.class0 = test_data$stroke, 
               curve = TRUE)

plot(pr)

# calculate the area under the PR curve (AUCPR)
auc_pr <- pr$auc.integral
print(paste("Area under the PR curve:", auc_pr))

# prediction model threshold 0.037
pred.gam <- ifelse(pred.gam>0.037,1,0)
head(pred.gam)

misclass.err <- mean(pred.gam != test_data$stroke)
misclass.err
print(paste('Prediction accuracy =', 1-misclass.err))
# accuracy = 95.72%

# Confusion matrix
pred.gam <- factor(pred.gam, levels=c(0,1), 
                   labels=c("Predicted no stroke", "Predicted stroke"))
test_data$stroke <- factor(test_data$stroke, 
                           levels=c(0,1), 
                           labels=c("No stroke", "Stroke" ))

table(pred.gam, test_data$stroke)
# indicates that out of 982 predictions,
# 940 were correctly predicted as "No stroke" and 
# 42 were true "Stroke" cases that the model failed to predict (false negatives). 
# There were no false positives (where the model incorrectly predicted "Stroke").

prop.table(table(pred.gam, test_data$stroke),2)
