
#STAT 385 Final Project
#Authors: - Group 9

#Load CSV:

sleep_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv", header = TRUE)

### Basic Data and Variable Overview and Changes ###

head(sleep_data)
print(names(sleep_data))

#Since blood pressure is a string of systolic over diastolic, we should extract as change to numeric
print(str(sleep_data$Blood.Pressure))

#create empty cols 
sleep_data$Systolic_BP <- NA
sleep_data$Diastolic_BP <- NA

for(i in 1:nrow(sleep_data)) {
  # Get the blood pressure string
  bp_string <- sleep_data$Blood.Pressure[i]
  
  #apply func to split by "/" char
  bp_parts <- strsplit(bp_string, "/")[[1]]
  
  #change to numeric and populate cols
  sleep_data$Systolic_BP[i] <- as.numeric(bp_parts[1])
  sleep_data$Diastolic_BP[i] <- as.numeric(bp_parts[2])
}

#check
print(sleep_data$Systolic_BP)
print(sleep_data$Diastolic_BP)


#convert categorical variables to factors
sleep_data$Gender <- as.factor(sleep_data$Gender)
sleep_data$Occupation <- as.factor(sleep_data$Occupation)
sleep_data$BMI.Category <- as.factor(sleep_data$BMI.Category)
sleep_data$Sleep.Disorder <- as.factor(sleep_data$Sleep.Disorder)

#Check changes
print(str(sleep_data))


### Overall EDA ###

#Start with summary tables:

#numeric cols
numerical_vars <- sleep_data[, c("Age", "Sleep.Duration", "Quality.of.Sleep", 
                                 "Physical.Activity.Level", "Stress.Level", 
                                 "Heart.Rate", "Daily.Steps", "Systolic_BP", "Diastolic_BP")]
print(summary(numerical_vars))

#categorical cols
print(table(sleep_data$Gender))
print(table(sleep_data$BMI.Category))
print(table(sleep_data$Sleep.Disorder))

#Plots:
par(mfrow = c(2, 3))

# Histograms for key numerical variables
hist(sleep_data$Age, main = "Distribution of Age", xlab = "Age", col = "lightblue")
hist(sleep_data$Sleep.Duration, main = "Sleep Duration", xlab = "Hours", col = "lightgreen")
hist(sleep_data$Quality.of.Sleep, main = "Sleep Quality", xlab = "Rating (1-10)", col = "lightcoral")
hist(sleep_data$Stress.Level, main = "Stress Level", xlab = "Rating (1-10)", col = "lightyellow")
hist(sleep_data$Physical.Activity.Level, main = "Physical Activity", xlab = "Minutes/day", col = "lightpink")
hist(sleep_data$Systolic_BP, main = "Systolic Blood Pressure", xlab = "mmHg", col = "orange")
par(mfrow = c(1, 1))

### EDA for Investigating Sleep Disorder ###
par(mfrow = c(2, 2))
boxplot(Sleep.Duration ~ Sleep.Disorder, data = sleep_data, 
        main = "Sleep Duration by Disorder", col = c("lightgreen", "lightcoral", "lightblue"))
boxplot(Quality.of.Sleep ~ Sleep.Disorder, data = sleep_data, 
        main = "Sleep Quality by Disorder", col = c("lightgreen", "lightcoral", "lightblue"))
boxplot(Stress.Level ~ Sleep.Disorder, data = sleep_data, 
        main = "Stress Level by Disorder", col = c("lightgreen", "lightcoral", "lightblue"))
boxplot(Physical.Activity.Level ~ Sleep.Disorder, data = sleep_data, 
        main = "Physical Activity by Disorder", col = c("lightgreen", "lightcoral", "lightblue"))
par(mfrow = c(1, 1))


# Correlation matrix for numerical variables
cat("\nCorrelation Matrix for Numerical Variables:\n")
cor_matrix <- cor(numerical_vars)
print(round(cor_matrix, 3))

### Random Forest Model Process ###

# select model features
model_data <- sleep_data[, c("Gender", "Age", "Occupation", "Sleep.Duration", 
                             "Quality.of.Sleep", "Physical.Activity.Level", 
                             "Stress.Level", "BMI.Category", "Systolic_BP", 
                             "Diastolic_BP", "Heart.Rate", "Daily.Steps", 
                             "Sleep.Disorder")]

#split data into training and test sets (75% training, 25% testing)
set.seed(20251114)  #seed for reproductibility
alpha <- 0.75
inTrain <- sample(1:nrow(model_data), alpha * nrow(model_data))
train.set <- model_data[inTrain, ]
test.set <- model_data[-inTrain, ]

#check split
nrow(train.set) #280
nrow(test.set) #94

#Model:
library(randomForest)

#set variable to use for models with diff mtry vals
num_predictors<-12

# Train Random Forest with different mtry values
set.seed(123)
#value 1: mtry = sqrt(p)
rf_sqrt <- randomForest(Sleep.Disorder ~ ., data = train.set,
                        mtry = floor(sqrt(num_predictors)),
                        ntree = 300,
                        importance = TRUE)

#value 2: mtry = p/3  
rf_third <- randomForest(Sleep.Disorder ~ ., data = train.set,
                         mtry = floor(num_predictors/3),
                         ntree = 300, 
                         importance = TRUE)

#Compare Error Rates of both tested values
cat("mtry = sqrt(p):", rf_sqrt$err.rate[300, "OOB"], "\n")
cat("mtry = p/3:", rf_third$err.rate[300, "OOB"], "\n")

#both have 0.1 

# Plot OOB error rates
plot(rf_sqrt$err.rate[, "OOB"], type = "l", lwd = 2, col = "lightblue",
     main = "Random Forest: OOB Error Rate",
     xlab = "Number of Trees", ylab = "OOB Error Rate",
     ylim = range(c(rf_sqrt$err.rate[, "OOB"], rf_third$err.rate[, "OOB"])))
lines(rf_third$err.rate[, "OOB"], lwd = 2, col = "pink")
legend("topright", legend = c("m=sqrt(p)", "m=p/3"),
       col = c("lightblue", "pink"), lty = 1, lwd = 2)

#confusion matrix for sqrt and third
pred_sqrt <- predict(rf_sqrt, test.set)
pred_third <- predict(rf_third, test.set)
conf_matrix <- table(pred_sqrt, test.set$Sleep.Disorder)
print(conf_matrix)

# pred_sqrt     Insomnia None Sleep Apnea
# Insomnia          17    2           0
# None               3   48           2
# Sleep Apnea        0    0          22

### Tuning mtry Parameter ###

mtry_vals <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)
oob_errors <- numeric(length(mtry_vals))

for (i in 1:length(mtry_vals)) {
  set.seed(123)
  rf_temp <- randomForest(Sleep.Disorder ~ ., data = train.set, mtry = mtry_vals[i],ntree = 300)
  oob_errors[i] <- rf_temp$err.rate[300, "OOB"]
  cat("mtry =", mtry_vals[i], "OOB error =", round(oob_errors[i], 4), "\n")
}

#tuning results
tuning_results <- data.frame(mtry = mtry_vals, OOB_Error = oob_errors)
print(tuning_results)

#=best mtry value
best_mtry <- mtry_vals[which.min(oob_errors)]
#2 was the best mtry value

### Final Model with best mtry Value ###
set.seed(123)
final_rf <- randomForest(Sleep.Disorder ~ ., data = train.set,
                         mtry = best_mtry,
                         ntree = 300,
                         importance = TRUE)

#variable importance plot
varImpPlot(final_rf, main = "Random Forest: Variable Importance")

#predictions on test set
rf_predictions <- predict(final_rf, newdata = test.set)

#Confusion matrix and error rate
conf_matrix <- table(Predicted = rf_predictions, Actual = test.set$Sleep.Disorder)
print(conf_matrix)

# Predicted     Insomnia None Sleep Apnea
# Insomnia          17    0           0
# None               3   50           2
# Sleep Apnea        0    0          22

test_error <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
test_error
#0.05319149

#accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

#0.9468085

### LASSO MODEL ###

library("glmnet")

#convert data to matrix format for glmnet
xmat_train <- as.matrix(train.set[, !names(train.set) %in% c("Sleep.Disorder")])
y_train <- as.numeric(train.set$Sleep.Disorder)  # Convert factor to numeric (1,2,3)

xmat_test <- as.matrix(test.set[, !names(test.set) %in% c("Sleep.Disorder")])
y_test <- as.numeric(test.set$Sleep.Disorder)

#cv for lambda

set.seed(20251114) #seed for reproducibility

cvfit <- cv.glmnet(xmat_train, y_train, 
                   family = "multinomial",  # For 3-class classification
                   alpha = 1,               # alpha=1 for LASSO penalty
                   nfolds = 10)


lambest_min <- cvfit$lambda.min     # Lambda with minimum CV error
lambest_1se <- cvfit$lambda.1se 

#Best lambda (minimum CV error): 0.00573 
#Lambda: 0.03681 

# Plot cross-validation results
plot(cvfit, main = "LASSO: Cross-Validation Error vs Lambda")

#fit final model with best lambda

lasso_model <- glmnet(xmat_train, y_train,
                      family = "multinomial",
                      alpha = 1,
                      lambda = lambest_min)

# Extract coefficients
lasso_coef <- coef(lasso_model)

# Show coefficients for each class
class_names <- levels(train.set$Sleep.Disorder)
for(i in 1:length(lasso_coef)) {
  cat("\nClass:", class_names[i], "\n")
  
  # Get coefficients for this class
  coef_class <- as.matrix(lasso_coef[[i]])
  non_zero <- which(coef_class != 0)
  
  if(length(non_zero) > 1) {  # More than just intercept
    for(j in non_zero) {
      if(rownames(coef_class)[j] != "(Intercept)") {
        cat(sprintf("%-20s: %8.4f\n", rownames(coef_class)[j], coef_class[j]))
      }
    }
  } else {
    cat("(All feature coefficients shrunk to zero)\n")
  }
}

# Get class predictions
lasso_pred <- predict(lasso_model, 
                      newx = xmat_test,
                      type = "class",
                      s = lambest_min)

#convert back to factor with og labels
lasso_pred_factor <- factor(lasso_pred, 
                            levels = 1:3,
                            labels = class_names)

# Confusion matrix
conf_matrix <- table(Predicted = lasso_pred_factor, 
                     Actual = test.set$Sleep.Disorder)

print(conf_matrix)

# Actual
# Predicted     Insomnia None Sleep Apnea
# Insomnia          16    1           2
# None               3   49           2
# Sleep Apnea        1    0          20

# Calculate error rate
lasso_error <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
#LASSO Test Error Rate: 0.0957 
#LASSO Test Accuracy: 0.9043 
