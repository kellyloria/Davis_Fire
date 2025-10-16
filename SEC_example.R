## Stepwise Clustered Ensemble (SCE) example

# full documentation at : https://cran.r-project.org/web/packages/SCE/readme/README.html

# devtools::install_github("loong2020/Stepwise-Clustered-Ensemble")

# Key limitation ** it requires full data set. No NAs

# Load required packages
library(SCE)
library(parallel)

## SCA (Single tree) Analysis
# Load the example datasets
data(Streamflow_training_10var)
data(Streamflow_testing_10var)

# Define predictors and predictants
Predictors <- c("Prcp", "SRad", "Tmax", "Tmin", "VP", "smlt", "swvl1", "swvl2", "swvl3", "swvl4")
Predictants <- c("Flow")


####
# What: Builds a Single-tree Clustered Analysis (SCA) model.
# How it works: It recursively splits data into subgroups (clusters) using significance tests (α = 0.05), 
#               forming a regression tree that models relationships between predictors and response.
#   Key parameters:
#   - Nmin: Minimum number of samples in a node.
#   - resolution: Controls precision of splitting thresholds.

# Perform SCA
set.seed(123)
model <- SCA(alpha = 0.05, 
             Training_data = Streamflow_training_10var, 
             X = Predictors, 
             Y = Predictants, 
             Nmin = 5, 
             resolution = 100)

# Use S3 methods
#   summary(): Overview of model structure.
#   importance(): Calculates how much each variable contributes to model accuracy.
#   predict(): Generates predicted streamflow for test data.
#   evaluate(): Computes performance metrics (R², RMSE, NSE, etc.) for both training and testing sets.


print(model)
summary(model)

# Calculate variable importance
Imp_ranking <- importance(model, digits = 2)
print(Imp_ranking)

# Make predictions
prediction <- predict(model, Streamflow_testing_10var)

# Evaluate performance
performance <- evaluate(
  object = model,
  Testing_data = Streamflow_testing_10var,
  Training_data = Streamflow_training_10var
)
print(performance)

Importance_ranking_sorted <- Imp_ranking[order(-Imp_ranking$Relative_Importance), ]
barplot(
  Importance_ranking_sorted$Relative_Importance,
  names.arg = Importance_ranking_sorted$Predictor,
  las = 2, # vertical labels
  col = "skyblue",
  main = "Variable Importance (SCA)",
  ylab = "Importance",
  xlab = "Predictor"
)


## SCE (Tree ensemble) Analysis
#   - ensemble of many SCA trees.
#   - Like a random forest — improves prediction accuracy and robustness by averaging multiple trees.

# Build SCE model
set.seed(123)
Ensemble <- SCE(Training_data = Streamflow_training_10var,
                X = Predictors,
                Y = Predictants,
                mfeature = round(0.5 * length(Predictors)),
                Nmin = 5,
                Ntree = 40,
                alpha = 0.05,
                resolution = 100)

# Use S3 methods
print(Ensemble)
summary(Ensemble)

# Make predictions
predictions <- predict(Ensemble, Streamflow_testing_10var)
cat("Prediction components:", names(predictions), "\n")
cat("Testing predictions dimensions:", dim(predictions$Testing), "\n")

# Calculate variable importance
Imp_ranking <- importance(Ensemble, digits = 2)

# Evaluate model performance
evaluation <- evaluate(
  object = Ensemble,
  Testing_data = Streamflow_testing_10var,
  Training_data = Streamflow_training_10var,
  digits = 3
)
print(evaluation)

Importance_ranking_sorted <- Imp_ranking[order(-Imp_ranking$Relative_Importance), ]
barplot(
  Importance_ranking_sorted$Relative_Importance,
  names.arg = Importance_ranking_sorted$Predictor,
  las = 2, # vertical labels
  col = "skyblue",
  main = "Variable Importance (SCE)",
  ylab = "Importance",
  xlab = "Predictor"
)

## Multiple Predictants Case
# Define predictors and multiple predictants
# Load the example datasets

# Try to keep the predictor-to-sample ratio below 1:10 
# (e.g., ≤ 10 predictors for every 100 samples) unless you use feature selection or regularization

data(Air_quality_training)
data(Air_quality_testing)

Predictors <- c("SO2", "NO2", "CO", "O3", "TEMP", "PRES", "DEWP", "RAIN", "WSPM")
Predictants <- c("PM2.5", "PM10")

# Build and evaluate model
set.seed(123)
Ensemble <- SCE(Training_data = Air_quality_training,
                X = Predictors,
                Y = Predictants,
                mfeature = round(0.5 * length(Predictors)),
                Nmin = 5,
                Ntree = 40,
                alpha = 0.05,
                resolution = 100)

# Use S3 methods
print(Ensemble)
summary(Ensemble)

# Make predictions
predictions <- predict(Ensemble, Air_quality_testing)

# Calculate variable importance
Imp_ranking <- importance(Ensemble, digits = 2)

# Evaluate model performance
evaluation <- evaluate(
  object = Ensemble,
  Testing_data = Air_quality_testing,
  Training_data = Air_quality_training
)
print(evaluation)

Importance_ranking_sorted <- Imp_ranking[order(-Imp_ranking$Relative_Importance), ]
barplot(
  Importance_ranking_sorted$Relative_Importance,
  names.arg = Importance_ranking_sorted$Predictor,
  las = 2, # vertical labels
  col = "skyblue",
  main = "Variable Importance (SCE)",
  ylab = "Importance",
  xlab = "Predictor"
)

##Recursive Feature Elimination
# Load the example datasets
data(Streamflow_training_22var)
data(Streamflow_testing_22var)

# Define predictors and predictants
Predictors <- c(
  "Precipitation", "Radiation", "Tmax", "Tmin", "VP",
  "Precipitation_2Mon", "Radiation_2Mon", "Tmax_2Mon", "Tmin_2Mon", "VP_2Mon",
  "PNA", "Nino3.4", "IPO", "PDO",
  "PNA_lag1", "Nino3.4_lag1", "IPO_lag1", "PDO_lag1",
  "PNA_lag2", "Nino3.4_lag2", "IPO_lag2", "PDO_lag2"
)
Predictants <- c("Flow")

# Perform RFE
set.seed(1)
result <- RFE_SCE(
  Training_data = Streamflow_training_22var,
  Testing_data = Streamflow_testing_22var,
  Predictors = Predictors,
  Predictant = Predictants,
  Nmin = 5,
  Ntree = 48,
  alpha = 0.05,
  resolution = 1000,
  step = 3  # Number of predictors to remove at each iteration
)

# Plot RFE results
Plot_RFE(result)

## how to split novel dataset in train/test?

# set.seed(123)  # for reproducibility
# n <- nrow(mydata)
# train_indices <- sample(1:n, size = 0.8 * n)  # 80% training
# 
# Training_data <- mydata[train_indices, ]
# Testing_data  <- mydata[-train_indices, ]


#Documentation
#Full documentation is available through the R help system:
  
  # Core functions
  ?SCE
?SCA

# S3 methods
?predict.SCE
?predict.SCA
?importance.SCE
?importance.SCA
?evaluate.SCE
?evaluate.SCA
?print.SCE
?print.SCA
?summary.SCE
?summary.SCA

# Traditional functions (for advanced users)
?Model_simulation
?SCA_tree_predict
?SCA_Model_evaluation
?SCE_Model_evaluation
?RFE_SCE
?Plot_RFE
?Wilks_importance
?SCA_importance