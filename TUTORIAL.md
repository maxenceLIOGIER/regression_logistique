
# Tutorial: Using the LogisticRegression Package

## 1. Install the Package
To install the package, make sure you have `devtools` installed. Then run the following commands in R:

```r
install.packages("devtools")
devtools::install_github("username/LogisticRegression")
```

## 2. Loading and Preparing Data
For demonstration purposes, we will use a dataset `df` where `X` contains the feature variables and `y` contains the target variable.

```r
# Load your dataset
df <- read.csv("your_dataset.csv")

# Define the feature variables (X) and the target variable (y)
X <- df[, -ncol(df)]  # Exclude the last column as features
y <- df[, ncol(df)]   # The last column as the target variable
```

## 3. Data Splitting
To evaluate the model, we will split the dataset into training and testing sets (70% training and 30% testing):

```r
# Split data into training and testing sets
index <- sample(seq_len(nrow(df)), nrow(df) * 0.7)
X_train <- X[index, ]
y_train <- y[index]
X_test <- X[-index, ]
y_test <- y[-index]
```

## 4. Fitting the Model
Now, let's initialize the LogisticRegression model and fit it to the training data:

```r
# Initialize the LogisticRegression model
model <- LogisticRegression$new()

# Fit the model to the training data
fit_time <- system.time({
  model <- model$fit(X_train, y_train)
})

# Print the time taken to fit the model
print(fit_time)
```

## 5. Model Summary
Once the model is trained, we can display a summary of the model's performance and parameters:

```r
# Show the model summary
model$summary()
```

## 6. Predicting with the Model
Next, we will use the `predict()` method to make predictions on the test data:

```r
# Make predictions on the test data
y_pred <- model$predict(X_test)

# Print the model's confusion matrix
print(model$test(y_test, y_pred, confusion_matrix = TRUE))
```

## 7. (Optional) Variable Importance
You can also check the importance of each feature in the model using the `var_importance()` method:

```r
# Show variable importance (optional)
model$var_importance(graph = TRUE)
```

## Conclusion
This basic tutorial walks you through the process of training and evaluating a logistic regression model using our `LogisticRegression` package. For more detailed guidance, refer to the documentation or explore additional functions like `var_importance()` for more in-depth analysis.
