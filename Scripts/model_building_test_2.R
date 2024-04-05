#set up random forest model
library(randomForest)


model <- model_df[,3:11]
model$type <- as.factor(as.character(model$type))


#Split our data between training and testing
#80% and 20% for testing 
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(model), 0.8 * nrow(model))  # 80% for training
train_data <- model[train_indices, ]
test_data <- model[-train_indices, ]


# Create random forest model

rf_model <- randomForest(type ~ ., data = train_data)

# test on training data 
predictions <- predict(rf_model, newdata = test_data)


# Evaluate the model
accuracy <- mean(predictions == test_data$type)
print(paste("Accuracy:", accuracy))

# Importance scores
importance_scores <- importance(rf_model)
print(importance_scores)
#create graph 

importance <- data.frame(importance_scores)

importance <- rownames_to_column(importance, var = "Index")

ggplot(importance, aes(x = Index, y = MeanDecreaseGini, fill = MeanDecreaseGini)) + 
  geom_bar(stat = "identity") + theme(panel.background = element_rect(fill = "transparent"))
  