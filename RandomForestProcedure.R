library(randomForest)

library(caret)
# Importing training values
train_values <- read.csv("trainingsetvalues.csv")

# Importing training labels
train_labels <- read.csv("trainingsetlabels.csv")

# Importing test values
test_values <- read.csv("testsetvalues.csv")

# merging the training values and labels, 
# essentially just adds the training labels
train <- merge(train_values, train_labels)
set.seed(69)
# construction of the forest model
model_forest <- randomForest(as.factor(status_group) ~ amount_tsh
                             + gps_height + installer 
                             + longitude + latitude
                             + construction_year + extraction_type_group
                             + quality_group + quantity + source_class
                             + waterpoint_type, 
                             data = train, importance = TRUE,
                             ntree = 9, nodesize = 3)


# get model statistics
importance(model_forest)

# predict training set with own model
forest_pred_train <- predict(model_forest, train)
# get prediction stats
confusionMatrix(forest_pred_train, as.factor(train$status_group))

# predicting test set with model
forest_pred_test <- predict(model_forest, test_values)

# create submission data frame
submission <- data.frame(test_values$id)
submission$status_group <- forest_pred_test
names(submission)[1] <- "id"

# printing submission to csv
write.csv(submission, file = "submission.csv", row.names = FALSE)

