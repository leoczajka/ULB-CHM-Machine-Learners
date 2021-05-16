# construction of the forest model
model_tree <- rpart(formula = as.factor(status_group) ~ quantity + longitude 
                    + latitude + construction_year + gps_height + amount_tsh,
                    data = train, method = "class")


# predict training set with own model
rpart_pred_train_labs <- predict(model_tree, train, type = 'class')
rpart_pred_train <- data.frame(train$id)
names(rpart_pred_train)[1]<-"id"
rpart_pred_train[2]<-rpart_pred_train_labs
names(rpart_pred_train)[2]<-"status_group"
# get prediction stats
confusionMatrix(as.factor(rpart_pred_train$status_group), as.factor(train$status_group))

# get a matrix of the test predictions
rpart_pred_test_labs <- predict(model_tree, test, type = 'class')
rpart_pred_test <- data.frame(test$id)
names(rpart_pred_test)[1]<-"id"
rpart_pred_test[2]<-rpart_pred_test_labs
names(rpart_pred_test)[2]<-"status_group"


# printing submission to csv
write.csv(rpart_pred_test, file = "submission.csv", row.names = FALSE)