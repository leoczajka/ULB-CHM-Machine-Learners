library(randomForest)

library(caret)
# Importing training values
train_values <- read.csv("trainingsetvalues.csv")

# Importing training labels
train_labels <- read.csv("trainingsetlabels.csv")

# Importing test values
test_values <- read.csv("testsetvalues.csv")

test<- test_values

# merging the training values and labels, 
# essentially just adds the training labels
train <- merge(train_values, train_labels)


# Functions:
na2mean<-function(vec) {
  mean_vec<-mean(vec,na.rm=T)
  vec[is.na(vec)]<-mean_vec
  vec
}

#  turns 0 to average in feature amount_tsh
train$amount_tsh[train$amount_tsh==0] <- NA

train$amount_tsh<- na2mean(train$amount_tsh)

test$amount_tsh[test$amount_tsh==0] <- NA

test$amount_tsh<- na2mean(test$amount_tsh)

# turns 0 to average in population

train$population[train$population==0] <- NA

train$population<- na2mean(train$population)

test$population[test$population==0] <- NA

test$population<- na2mean(test$population)

# turns 0 to average in construction_year

train$construction_year[train$construction_year==0] <- NA

train$construction_year<- na2mean(train$construction_year)

test$construction_year[test$construction_year==0] <- NA

test$construction_year<- na2mean(test$construction_year)


# Looking at the installer feature
summary(as.factor(train$installer))
# lowercasing installer
train$altinstaller <- tolower(train$installer)
# 
train$amount_tsh
# looking at the new alt-installer feature
summary(as.factor(train$altinstaller))

set.seed(69)
# construction of the forest model
model_forest <- randomForest(as.factor(status_group) ~ amount_tsh
                             + gps_height + altinstaller + date_recorded
                             + longitude + latitude + management
                             + construction_year + extraction_type_group
                             + water_quality + quantity + source
                             + waterpoint_type + population, 
                             data = train, importance = TRUE,
                             ntree = 78, nodesize = 2)


# get model statistics
importance(model_forest)

# predict training set with own model
forest_pred_train <- predict(model_forest, train)
# get prediction stats
confusionMatrix(forest_pred_train, as.factor(train$status_group))

# lowercasing installer in test set
test$altinstaller <- tolower(test$installer)

# predicting test set with model
forest_pred_test <- predict(model_forest, test)

# create submission data frame
submission <- data.frame(test$id)
submission$status_group <- forest_pred_test
names(submission)[1] <- "id"

# printing submission to csv
write.csv(submission, file = "submission.csv", row.names = FALSE)
