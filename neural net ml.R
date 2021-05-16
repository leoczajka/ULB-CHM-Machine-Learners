
setwd("~/BioInformatique/MA1/Q2/machine learning/projet")
rm(list=ls())

##install.packages("neuralnet")
# load library
library(neuralnet)
library(caret)

# Importing data
train_values <- read.csv("training_set_val.csv")
train_labels <- read.csv("training_set_label.csv")
test_values <- read.csv("test_set_val.csv")

# merging the training values and labels
train <- merge(train_values, train_labels)
trainf <- merge(train_values, train_labels)
summary(train)

# creating training data set
waterpoint_type=c(trainf$waterpoint_type)
source=c(trainf$source)
quantity=c(trainf$quantity)
water_quality=c(trainf$water_quality)
payment_type=c(trainf$payment_type)
management=c(trainf$management)
extraction_type=c(trainf$extraction_type)
region_code=c(trainf$region_code)

gps=c(trainf$gps_height)

longitude=c(trainf$longitude)
latitude=c(trainf$latitude)

date_reco=c(trainf$date_recorded)
construction_year=c(trainf$construction_year)
tsh=c(trainf$amount_tsh)

status_group=c(trainf$status_group)



# combine the features into the data set dff
dff=data.frame(waterpoint_type,source,quantity,water_quality,payment_type,management,extraction_type,region_code,
              #tsh,
              date_reco,longitude,latitude,construction_year,status_group)

#neural network
nn=neuralnet(status_group~waterpoint_type+source+quantity+water_quality+payment_type+
               #management+
               extraction_type+
               #tsh+
               #date_reco+
               #longitude+latitude+
               #construction_year+
               region_code,data=dff, hidden=20,act.fct = "logistic",
             linear.output = FALSE)

# plot neural network
plot(nn)

# creating the df test set
waterpoint_typeT=c(test_values$waterpoint_type)
sourceT=c(test_values$source)
quantityT=c(test_values$quantity)
water_qualityT=c(test_values$water_quality)
payment_typeT=c(test_values$payment_type)
managementT=c(test_values$management)
extraction_typeT=c(test_values$extraction_type)
region_codeT=c(test_values$region_code)

gpsT=c(test_values$gps_height)

longitudeT=c(test_values$longitude)
latitudeT=c(test_values$latitude)

date_recoT=c(test_values$date_recorded)
construction_yearT=c(test_values$construction_year)
tshT=c(test_values$amount_tsh)

test=data.frame(waterpoint_typeT,sourceT,quantityT,water_qualityT,payment_typeT,
                  #managementT,
                  extraction_typeT,
                  #tshT,
                  #date_recoT,
                  #longitudeT,latitudeT,
                  #construction_yearT,
                  region_codeT)
                

## Prediction using neural network
prediction=compute(nn,test)
table(prediction$net.result)
hist(prediction$net.result)


# Converting probabilities 
prob <- prediction$net.result
pred <- ifelse(prob>0.99999997, 1, 0)
table(pred)


set.seed(69)

# get model statistics
importance(nn)

# predict training set with own model
nn_train <- predict(nn, trainf)
# get prediction stats
confusionMatrix(nn_train, as.factor(trainf$status_group))

# lowercasing installer in test set
test$altinstaller <- tolower(test$installer)

# predicting test set with model
nn_test_prediction <- predict(nn, test)

# create submission data frame
submission <- data.frame(test$id)
submission$status_group <- nn_test_prediction
names(submission)[1] <- "id"

# printing submission to csv
write.csv(submission, file = "submission.csv", row.names = FALSE)


