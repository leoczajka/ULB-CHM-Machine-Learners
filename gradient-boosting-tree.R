
# running time is extremely slow so we will restrict our sample
# keep var with 
# common mistakes : "factor quantity has new levels unknown"

df_gbt <- df %>% filter(df$construction_year!=0)
df_gbt <- df_gbt %>% filter(df_gbt$population>1)
df_gbt <- df_gbt %>% filter(df_gbt$amount_tsh>0)

df_gbt$random_100 <- runif(nrow(df_gbt), min=1, max=nrow(df_gbt))
df_gbt$subset_100 <-  ntile(df_gbt$random_100, 100)

df_gbt <- df_gbt %>% filter(df_gbt$subset_100 < 10)
nrow(df_gbt)

df_gbt$random <- runif(nrow(df_gbt), min=1, max=nrow(df_gbt))
df_gbt$subset <-  ntile(df_gbt$random, 10)

gbt_model_1 <- function(some_number) {
  df_train <- df_gbt %>% filter(df_gbt$subset == some_number)
   df_test <- df_gbt %>% filter(df_gbt$subset != some_number)
  
   model_gbt <- train(as.factor(status_group) ~ 
                                 + gps_height + population 
                               + construction_year + longitude + quantity_group + latitude, 
                               data = df_train,
                               method = "xgbTree")
  
  df_test$y_pred <- predict(model_gbt, df_test)
  
  error <- sum(df_test$y_pred!=df_test$status_group)/nrow(df_test)
  
  results <- list("df_test" = df_test , "model_gbt" = model_gbt, "error" = error)
  
  return(results)
}


list_results_gbt1 <- lapply(1:10, gbt_model_1) #Generate data
results_v = c()
for (i in 1:10){
  results_v[i] <- list_results_gbt1[[i]][3]$error
  print(list_results_gbt1[[i]][3]$error)
}

error_mean_gbtmodel1 = mean(results_v)
print('mean of the 10 results (gbt model 1) : ')
print(error_mean_gbtmodel1)

