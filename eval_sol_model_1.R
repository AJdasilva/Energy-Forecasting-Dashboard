# Aaron
# Solar Power Generation (Evaluate Sol Model 1)

eval_sol_model_1.f <-function(specific_time_interval, start_date="2014-01-01",end_date="2016-05-08",
                           train_end_date = "2016-01-01",forecast_horizon = 7){
  
  start_day <-  which(grepl(start_date, specific_time_interval$dateTime))
  
  end_day <-  which(grepl(end_date, specific_time_interval$dateTime))
  
  train_days <- which(grepl(train_end_date, specific_time_interval$dateTime))
  
  # used to get train data subset
  start_plus_train_day <-train_days+start_day
  
  # gradient boosting model
  # Note: when level_of_data is not hourly, use is cumulative, temp and hum are averages
  gbm <- gbm(future_gen ~ gen + hum + ema_gen + temp + vis + windBearing + press + precipProb + cc + season + month, distribution = "gaussian",
             data = specific_time_interval[start_day:start_plus_train_day,], n.trees = 1000, interaction.depth = 5,
             shrinkage = 0.01)
  
  # used to get test data subset
  pred_start_day<-start_plus_train_day+1+forecast_horizon # make sure test data doesn't include train data
  specific_time_interval_test_data <- specific_time_interval[pred_start_day:end_day,] 
  
  # predict using gradient boosting model on held out test set
  pred <-predict.gbm(gbm, specific_time_interval_test_data,n.trees = 500)
  
  # append predictions to data frame
  specific_time_interval_test_data$gbm_pred <- round(pred,3)
  
  # use mean square error of model to evaluate model performance
  mse_of_model<-Metrics::mse(pred,specific_time_interval_test_data$future_gen)
  
  # return the mean square error, variable importance through summary, and the test data
  model_results<-list(mse_of_model,summary(gbm),specific_time_interval_test_data)
  
  return(model_results)
}