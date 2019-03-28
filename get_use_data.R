# Lance
# Use (Get use data)

get_use_data<-function(data,time_period="morning",level_of_data = "daily",forecast_horizon = 7){
  
  if(time_period != "whole_day") {
    # get the specific interval you want like morning
    specific_time_interval <- data %>% filter(time_interval == time_period)
  }
  else{
    # when you want to get the entire day
    specific_time_interval <- data
  }
  specific_time_interval$dateTime <- as_datetime(specific_time_interval$dateTime)
  
  # as_tbl_time makes the data frame a tibble time data frame which allows you to easily asjust how granular-
  # time is
  specific_time_interval<-as_tbl_time(specific_time_interval,index=dateTime)
  
  specific_time_interval$future_use <- round(lead(specific_time_interval$use,forecast_horizon),3)
  
  if (level_of_data != "hourly"){
    
    # uses the tibbletime package
    specific_time_interval<- specific_time_interval %>% collapse_by(level_of_data) %>% 
      group_by(dateTime) %>% dplyr::summarise(use = sum(use), future_use =sum(future_use),temp=mean(temp),
                                              hum=mean(hum))
    
    daily_factors_all_years$dateTime <- as.Date(daily_factors_all_years$dateTime)
    specific_time_interval$dateTime <- as.Date(specific_time_interval$dateTime)
    
    
    specific_time_interval <- inner_join(specific_time_interval, daily_factors_all_years, by="dateTime") # add in the factors since summarise removed them 
  }
  # ema is exponentially weighted moving average, putting this in the model to model last week's use trend
  specific_time_interval$ema_use <- EMA(specific_time_interval$use, forecast_horizon)
  
  return(specific_time_interval)
}