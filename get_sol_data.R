# Aaron
# Solar Power Generation (Get sol data)

get_sol_data.f<-function(data,daily_fac,peak_time="yes",level_of_data = "daily",forecast_horizon = 7){
  # filter data based on peak time "yes" or "no"
  if(peak_time == "yes" | peak_time == "no"){
    specific_time_interval <- data %>% filter(pk_time == peak_time)
  } else{
    # when you want to get the entire day
    specific_time_interval <- data
  }
  
  specific_time_interval$dateTime <- as_datetime(specific_time_interval$dateTime)
  
  # as_tbl_time makes the data frame a tibble time data frame which allows you to easily asjust how granular-
  # time is
  specific_time_interval<-as_tbl_time(specific_time_interval,index=dateTime)
  
  specific_time_interval$future_gen <- round(lead(specific_time_interval$gen,forecast_horizon),3)
  
  
  if (level_of_data != "hourly"){
    # ADD STUFF FOR SOLAR!!!
    # uses the tibbletime package
    specific_time_interval<- specific_time_interval %>% collapse_by(level_of_data) %>% 
      group_by(dateTime) %>% dplyr::summarise(gen = sum(gen), future_gen =sum(future_gen),temp=mean(temp),
                                       hum=mean(hum),vis = mean(vis), windBearing=mean(windBearing),
                                       press = mean(press),precipProb = mean(precipProb), cc = mean(cc))
    
    daily_fac$dateTime <- as.Date(daily_fac$dateTime)
    specific_time_interval$dateTime <- as.Date(specific_time_interval$dateTime)
    
    
    specific_time_interval <- inner_join(specific_time_interval, daily_fac, by="dateTime") # add in the factors since summarise removed them 
  }
  # ema is exponentially weighted moving average, putting this in the model to model last week's gen trend
  specific_time_interval$ema_gen <- EMA(specific_time_interval$gen, forecast_horizon)
  
  return(specific_time_interval)
}
