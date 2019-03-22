########################### Set up environment #################################
# load required packages
library(lubridate)
library(dplyr)
library(magrittr)
library(stringr)
library(rvest)
library(ggplot2)
library(grid)
library(scales)
library(leaflet)
library(shiny)
library(DT)
library(circlize)
library(readr)
library(plyr)
library(lubridate)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibbletime)
library(ggplot2)
library(randomForest)
library(Metrics)
library(zoo)
library(gbm)
library(MASS)
library(TTR)
library(shiny)
library(shinydashboard)

# set up some colours to use
DarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
Purple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
lightGrey <- rgb(red = 186, green = 187, blue = 188, maxColorValue = 255)


# run the scripts that get, clean and combine the data together
homeC_clean_2014 <- read.csv("homeC_clean_2014.csv")
homeC_clean_2015 <- read.csv("homeC_clean_2015.csv")
homeC_clean_2016 <- read.csv("homeC_clean_2016.csv")
daily_factors_all_years <- read.csv("homeC_daily_factors.csv")
homeC_clean_all_years <- rbind(homeC_clean_2014,homeC_clean_2015,homeC_clean_2016)

# Establish a new column for peak our for solar power generation
# peak_time<-function(df){
#   timeDummy <- as.numeric(0:23)[df$time]
#   for (i in 1:nrow(df)){
#     if(timeDummy[i]>=9&timeDummy[i]<=17){
#       df$pk_time[i] <- "yes"
#     } else{
#       df$pk_time[i] <- "no"
#     } # end else
#   }
#   return(df);
# }
# homeC_clean_2014 <- peak_time(homeC_clean_2014);
# homeC_clean_2015 <- peak_time(homeC_clean_2015);
# homeC_clean_2016 <- peak_time(homeC_clean_2016);
# homeC_clean_all_years <- peak_time(homeC_clean_all_years);
# write.csv(homeC_clean_2014, file = "homeC_clean_2014.csv")
# write.csv(homeC_clean_2015, file = "homeC_clean_2015.csv")
# write.csv(homeC_clean_2016, file = "homeC_clean_2016.csv")
# write.csv(homeC_clean_all_years, file = "homeC_clean_all_years.csv")

##### pull in helper functions ##### 
# source("./season_period.R")
# source("./add_factors.R")
# source("./get_processed_data.R")
# source("./plot_model.R")
# source("./evaluate_model_1.R")

# Solar Power Generation Helper Functions:
source("./get_sol_data.R")
source("./plot_sol_model.R")
source("./eval_sol_model_1.R")

get_processed_data<-function(data,time_period="morning",level_of_data = "daily",forecast_horizon = 7){
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
        group_by(dateTime) %>% summarise(use = sum(use), future_use =sum(future_use),temp=mean(temp),
                                         hum=mean(hum))
      
      daily_factors_all_years$dateTime <- as.Date(daily_factors_all_years$dateTime)
      specific_time_interval$dateTime <- as.Date(specific_time_interval$dateTime)
      
      
      specific_time_interval <- inner_join(specific_time_interval, daily_factors_all_years, by="dateTime") # add in the factors since summarise removed them 
    }
    # ema is exponentially weighted moving average, putting this in the model to model last week's use trend
    specific_time_interval$ema_use <- EMA(specific_time_interval$use, forecast_horizon)
    
    return(specific_time_interval)
}

plot_model <- function(specific_time_interval_test_data,view_window_start="2016-01-30",view_window_end="2016-04-05"){
    
    start_day <-  which(grepl(view_window_start, specific_time_interval_test_data$dateTime))
    
    end_day <-  which(grepl(view_window_end, specific_time_interval_test_data$dateTime))
    
    data<- specific_time_interval_test_data[start_day:end_day,]
    
    # plot code is complex to make the plot plug and play with the input variables
    model_plot<-ggplot(data, aes(dateTime,future_use,group=1)) + geom_line(color="blue") +  
      geom_line(aes(dateTime,gbm_pred,group=1),color="red") + 
      ggtitle(" Use[kW] Plot of Test Data (Gradient Boosting Model) ",
              subtitle=paste("Actual use blue, Predicted use red","")) +
      theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),text = element_text(size=15))+ylab("Use [kW]")
    
    return(model_plot)
}
  
evaulate_model_1<-function(specific_time_interval, start_date="2014-01-01",end_date="2016-05-08",
                             train_end_date = "2016-01-01",forecast_horizon = 7){
    
    start_day <-  which(grepl(start_date, specific_time_interval$dateTime))
    
    end_day <-  which(grepl(end_date, specific_time_interval$dateTime))
    
    train_days <- which(grepl(train_end_date, specific_time_interval$dateTime))
    
    # used to get train data subset
    start_plus_train_day <-train_days+start_day
    
    # gradient boosting model
    # Note: when level_of_data is not hourly, use is cumulative, temp and hum are averages
    gbm <- gbm(future_use ~ use + hum + ema_use + temp + day_of_week + season + month, distribution = "gaussian",
               data = specific_time_interval[start_day:start_plus_train_day,], n.trees = 500, interaction.depth = 4,
               shrinkage = 0.01)
    
    # used to get test data subset
    pred_start_day<-start_plus_train_day+1+forecast_horizon # make sure test data doesn't include train data
    specific_time_interval_test_data <- specific_time_interval[pred_start_day:end_day,] 
    
    # predict using gradient boosting model on held out test set
    pred <-predict.gbm(gbm, specific_time_interval_test_data,n.trees = 500)
    
    # append predictions to data frame
    specific_time_interval_test_data$gbm_pred <- round(pred,3)
    
    # use mean square error of model to evaluate model performance
    mse_of_model<-Metrics::mse(pred,specific_time_interval_test_data$future_use)
    
    # return the mean square error, variable importance through summary, and the test data
    model_results<-list(mse_of_model,summary(gbm),specific_time_interval_test_data)
    
    return(model_results)
}


##### end helper functions ##### 

# initialize state of dashboard to something that works 
view_window_start = "2016-01-30"
view_window_end = "2016-04-05"
forcast_horizon = 7
start_date = "2014-01-07" # start late due to ewa needing time for its last week trend
end_date = "2016-11-05"
train_end_date = "2016-01-01"
level_of_data = "daily"
time_period = "late evening"


# establish shiny server -------------------------------------------------------
shinyServer( function(input, output) {
  # output plot 1
  output$plot1 <- renderPlot({
    processed_data <- get_processed_data(homeC_clean_all_years,input$time_period,
                                         level_of_data,input$slider)
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$slider)
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    model <- evaulate_model_1(processed_data,input$date_range[1], input$date_range[2],
                              input$train_date,input$slider)  
    obj2 <- model[[3]] 
    plot <- plot_model(obj2,input$window_range[1],input$window_range[2])
    plot
  })
  # output text 1
  output$text1 <- renderText({
    processed_data <- get_processed_data(homeC_clean_all_years,input$time_period,
                                         level_of_data,input$slider)
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$slider)
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    obj <- processed_data
    model <- evaulate_model_1(obj, input$date_range[1], input$date_range[2],
                              input$train_date, input$slider)  
    model[[1]]
  })
  # output table
  output$table <- renderDataTable({
    processed_data <- get_processed_data(homeC_clean_all_years,input$time_period,
                                         level_of_data,input$slider)
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$slider)
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    model <- evaulate_model_1(processed_data,input$date_range[1], input$date_range[2],
                              input$train_date,input$slider)  
    # for data table output
    obj3 <- model[[3]] %>% dplyr::select(dateTime,forecast_date,future_use,
                                         gbm_pred) %>% rename("Date" = dateTime,"Forecast Date"=forecast_date,
                                                              "Actual Forecast Date Use [kW]" = future_use, "Model Prediction [kW]"=gbm_pred)
    # allows me to give a default length to the table, datatable function from DT package
    datatable(obj3,
              options = list(
                "pageLength" = 10))})
  
  
  #### Solar Power Generation Outputs: ####
  # output plot 2
  output$plot2 <- renderPlot({
    processed_data <- get_sol_data.f(homeC_clean_all_years,daily_factors_all_years,input$peak_time,
                                         level_of_data,input$solslide)
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$solslide)
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    model <- eval_sol_model_1.f(processed_data,input$sol_date_range[1], input$sol_date_range[2],
                              input$sol_train_date,input$solslide)  
    obj2 <- model[[3]] 
    plot <- plot_sol_model.f(obj2,input$sol_win_range[1],input$sol_win_range[2])
    plot
  })
  # output text 2
  output$text2 <- renderText({
    processed_data <- get_sol_data.f(homeC_clean_all_years,daily_factors_all_years,input$peak_time,
                                         level_of_data,input$solslide)
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$solslide)
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    obj <- processed_data
    model <- eval_sol_model_1.f(obj, input$sol_date_range[1], input$sol_date_range[2],
                              input$sol_train_date, input$solslide)  
    model[[1]]
  })
  # output table 2
  output$table2 <- renderDataTable({
    processed_data <- get_sol_data.f(homeC_clean_all_years,daily_factors_all_years,input$peak_time,
                                         level_of_data,input$solslide)
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$solslide)
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    model <- eval_sol_model_1.f(processed_data,input$sol_date_range[1], input$sol_date_range[2],
                              input$sol_train_date,input$solslide)  
    # for data table output
    obj3 <- model[[3]] %>% dplyr::select(dateTime,forecast_date,future_gen,
                                         gbm_pred) %>% rename("Date" = dateTime,"Forecast Date"=forecast_date,
                                                              "Actual Forecast Date Gen [kW]" = future_gen, "Model Prediction [kW]"=gbm_pred)
    # allows me to give a default length to the table, datatable function from DT package
    datatable(obj3,
              options = list(
                "pageLength" = 10))})
})
