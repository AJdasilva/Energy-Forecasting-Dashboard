#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


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
library(h2o)
library(TTR)
library(DT)

library(broom)
library(modelr)

library(shiny)
library(shinydashboard)


homeC_clean_2014 <- read.csv("homeC_clean_2014.csv")
homeC_clean_2015 <- read.csv("homeC_clean_2015.csv")
homeC_clean_2016 <- read.csv("homeC_clean_2016.csv")
daily_factors_all_years <- read.csv("homeC_daily_factors.csv")

# put them all together in a big data frame
homeC_clean_all_years <- rbind(homeC_clean_2014,homeC_clean_2015,homeC_clean_2016)

view_window_start = "2016-01-30" # initialize state of dashboard to something that works 
view_window_end = "2016-04-05"
forcast_horizon = 7

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
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank())+ylab("Use [kW]")
  
  return(model_plot)
}

evaulate_model_1<-function(specific_time_interval, start_date="2014-01-01",end_date="2016-05-08",
                           train_end_date = "2016-01-01"){
  
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
  pred_start_day<-start_plus_train_day+1
  
  specific_time_interval_test_data <- specific_time_interval[pred_start_day:end_day,] 
  
  # predict using gradient boosting model on held out test set
  pred <-predict.gbm(gbm, specific_time_interval_test_data,n.trees = 500)
  
  # append predictions to data frame
  specific_time_interval_test_data$gbm_pred <- round(pred,3)
  
  # use mean square error of model to evaluate model performance
  mse_of_model<-hydroGOF::mse(pred,specific_time_interval_test_data$future_use)
                                                                                                                                                  
  # return the mean square error, variable importance through summary, and the test data
  model_results<-list(mse_of_model,summary(gbm),specific_time_interval_test_data)
  
  return(model_results)
}

# these dates are somewhat arbitrary
start_date = "2014-01-07" # start late due to ewa needing time for its last week trend
end_date = "2016-11-05"
train_end_date = "2016-01-01"
level_of_data = "daily"
time_period = "late evening"

ui <- dashboardPage(
  dashboardHeader(title = "Forecasting Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      
      box(title = "Error of Use Model (mse)",
          textOutput("text1"))
      
    ),
    fluidRow(
      
      box(plotOutput("plot1", height = 400)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Forecast Horizon (days):", 1, 50, 50),
        dateRangeInput("date_range", "Analysis Range: Start Date to End Date", start = start_date, end = end_date),
        dateInput("train_date","Date to End Training",value = train_end_date),
        selectInput("time_period", "Choose time period:", 
                    choices = c('Entire Day'='whole_day','Early Morning 1:00am-8:00am'='early morning','Afternoon 8:00am-11:00am'='morning',
                                'Late Afternoon noon-6:00pm'='afternoon',
                                'Evening 6:00pm-8:00pm'='evening','Late Evening 8:00pm-midnight'='late evening')),
        dateRangeInput("window_range", "Viewing Window Range: Start Date to End Date", start = view_window_start, end = view_window_end)
      ),
      box(
          dataTableOutput('table')
      )
    )
  )
)

server <- function(input, output) {
  
  values <- reactiveValues()
  
  observe({
    
    processed_data <- get_processed_data(homeC_clean_all_years,input$time_period,
                       level_of_data,input$slider)
   
    values$obj <- processed_data
  
    processed_data$forecast_date <- lead(as.numeric(processed_data$dateTime),input$slider)
    
    processed_data$forecast_date <- as.Date(processed_data$forecast_date)
    
    model <- evaulate_model_1(processed_data,input$date_range[1], input$date_range[2],
                     input$train_date)  
    
    values$obj2 <- model[[3]] 
    

    # for data table output
    values$obj3 <- model[[3]] %>% dplyr::select(dateTime,forecast_date,future_use,
                                                gbm_pred) %>% rename("Date" = dateTime,"Forecast Date"=forecast_date,
                                                                     "Actual Forecast Date Use" = future_use, "Model Prediction"=gbm_pred)
    
   })
  
  output$plot1 <- renderPlot({
    
    obj2 <- values$obj2
    
    plot <- plot_model(obj2,input$window_range[1],input$window_range[2])
    
    plot
  })
  output$text1 <- renderText({
    
    obj <- values$obj
    model <- evaulate_model_1(obj, input$date_range[1], input$date_range[2],
                              input$train_date)  
    
    model[[1]]
  })
  output$table <- renderDataTable({
    
    obj3 <- values$obj3
    
    # allows me to give a default length to the table, datatable function from DT package
    datatable(obj3,
              options = list(
                "pageLength" = 10))}) 
}

shinyApp(ui, server)

#runApp('SmartGrid', display.mode = "showcase") # to look at code within app

