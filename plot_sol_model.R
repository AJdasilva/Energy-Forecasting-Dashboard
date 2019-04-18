# Aaron
# Solar Power Generation (plot sol model)

plot_sol_model.f <- function(specific_time_interval_test_data,view_window_start="2016-01-30",view_window_end="2016-04-05"){
  
  start_day <-  which(grepl(view_window_start, specific_time_interval_test_data$dateTime))
  
  end_day <-  which(grepl(view_window_end, specific_time_interval_test_data$dateTime))
  
  data<- specific_time_interval_test_data[start_day:end_day,]
  
  # plot code is complex to make the plot plug and play with the input variables
  model_plot<-ggplot(data, aes(dateTime,future_gen,group=1)) + geom_line(color="blue") +  
    geom_line(aes(dateTime,gbm_pred,group=1),color="green") + 
    ggtitle(" Gen[kW] Plot of Test Data (Gradient Boosting Model) ",
            subtitle=paste("Actual gen blue, Predicted gen green","")) +
    theme(plot.title=element_text(size=11), axis.title.x=element_blank(),axis.ticks.x=element_blank(),text = element_text(size=15))+ylab("Gen [kW]")
  model_plot <- plotly::ggplotly(model_plot)

  return(model_plot)
}