
# plot the selected predictor variable or outcome over time
plot_variable <- function(data,variable_to_plot,xlim,ylim){
  
  xlimit <- c(0,1)
  if (!is.null(xlim[1])){ 
    
    xlimit[1] <- as.numeric(as.Date(xlim[1])) # fixes some issues I had with data types
    xlimit[2] <- as.numeric(as.Date(xlim[2]))
    
  }
  
  model_plot <- ggplot(data, aes(dateTime,get(variable_to_plot))) + geom_line(color="blue") + 
    coord_cartesian(xlim, ylim, expand = FALSE) + 
    ggtitle(paste("Plot of",variable_to_plot,"over time"))+theme(axis.ticks.x=element_blank(),text = element_text(size=15))+
    labs(x="Date", y = variable_to_plot)
  

  return(model_plot)
}

# plot the histogram with overlayed kernal density plot of the selected predictor variable or outcome
plot_histogram <- function(data, variable_to_plot){
  
  histogram <- ggplot(data, aes(get(variable_to_plot)))+geom_histogram(aes(y=..density..),bins=30, color="blue",fill="white",alpha=0.5)+
    geom_density(aes(y=..density..),color="black",fill="gray",alpha = 0.2)+labs(x=variable_to_plot, y = "count")+
    ggtitle(paste("Histogram and density plot of",variable_to_plot))+
    theme(plot.title=element_text(size=11),axis.ticks.x=element_blank(),text = element_text(size=15))
  histogram <- plotly::ggplotly(histogram)
  
  return(histogram)
}

# outputs the mean of a variable given a specific month
get_summary_stats <- function(data, variable_to_plot, given_month){
  
  summary_stats_vector <- c(0,0,0,0,0)
  
  data_subset <- data %>% dplyr::filter(month(dateTime) == given_month)
  
  variable_selected <- data_subset[,variable_to_plot]
  
  summary_stats_vector[[1]] <- round(mean(na.omit(variable_selected)),3)
  summary_stats_vector[[2]] <- round(var(na.omit(variable_selected)),3)
  summary_stats_vector[[3]] <- round(min(na.omit(variable_selected)),3)
  summary_stats_vector[[4]] <- round(max(na.omit(variable_selected)),3)
  summary_stats_vector[[5]] <- round(skewness(na.omit(variable_selected)),3)
  
  summary_stats_df<-as.data.frame(cbind(c("Mean","Variance", "Minimum", "Maximum", "Skewness"),summary_stats_vector))
  colnames(summary_stats_df) <- c("Statistic", "Value")
  
  return(summary_stats_df)
} 