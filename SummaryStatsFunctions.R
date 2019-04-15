
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
    ggtitle(paste("Histogram and density plot of",variable_to_plot))+theme(axis.ticks.x=element_blank(),text = element_text(size=15))
  
  return(histogram)
}

# outputs the mean of a variable given a specific month
get_mean <- function(data, variable_to_plot, given_month){
  
  data_subset <- data %>% dplyr::filter(month(dateTime) == given_month)
  
  variable_selected <- data_subset[,variable_to_plot]
  
  mean_of_subset <- mean(na.omit(variable_selected))
  
  return(mean_of_subset)
} 