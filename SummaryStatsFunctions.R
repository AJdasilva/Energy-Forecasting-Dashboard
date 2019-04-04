plot_variable <- function(data,variable_to_plot,xlim,ylim){
  
  xlimit <- c(0,1)
  if (!is.null(xlim[1])){
    
    xlimit[1] <- as.numeric(as.Date(xlim[1]))
    xlimit[2] <- as.numeric(as.Date(xlim[2]))
    
  }
  
  model_plot <- ggplot(data, aes(dateTime,get(variable_to_plot))) + geom_line(color="blue") + 
    coord_cartesian(xlim, ylim, expand = FALSE) + 
    ggtitle(paste("Plot of",variable_to_plot,"over time"))+theme(axis.ticks.x=element_blank(),text = element_text(size=15))+
    labs(x="Date", y = variable_to_plot)
  

  return(model_plot)
  }