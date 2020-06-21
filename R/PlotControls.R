PlotControls <- function(indata) {
  if (is.null(indata)) {return(NULL)} else {
    data_std <- filter(indata,Group == "STD")
    data_cntr <- filter(indata,Group == "CNTR")
    lm <- lm(Area~Amount, data=data_std)
    plot <- (
      ggplot(data=data_cntr, aes(x=Amount,y=Area)) 
      + geom_point(data=data_std,size=2) 
      + geom_abline(intercept = lm$coefficients[1],slope = lm$coefficients[2], color = "light grey") 
      + theme_bw() 
      + geom_point(data=data_cntr, color='red',size=2) 
      + geom_vline(aes(xintercept=Amount), color='red') 
      + geom_hline(aes(yintercept=Area), color='red'))
    return(plot)
  } 
}