PlotStdCurve <- function(indata) {
  if (is.null(indata)) {return(NULL)} else {
    data <- filter(indata,Group == "STD")
    lm <- lm(Area~Amount, data=data)
    plot <- (
      ggplot(data=data, aes(x=Amount,y=Area)) 
      + geom_point(size=2) 
      + geom_abline(intercept = lm$coefficients[1],slope = lm$coefficients[2], color = "light grey") 
      + geom_segment(aes(xend = Amount, yend = lm$fitted.values)) 
      + theme_bw()
    )
    return(plot)
  }
}