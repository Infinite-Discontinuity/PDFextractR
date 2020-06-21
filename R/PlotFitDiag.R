PlotFitDiag <- function(indata) {
  if(is.null(indata)) {return(NULL)} else{
    data <- filter(indata,Group == "STD")
    mod <- lm(data=data, Area~Amount)
    plot <- (
      ggplot(data=data, aes(x=Amount, y=mod$residuals)) 
      + geom_point(size=2) 
      + geom_smooth() 
      + geom_hline(aes(yintercept=0),linetype = "dashed") 
      + theme_bw()
    )
    return(plot)
  }
}