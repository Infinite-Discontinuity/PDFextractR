PlotAll <- function(indata_std, indata_smp) {
  if (is.null(indata_std)) {return(NULL)} else if (is.null(indata_smp)) {return(NULL)} else {
    data_std <- filter(indata_std,Group == "STD")
    data_cntr <- filter(indata_std,Group == "STD")
    data_smp <- indata_smp
    lm <- lm(Area~Amount, data=data_std)
    plot <- (
      ggplot(data=data_smp, aes(x=Amount,y=Area)) 
      + geom_point(data=data_std, size=2) 
      + geom_abline(intercept = lm$coefficients[1],slope = lm$coefficients[2], color = "light grey") 
      + theme_bw()
      + geom_point(data=data_smp, color='blue',size=2)
      + geom_rug(sides="trbl",color="blue")
      + coord_cartesian(clip = "off")
    )
    return(plot)
  }
}
