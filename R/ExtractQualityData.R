ExtractQualityData <- function(indata) {
  if(is.null(indata)) {return(NULL)} else {
    data <- filter(indata,Group == "STD")
    data2 <- filter(indata,Group == "CNTR")
    tar <- 0.1
    mod <- lm(data=data, Area~Amount)
    sum <- summary(mod)
    av <- mean(data2$Concentration)
    sd <- sd(data2$Concentration)
    tbl <- data.frame()
    tbl[1,1] <-c(sum$r.squared)
    tbl[1,2] <- 100*(av - tar)/tar
    tbl[1,3] <- 100*sd/av
    names(tbl) <- c("R^2","Relative Error (%)","Relative SD (%)")
    return(tbl)
  }
}