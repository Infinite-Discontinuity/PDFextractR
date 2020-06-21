ControlStats <- function(input) {
  data <- input
  tar <- 0.1
  av <- mean(data$Concentration)
  sd <- sd(data$Concentration)
  lst <- list()
  lst[[1]] <- 100*(av - tar)/tar
  lst[[2]] <- 100*sd/av
  return(lst)
  
  #output$rel_er <- 100*(av - tar)/tar
  #output$rel_sd <- 100*sd/av
  #return(rel_er, rel_sd)
}