ExtractRSq <- function(indata) {
  data <- indata
  mod <- lm(data=data, Area~Amount)
  sum <- summary(mod)
  txt <-c(sum$r.squared)
  return(txt)
}