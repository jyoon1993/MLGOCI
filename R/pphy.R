#' Phytoplankton dynamics (Biomass)
#' 
#' This function allows you to express the phytoplankton dynamics using GOCI
#' @param Please use the Chl-a pixel data offered by GOCI
#' @keywords Phytoplankton
#' @export
#' @examples 
#' pphy()
pphy <- function(x){
  res <- (log10(x)/mean(log10(x)))*0.235
  p.plot = plot(res/mean(res)*0.235, type = 'l', ylim = c(0,1), xlab = 'Days', ylab = "Biomass (%)",axes = F, col = 'black')
  +axis(1)
  +axis(2)
  box()
  print(p.plot)
  return(res/mean(res)*0.235)
}