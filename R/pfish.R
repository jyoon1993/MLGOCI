#' Fish dynamics (Biomass)
#' 
#' This function allows you to express the fish dynamics using GOCI
#' @param Please use the estimates of 'pphy'
#' @keywords Fish
#' @export
#' @examples 
#' pfish()
pfish <- function(x) {
  x1 = x
  x2 = (1-x[1])/2
  x3 = (1-x[1])/2
  
  con = x1[1]*x2[1]*x3[1]
  
  a = seq(0.5,1,by = 0.001)
  a = a[-c(1,length(a))]
  b = 2-a
  
  d1 = c()
  d2 = c()
  d3 = c()
  result = c()
  d = c()
  
  for(j in 1:length(a)){
    for(i in 1:length(x1)){
      d1 = x1[i]*(1-x1[i]-b[j]*x2[i]-a[j]*x3[i])
      d2 = x2[i]*(1-a[j]*x1[i]-x2[i]-b[j]*x3[i])
      d3 = x3[i]*(1-b[j]*x1[i]-a[j]*x2[i]-x3[i])
      
      
      x2[i+1] = x2[i]+d2
      x3[i+1] = x3[i]+d3
    }
    test = (x1[length(x1)]*x2[length(x1)]*x3[length(x1)])-con
    result = c(result,test)
  }
  j = which(abs(result)==min(abs(result)))
  
  for(i in 1:length(x1)){
    d1 = x1[i]*(1-x1[i]-b[j]*x2[i]-a[j]*x3[i])
    d2 = x2[i]*(1-a[j]*x1[i]-x2[i]-b[j]*x3[i])
    d3 = x3[i]*(1-b[j]*x1[i]-a[j]*x2[i]-x3[i])
    
    
    
    x2[i+1] = x2[i]+d2
    x3[i+1] = x3[i]+d3
    
    p.plot = plot(x3*0.255/mean(x3), type = 'l', ylim = c(0,1), xlab = 'Days', ylab = "Biomass (%)",axes = F, col = 'blue')
    +axis(1)
    +axis(2)
    box()
    print(p.plot)
    return(x3*0.255/mean(x3))
  }
}