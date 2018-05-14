#' Define a function that iteratively shift values in a vector avoid overlap
#'
#' @param x vector of values to be spaced out
#' @param maxiterations the maximum number of iterations to try
#' @param radius how big a a radius of repulsion should we give to each value
#' @param shift what is the maximum amount we should shift the value
#' @param minval what is the minimum allowed value after shifting
#' @param maxval what is the maximum allowed value after shifting
#' @export
#' @examples 
#' minval = 0 - 0.5
#' maxval = 20 + 0.5
#' plot(1,type='n',ann=F,xaxt='n',yaxt='n',xlim=c(0,40),ylim=c(0,20),xaxs='i',yaxs='i')
#' 
#' # pick vals between 1 and 100
#' x = runif(10,0,20)
#' y = minimizeoverlap(x,maxiterations=100,radius=.5,shift=0.1,minval=minval,maxval=maxval)
#' 
#' for (i in 1:length(x))
#' {
#'   plotCircle(1,x[i],.5)
#'   plotCircle(2,y[i],.5)
#' }
#'
### Define a function that iteratively moves values in one dimension to avoid overlap
minimizeoverlap <- function(x,maxiterations=100,radius=.5,shift=0.1,minval=-Inf,maxval=Inf)
{
  # add names to vector if not supplied
  if (is.null(names(x))){names(x) = as.character(1:length(x))}
  
  for (j in 1:maxiterations)
  {
    newval=unlist(lapply(names(x),linearspacing,nodes=x,radius=radius,shift=shift,minval=minval,maxval=maxval))
    if (identical(x, newval))
    {
      print (paste("optimized positions after",j," iterations"))
      break
    }
    x=newval
  }
  return(unlist(x))
}

plotCircle <- function(x, y, r) {
  angles <- seq(0,2*pi,length.out=360)
  lines(r*cos(angles)+x,r*sin(angles)+y)
}
