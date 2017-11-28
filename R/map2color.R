#' converts numbers to colors
#' 
#'
#' @param x vector of numbers
#' @param pal color palette
#' @param limits vector of length two describing the limits of the color mapping
#' @export


#' @examples
#' x = 1:10
#' y = rep(1,10)
#' pal = colorRampPalette(c("red","green","blue"))(10)
#' cols = map2color(x,pal=pal)
#' plot(x=x,y=y,col=cols,pch=19)

# modified from response here (https://stackoverflow.com/questions/15006211/how-do-i-generate-a-mapping-from-numbers-to-colors-in-r)
map2color <- function(x=NULL,pal=colorRampPalette(c("deepskyblue2","black","gold"))(100),limits=NULL)
{
  
  # set the limits to the min and max
  if(is.null(limits)) 
  {
    # get just the finite values
    finitevalues = x[which(is.finite(x)==TRUE)]
    
    # set the limits
    limits = range(x)
  }
  
  # get the new colors
  newcolors = pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
  
  # return the new colors
  return(newcolors)
}

