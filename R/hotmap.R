#' plots hotmap
#'
#'
#' @param ourmatrix matrix to biuld hotmap from
#' @param colors colors to use for heatmap
#' @export


#' @examples
#' 
#' x = c(10,20,30,40,50,60)
#' mat = matrix( data =  x,nrow = 3, byrow= FALSE)
#' hotmap(mat)


# Define a function to plot a heatmap from a matrix
hotmap <- function(ourmatrix,colors,gaps=NULL,gapsize=.025)
{
  
  # determine size of plot
  ourxlim = c(0, ncol(ourmatrix))
  ourylim = c(0, nrow(ourmatrix))
  
  # add gaps if neccesary
  
  # add room for row and column colors if required

  # make a blank plot
  plot(1,type='n',

       # set x and y axes coordinates
       xlim=ourxlim ,
       ylim=ourylim,

       # use the exact xlim and ylim values
       xaxs= 'i',
       yaxs = 'i',

       # don't add tick marks
       xaxt='n',
       yaxt='n',

       # don't add axis labels
       ann = FALSE,
       bty = 'n'
       )

  # convert to 3 column format
  threecol = setNames(melt(ourmatrix), c('row', 'column', 'vals'))
  
  # convert numbers to colors
  threecol$color = map2color(threecol$vals)
  
  # establish rectangle coordinates
  threecol$x0 = threecol$column - 1
  threecol$x1 = threecol$column
  threecol$y0 = nrow(ourmatrix) - threecol$row
  threecol$y1 = threecol$y0 + 1
  
  # plot the data
  rect(xleft= threecol$x0,
       xright = threecol$x1,
       ybottom = threecol$y0,
       ytop =  threecol$y1,
        col = threecol$color,border = NA
       )
  
  
  # add labels
  
  # add large labels
  
}





