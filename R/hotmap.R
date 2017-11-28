




# Define a function to plot a heatmap from a matrix
hotmap <- function(ourmatrix,colors)
{
  # for testing (delete before using)
   ourmatrix = mat

  # determine size of plot
  ourxlim = c(0, ncol(ourmatrix))
  ourylim = c(0, nrow(ourmatrix))
  
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
       ann = FALSE
       )
  
  # convert numbers to colors
  newcolors = map2color(ourmatrix)
  
  # set the y coordinates
  xlefts = c()
  for (i in (0:(ncol(ourmatrix)-1)))
  {
    xlefts = c(xlefts, rep(i, nrow(ourmatrix)))
  }
  
  # plot the data
  rect(xleft= xlefts,
       xright = xlefts +1,
       ybottom = rep(seq(nrow(ourmatrix)-1,0),ncol(ourmatrix)), 
       ytop =  rep(seq(nrow(ourmatrix),1),ncol(ourmatrix)),
        col = newcolors
       )
}





# make a list (vector of numbers)
x = c(10,20,30,40)

# convert that vector into a 2 x 2 matrix
mat = matrix( data =  x,nrow = 2, byrow= TRUE)

# plot hotmap
hotmap(mat)
