#' plots hotmap
#'
#'
#' @param ourmatrix matrix to biuld hotmap from
#' @param colors colors to use for heatmap
#' @param labrow boolean whether or not rows should be labeled
#' @param labcol boolean whether or not rows should be labeled
#' @param gaps vector of rows after which a gap should be inserted
#' @param gapsize size of each gap as a fraction of the entire height of the hotmap
#' @param rowcolors a matrix of colors to be plotted to the side of each row
#' @export
#' @examples
#' 
#' x = c(10,20,30,40,50,60)
#' mat = matrix( data =  x,nrow = 3, byrow= FALSE)
#' hotmap(mat)
#' 
#' 
#' a = readRDS("~/Dropbox/Work/Projects/Ongoing/Sushi2/extradata/ordered.matrix.RDS")
#' colnames(a) = c("0.0","0.5","1.0","1.5","2","4","6")
#' rowcolors = rep("black",nrow(a))
#' rowcolors[1:1315] = "firebrick2"
#' rowcolors[1:750]  = "grey"
#' hotmap(a,labrow = F,gaps=c(750,1315),rowcolors=rowcolors)
#' 
#' 
#' 
#' 


# Define a function to plot a heatmap from a matrix
hotmap <- function(ourmatrix,colors,labrow=TRUE,labcol=TRUE,gaps=NULL,gapsize=.025,rowcolors=NULL)
{

  # add space for rowcolors
  extracolspace = 0
  if (is.null(rowcolors) == FALSE)
  {
    verticalgap = .01* ncol(ourmatrix)
    extracolspace = ncol(as.matrix(rowcolors)) + verticalgap
  }
  
  # add gaps if neccesary
  extrarowspace = 0
  if (is.null(gaps) == FALSE)
  {
    absgapsize = gapsize*nrow(ourmatrix)
    extrarowspace = absgapsize*length(gaps)
  }
  
  # determine size of plot
  ourxlim = c(0, ncol(ourmatrix) + extracolspace )
  ourylim = c(-nrow(ourmatrix) - extrarowspace , 0)
  
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

  # remove col and rownames
  colnamesforlater = colnames(ourmatrix)
  rownamesforlater = row.names(ourmatrix)
  row.names(ourmatrix) = NULL
  colnames(ourmatrix) = NULL
  
  # convert to 3 column format
  threecol = setNames(melt(as.matrix(ourmatrix)), c("row", 'column', 'vals'))
  
  if (is.null(rowcolors) == FALSE)
  {
    threecolrowcol = setNames(melt(as.matrix(rowcolors)), c("row", 'column',"colors"))
    threecolrowcol$colors = as.character(threecolrowcol$colors)
  }
  
  if (is.null(gaps) == FALSE)
  {
   sortedgaps = gaps[order(gaps,decreasing = TRUE)]
    for (gap in sortedgaps)
   {
     threecol$row[which(threecol$row > gap)] = threecol$row[which(threecol$row > gap)] + absgapsize
     if (is.null(rowcolors) == FALSE)
     {
       threecolrowcol$row[which(threecolrowcol$row > gap)] = threecolrowcol$row[which(threecolrowcol$row > gap)] + absgapsize
     }
   }
  }
  
  threecol$rownames = rownamesforlater
  threecol$colnames = colnamesforlater
  
  # convert numbers to colors
  threecol$color = map2color(threecol$vals)

  # establish rectangle coordinates
  threecol$x0 = threecol$column - 1
  threecol$x1 = threecol$column
  threecol$y0 = -threecol$row
  threecol$y1 = threecol$y0 + 1
  
  # plot the data
  rect(xleft= threecol$x0,
       xright = threecol$x1,
       ybottom = threecol$y0,
       ytop =  threecol$y1,
        col = threecol$color,border = NA
       )
  
  if (is.null(rowcolors) == FALSE)
  {
    # establish rectangle coordinates
    threecolrowcol$x0 = verticalgap + ncol(ourmatrix) + threecolrowcol$column/2 - .5
    threecolrowcol$x1 = threecolrowcol$x0 + 0.5
    threecolrowcol$y0 = -threecolrowcol$row
    threecolrowcol$y1 = threecolrowcol$y0 + 1
    
    # plot the data
    rect(xleft= threecolrowcol$x0,
         xright = threecolrowcol$x1,
         ybottom = threecolrowcol$y0,
         ytop =  threecolrowcol$y1,
         col = threecolrowcol$color,border = NA
    )
  }
  

  # get unique names
  rownameandposition = threecol[1:nrow(ourmatrix),]
  
  # add row labels
  if (labrow == TRUE)
  {
    axis(side=2,at = rownameandposition$y0+0.5,labels = rownameandposition$rownames,las=2,tcl=0,lwd=0,font=2)
  }
  if (labcol == TRUE)
  {
    axis(side=1,at = 1:ncol(ourmatrix)-0.5,labels = colnamesforlater,las=1,tcl=0,lwd=0,font=2)
  }

  # add large labels
  
}





