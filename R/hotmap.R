#' plots hotmap
#'
#' @param ourmatrix matrix to biuld hotmap from
#' @param colors colors to use for heatmap
#' @param labrow boolean whether or not rows should be labeled
#' @param labcol boolean whether or not rows should be labeled
#' @param gaps vector of rows after which a gap should be inserted
#' @param gapsize size of each gap as a fraction of the entire height of the hotmap
#' @param rowcolors a matrix of colors to be plotted to the side of each row
#' @param xlab.cex magnification level of x axis font
#' @param xlab.font font type of x axis font
#' @param selectylabs names of rows to be labeled on y axis
#' @param ylab.dist the distance (line) away from the y axis that labels whould be written
#' @param ylab.cex magnification level of x axis font
#' @param ylab.font font type of y axis font
#' @param radius (if selectylabs is not empty)
#' @export
#' @examples
#' 
#'
#'
#'data(SushiRNA)
#'colnames(SushiRNA) = c("0.0","0.5","1.0","1.5","2","4","6")
#'# first row colors
#'rowcolors = rep("black",nrow(a))
#'rowcolors[1:750]  = "grey"
#'rowcolors[751:1315] = "firebrick2"
#'par(mar=c(3,5,2,2))
#'par(mgp=c(3,.05,0))
#'test = sample(rownames(a),30)
#'hotmap(SushiRNA,labrow = F,gaps=c(750,1315),rowcolors=rowcolors,
#'        xlab.cex=0.6,ylab.cex=0.4,ylab.font=1,
#'        selectylabs = test,ylab.dist=0.05,radius = .01)
#'
#' mtext(side=1,line=1.0,font=2,text="Time (h) after LPS/IFNg treatment",cex=.75)
#'
#'
# Define a function to plot a heatmap from a matrix
hotmap <- function(ourmatrix,colors,labrow=TRUE,labcol=TRUE,gaps=NULL,gapsize=.025,rowcolors=NULL,
                   xlab.cex=1,xlab.font=1,
                   selectylabs = c(),ylab.dist=0.05,ylab.cex=1,ylab.font=1,
                   radius = 0.01)
{
  if (length(selectylabs) > 0)
  {
    if (radius > 1/(3*length(selectylabs)))
    {
      print (paste("radius too large.  Setting radius to",1/(3*length(selectylabs))))
      radius = 1/(3*length(selectylabs))
    }
  }
  
  # add space for rowcolors
  extracolspace = 0
  if (is.null(rowcolors) == FALSE)
  {
    verticalgap = .01* ncol(ourmatrix)
    extracolspace = 0.5 * ncol(as.matrix(rowcolors)) + verticalgap
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
  
  # convert data to 3 column format
  threecol = setNames(melt(as.matrix(ourmatrix)), c("row", 'column', 'vals'))

  
  # convert row colors to 3 column format
  if (is.null(rowcolors) == FALSE)
  {
    row.names(rowcolors) = NULL
    colnames(rowcolors) = NULL
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
    axis(side=2,at = rownameandposition$y0+0.5,labels = rownameandposition$rownames,las=2,tcl=0,lwd=0,font=2,cex.axis=ylab.cex)
  }
  if (length(selectylabs) > 0 )
  {
    # select rows to label
    ylabelsorig = rownameandposition[which(rownameandposition$rownames %in% selectylabs),]
    origpos = ylabelsorig$y0+0.5
    names(origpos) = ylabelsorig$rownames
    
    # determin optimal positions
    sphereradius = radius*(ourylim[2]-ourylim[1])
    shift        = sphereradius/3
    newpos = minimizeoverlap(origpos,maxiterations=100,
                             radius=sphereradius,
                             shift=shift,
                             minval=ourylim[1]-2*sphereradius,
                             maxval=ourylim[2]+2*sphereradius)
    
    labloffset = (ylab.dist/5)*abs(ourxlim[1]-ourxlim[2])
    x0 = rep(ourxlim[1],length(newpos))
    x1 = rep(ourxlim[1]-3*labloffset,length(newpos))
    x2 = rep(ourxlim[1]-4*labloffset,length(newpos))
    x3 = rep(ourxlim[1]-5*labloffset,length(newpos))
    
    # add lines and labels
    par(xpd=TRUE)
    segments(x0=x0,x1 = x1,y0=origpos,y1=newpos)
    segments(x0=x1,x1 = x2,y0=newpos,y1=newpos)

    text(x = x3, y = newpos,labels = names(newpos) ,adj = 1,cex = ylab.cex, font = ylab.font)
    par(xpd=TRUE)
  }
  
  if (labcol == TRUE)
  {
    axis(side=1,at = 1:ncol(ourmatrix)-0.5,labels = colnamesforlater,las=1,tcl=0,lwd=0,font=xlab.font,cex.axis=xlab.cex)
  }
}