#' Returns a new value for an object in an attempt to space it out from others in a vector
#'
#'
#' @param label the name of the object in the vector to consider
#' @param nodes the complete vector of values (with names)
#' @param radius how big a a radius of repulsion should we give to each value
#' @param shift what is the maximum amount we should shift the value
#' @param minval what is the minimum allowed value after shifting
#' @param maxval what is the maximum allowed value after shifting
#' @export
#' 
linearspacing <- function(label,nodes,radius=.5,shift=0.1,minval=-Inf,maxval=Inf)
{
  # remove the node in question
  othernodes = nodes[names(nodes) != label]
  othernodes = c(nodes,minval,maxval)
  
  # check for one above or below
  greaternodes = othernodes[which(othernodes-nodes[label] > 0.0)]
  lesserrnodes = othernodes[which(othernodes-nodes[label] < 0.0)]
  
  # get distances to closest neighbor
  distabove = Inf
  distbelow = Inf
  if (length(greaternodes) > 0){distabove = min(abs(greaternodes - nodes[label]))}
  if (length(lesserrnodes) > 0){distbelow = min(abs(lesserrnodes - nodes[label]))}
  
  # check for overlap above and below
  aboveoverlap = FALSE
  belowoverlap = FALSE
  if (distabove < 2 * radius){aboveoverlap = TRUE}
  if (distbelow < 2 * radius){belowoverlap = TRUE}
  
  if (aboveoverlap == TRUE & belowoverlap == FALSE)
  {
    return (nodes[label] - min(c(distabove*.45,shift)))
  }
  if (aboveoverlap == FALSE & belowoverlap == TRUE)
  {
    return (nodes[label] + min(c(distbelow*.45,shift)))
  }
  if (aboveoverlap == TRUE & belowoverlap == TRUE)
  {
    if (distabove > distbelow)
    {
      return (nodes[label] + min(c(distabove*.45,shift)))
    }
    if (distabove < distbelow)
    {
      return (nodes[label] - min(c(distbelow*.45,shift)))
    }
  }
  return(nodes[label])
}

