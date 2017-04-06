#' Generate a raw with statistic properties
#'
#' @params dim Number of dimension of the vector generate.
#' @params subspaces List of subspaces that contain a hidden space (i.e they show some dependency).
#' @params margins List of margins that correspond to each subspace.
#' @params prop Probability of a point belonging to the hidden space of a subspace to become an outlier.
#'
#' @returns A list with 2 elements where \code{data} contains the generated vector and \code{labels} the corresponding label.
#
#' @details
#' The row is at first drawn from the uniform distribution between 0 and 1 over each dimension
#' For each subspace in \code{subspaces}, if the row belongs to the hidden space (i.e. the value for each dimension
#' of the subspace if bigger than the margin), then it becomes an outlier with probability \code{prop}
#' If it is an outlier, it would stay in the hidden space and we make sure it is not too close from the border
#' by rescaling the value so that it is at least 10% of the hidden space away from it. 
#' If it is not an outlier, we map uniformly the point to one of the axis or center (for a 3-D dimensions)
#' The probability to be map to one of such element is determined by their volume
#' The bigger the area, the more likely the point will be mapped to the area. 
#' In the case a point is already an outlier in a subspace, it cannot become and outlier in an overlapping subspace
#' But it still has the same probability to become an outlier in another space. This appends by chance (pigeonhole principle)
#'
#' If the point if not an outlier, it will have \code{0} as label, otherwise, it has a string representing the different subspaces
generate.row <- function(dim=10, subspaces=list(c(3,4), c(7,8)), margins=list(0.1,0.1), prop=0.01) {
  # no sanity check, assumed to be done already 
  r <- runif(dim)
  outlierFlags <- rep(FALSE, length(subspaces))
  # TODO: Idea: Shuffle subspaces and margins (the same way obviously) 
  for(x in 1:length(subspaces)) {
    # Function generating the volumes of hypercubes of for subspace x of dimension length(subspaces[[x]])
    volume_function <- function(y) margins[[x]]^y*(1-margins[[x]])^(length(subspaces[[x]])-y)*choose(length(subspaces[[x]]),y)
    # Do something only if the point is already in the hidden region
    if(all(r[subspaces[[x]]] > margins[[x]])) { 
      # Do something only if the point is not already an outlier in any other subspace that has non-empty intersection 
      # If this is case, we might destroy his outlying behavior in the other subspace, which we want to avoid. 
      if(!any(lapply(subspaces[outlierFlags], function(y) length(intersect(subspaces[[x]],y))) > 0)) {
        outlierFlags[x] <- sample(c(TRUE,FALSE),1,prob=c(prop,1-prop)) # Choose if it is an outlier in this subspace, with probability prob
        if(!outlierFlags[x]) { # If it is not an outlier, move it to the L-shaped cluster 
          # Choose how many dimension we exclude (i.e., put it in an arm of the L or at the intersection, uniformly)
          # The number of dimensions to exclude is chosen with a probability such that it is uniform with the space of the margin
          # in the respective projections. 
          nb_exclude <- sample(0:(length(subspaces[[x]])-1), 1, prob=sapply(length(subspaces[[x]]):1, volume_function))
          exclude <- sample(1:length(subspaces[[x]]),nb_exclude)
          ifelse(nb_exclude != 0, s <- subspaces[[x]][-exclude], s <- subspaces[[x]])
          
          # Rescale uniformly the values of the attributes we did not exclude to fit in the range of the margin 
          r[s] <- (r[s]-margins[[x]])*margins[[x]]/(1-margins[[x]])
        } else {
          # In the case the point is an outlier: 
          # Add a little margin (10% of the hidden space) to avoid possible ambiguities about the nature of the outlier 
          wallmargin <- margins[[x]]+(1-margins[[x]])*0.1
          r[subspaces[[x]]] <- ((r[subspaces[[x]]]-margins[[x]]) * (1-wallmargin) / (1-margins[[x]])) + wallmargin
        }
      }
      
    }
  }
  
  # Create the true labels for each point
  # Do the process afterwards to minimize bug risks
  # i.e. make the labeling process independent from the generation 
  for(x in 1:length(subspaces)) {
    outlierFlags[x] <- all(r[subspaces[[x]]] > margins[[x]])
  }
  
  # Generate a description for the outlier as an integer composed of the index of each subspaces separated by ;
  # In case the point is not an outlier in any space then just put a 0
  ifelse(any(outlierFlags), description <- paste(sapply(subspaces[outlierFlags],function(x) paste(x,collapse=",")),collapse=";"), description <- "0") 
  
  # Row output 
  list("data"=r, "label"=description)
}

#' Helper function that generate multiple rows with the same characteristics
#' Useful to generate a static stream
#'
#' @params n Number of rows to generate.
#' @params dim Number of dimension of the vector generate.
#' @params subspaces List of subspaces that contain a hidden space (i.e they show some dependency).
#' @params margins List of margins that correspond to each subspace.
#' @params prop Probability of a point belonging to the hidden space of a subspace to become an outlier.
#'
#' @returns A list with 2 elements where \code{data} is a data.frame object containing the \code{n} vectors and \code{labels} containing \code{n} corresponding labels. 
generate.multiple.rows <- function(n, dim, subspaces, margins, prop) {
  # no sanity check, assumed to be done already 
  data <- data.frame()
  labels <- c()
  for(x in 1:n) {
    res <- generate.row(dim=dim, subspaces=subspaces, margins=margins, prop=prop)
    data <- rbind(data, t(res$data))
    labels <- c(labels, res$label)
  }
  attributes(data)$names <- c(1:dim)
  list("data"=data, "labels"=labels)
}