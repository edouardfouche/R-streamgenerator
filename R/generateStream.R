#' Generate Static Data Stream
#'
#' Generate a new synthetic multidimensional static data stream having the desired properties.
#'
#' @param n A vector containing \code{x} values, where the values corresponds to the number of points for each step and \code{x} to the number of steps. 
#' @param prop Proportion of outliers in the hidden space.
#' @param stream.config A stream configuration object. Should have been generated with \code{nsteps = 1}.
#'
#' @return A an object of class stream, which is a List of 5 elements.
#' - \code{data} contains the stream generated
#' - \code{labels} contains the description of each point (\code{0} if the point is not an outlier, or the subspace in which it is outlying as a string)
#' - \code{n} the number of points at each step
#' - \code{prop} the proportion of outliers in the hidden space
#' - \code{stream.config} the associated stream configuration object (which is valid only for static streams)
#'
#' @details
#' The data is generated uniformly, except in certain subspaces where the data is concentrated on the axes, 
#' in the L-like shape. This should create spaces with high dependency and space to observe hidden outliers. 
#' Note that the proportion of outlier \code{prop} does not relate directly to the percentage of outliers
#' in the output stream. Since it corresponds to the probability of a point, being ALREADY in the hidden space
#' to stay where it is, the overall proportion of outliers depends on the hidden space volume, which depends
#' on the number of subspaces and their margins.
#'
#' @examples
#' # Generate a stream with default parameters 
#' stream <- generate.static.stream()
#' # Generate a stream with custom configuration
#' stream.config <- generate.stream.config(dim=50, nsteps=1) # nsteps should be = 1
#' stream <- generate.static.stream(n=1000, prop=0.05, stream.config=stream.config)
#' # Output stream results (to uncomment)
#' # output.stream(stream, "example")
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @seealso
#' * \code{\link{generate.stream.config}}  : generate a stream.config file for a dynamic or static stream 
#'
#' @md
#' @export
generate.static.stream <- function(n=1000, prop=0.01, stream.config=NULL) {
  # Generate n point with dim dimensions where the list of subspaces are generated wall-like with the size of the wall taken from margins list
  # In the hidden space, a proportion prop of the points is taken as outliers 
  # Suggestion: add a verbose mode 
  sanitycheck.generate(n=n, prop=prop, stream.config=stream.config)

  if(is.null(stream.config)) {
    stream.config <- generate.stream.config(nsteps=1)
  } else {
    if(stream.config$nsteps != 1) {
      stop("The stream.config file in not compatible with static streams: nsteps should be = 1")
    }
  }
  dim <- stream.config$dim
  subspaces <- stream.config$subspaces
  margins <- stream.config$margins

  meta <- generate.multiple.rows(n, dim, subspaces, margins, prop)

  res <- list("data"=meta$data,"labels"=meta$labels, "n"=n, "prop"=prop, "stream.config"=stream.config)

  attr(res, "class") <- "stream"
  return(res)
}

#' Generate Dynamic Data Stream
#'
#' Generate a new synthetic multidimensional dynamic data stream having the desired properties.
#'
#' @param n A vector containing \code{x} values, where the values corresponds to the number of points for each step and \code{x} to the number of steps.
#' @param prop Proportion of outliers in the hidden space.
#' @param stream.config A stream configuration object. Should have been generated with \code{nsteps > 1}.
#' @param verbose If TRUE, then the state of the stream will be printed as output for every 100 points.
#'
#' @return A an object of class \code{stream}, which is a \code{List} of 5 elements.
#' - \code{data} contains the stream generated
#' - \code{labels} contains the description of each point (\code{0} if the point is not an outlier, or the subspace in which it is outlying as a string)
#' - \code{n} the number of points at each step
#' - \code{prop} the proportion of outliers in the hidden space
#' - \code{stream.config} the associated stream configuration object (which is valid only for dynamic streams)
#'
#' @details
#' The data is generated uniformly, except in certain subspaces where the data is concentrated on the axes, 
#' in the L-like shape. This should create spaces with high dependency and space to observe hidden outliers. 
#' Note that the proportion of outlier \code{prop} does not relate directly to the percentage of outliers
#' in the output stream. Since it corresponds to the probability of a point, being ALREADY in the hidden space
#' to stay where it is, the overall proportion of outliers depends on the hidden space volume, which depends
#' on the number of subspaces and their margins.
#'
#' @examples
#' # Generate a stream with default parameters 
#' stream <- generate.dynamic.stream()
#' # Generate a stream with custom configuration
#' stream.config <- generate.stream.config(dim=50, nsteps=10, volatility=0.5)
#' stream <- generate.dynamic.stream(n=100, prop=0.05, stream.config=stream.config)
#' # Output stream results (to uncomment)
#' # output.stream(stream, "example")
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @seealso
#' * \code{\link{generate.stream.config}}  : generate a stream.config file for a dynamic or static stream 
#'
#' @md
#' @export
generate.dynamic.stream <- function(n=100, prop=0.01, stream.config=NULL, verbose=FALSE) {
  # To check -> what is the correlation matrix ? 
  sanitycheck.generate(n=n, prop=prop, stream.config=stream.config, verbose=verbose)

  if(is.null(stream.config)) {
    stream.config <- generate.stream.config()
  } else {
    if(stream.config$nsteps <= 1) {
      stop("The stream.config file in not compatible with dynamic streams: nsteps should be > 1")
    }
  }

  if(length(n) == 1) {
    n <- rep(n,stream.config$nsteps)
  } # else assume that n has the good size, has checked by the sanity check 

  dim <- stream.config$dim
  subspaceslist <- stream.config$subspaceslist
  marginslist <- stream.config$marginslist


  data <- data.frame()
  labels <- c()
  # Generate some data for each time step description 
  for(seq in 1:length(n)) {
    if(verbose) print(paste("Step", seq, "of size", n[[seq]]))
    # Determine for the current state step the start and end margins values for each subspaces 
    subspaces_state <- list()
    currentmargins <- list()
    nextmargins <- list()
    
    if(seq ==1) {
      # At start, all attributes have no dependencies (this means, margins=1 for all)
      # As a result, we set the start value ("currentmargins") to 1 for all of them 
      subspaces_state <- subspaceslist[[seq]]
      currentmargins <- c(rep(1, length(subspaceslist[[seq]])))
      nextmargins <- marginslist[[seq]]
    } else {
      # We shall consider subspace from the previous and the next state 
      subspaces_state <- unique(c(subspaceslist[[seq-1]], subspaceslist[[seq]]))
      
      for(sub in 1:length(subspaces_state)) {
        # In the case a subspace is contained in the next step, its intended value should be equal to its margins in the next step    
        # Otherwise, it should be 1 
        if(any(sapply(subspaceslist[[seq]], function(x) setequal(x,subspaces_state[[sub]])))) {
          nextmargins <- c(nextmargins, marginslist[[seq]][sapply(subspaceslist[[seq]], function(x) setequal(x,subspaces_state[[sub]]))])
        } else {
          nextmargins <- c(nextmargins, 1)
        }
        
        # In the case a subspace is contained in the current step, its start value should be equal to its margins in the current step    
        # Otherwise, it should be 1 
        if(any(sapply(subspaceslist[[seq-1]], function(x) setequal(x,subspaces_state[[sub]])))) {
          currentmargins <- c(currentmargins, marginslist[[seq-1]][sapply(subspaceslist[[seq-1]], function(x) setequal(x,subspaces_state[[sub]]))])
        } else {
          currentmargins <- c(currentmargins, 1)
        }
      }
    }
    currentmargins <- as.list(currentmargins)
    nextmargins <- as.list(nextmargins)
    
    i <- 0
    for(x in 1:n[[seq]]) {
      # Update the current margins (transitioning uniformly between currentmargins and nextmargins)
      margins_state <- as.list(unlist(currentmargins) - (unlist(currentmargins)-unlist(nextmargins)) * (x-1)/n[[seq]])
      if(i %%100 == 0 & verbose) {
        print(c("subspaces_state:", paste(subspaces_state)), collapse=" ")
        #print(c("currentmargins:", paste(currentmargins)), collapse=" ")
        print(c("margins_state:", paste(margins_state)), collapse=" ")
        #print(c("nextmargins:", paste(nextmargins)), collapse=" ")
      }
      i <- i+1
      
      # Generate a row 
      res <- generate.row(dim=dim, subspaces=subspaces_state, margins=margins_state, prop=prop)
      data <- rbind(data, t(res$data))
      labels <- c(labels, res$label)
    }
  }
  # Put adequate names on the columns 
  attributes(data)$names <- c(1:dim)
  res <- list("data"=data,"labels"=labels, "n"=n, "prop"=prop, "stream.config"=stream.config)
  
  attr(res, "class") <- "stream"
  return(res)
}