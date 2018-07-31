#' Generate Static Data Stream
#'
#' Generate a new synthetic multidimensional static data stream having the
#' desired properties.
#'
#' @param n A vector containing \code{x} values, where the values corresponds
#'        to the number of points for each step and \code{x} to the number of
#'        steps. 
#' @param prop Proportion of outliers in the hidden space.
#' @param proptype Type of the proportion of outliers. Value "proportional":
#'        depend on the size of the empty space. Value "absolute": same absolute
#'        proportion per subspace.  
#' @param stream.config A stream configuration object. Should have been
#'        generated with \code{nstep = 1}.
#'
#' @return An object of class stream, which is a List of 5 elements.
#' - \code{data} contains the stream generated
#' - \code{labels} contains the description of each point (\code{0} if the point
#'     is not an outlier, or the subspace in which it is outlying as a string)
#' - \code{n} the number of points at each step
#' - \code{prop} the proportion of outliers in the hidden space
#' - \code{stream.config} the associated stream configuration object (which is
#'     valid only for static streams)
#'
#' @details
#' The data is generated uniformly, except in certain subspaces where the data
#' is concentrated in particular dependencies (i.e. in the "Wall" dependency,
#' data concentrates on the axes, in a L-like shape). This should create spaces
#' with high dependency and space to observe hidden outliers. Note that the
#' proportion of outlier \code{prop} does not relate directly to the percentage
#' of outliers in the output stream. Since it corresponds to the probability of
#' a point, being ALREADY in the hidden space to stay where it is, the overall
#' proportion of outliers depends on the hidden space volume, which depends
#' on the number of subspaces and their margins. The greater the margin, the
#' bigger the hidden space. 
#'
#' @examples
#' # Generate a stream with default parameters 
#' stream <- generate.static.stream()
#' # Generate a stream with custom configuration
#' stream.config <- generate.stream.config(dim=50, nstep=1) # nstep should be 1
#' stream <- generate.static.stream(n=1000, prop=0.05,
#'                                  stream.config=stream.config)
#' # Output stream results (to uncomment)
#' # output.stream(stream, "example")
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @seealso
#' * \code{\link{generate.stream.config}}  : generate a stream.config file for a
#'     dynamic or static stream 
#'
#' @md
#' @export
generate.static.stream <- function(n=1000, prop=0.01, proptype="proportional",
                                   stream.config=NULL) {
  # Generate n points with dim dimensions where the list of subspaces are
  # generated wall-like with the size of the wall taken from margins list as
  # 1-margin. In the hidden space, a proportion prop of the points is taken as
  # outliers. Suggestion: add a verbose mode 
  sanitycheck.generate(n=n, prop=prop, stream.config=stream.config)

  if(is.null(stream.config)) {
    stream.config <- generate.stream.config(nstep=1)
  } else {
    if(stream.config$nstep != 1) {
      stop("The stream.config file is not compatible with static streams:" + 
           " nstep should be = 1")
    }
  }
  dim <- stream.config$dim
  subspaces <- stream.config$subspaces
  margins <- stream.config$margins
  dependency <- stream.config$dependency
  discretize <- stream.config$discretize
  allowOverlap <- stream.config$allowOverlap

  meta <- generate.multiple.rows(n, dim, subspaces, margins, prop,
                                 proptype=proptype, dependency=dependency,
                                 discretize=discretize)

  res <- list("data"=meta$data,"labels"=meta$labels, "n"=n, "prop"=prop,
              "proptype"=proptype, "allowOverlap"=allowOverlap,
              "stream.config"=stream.config)

  attr(res, "class") <- "stream"
  return(res)
}

#' Generate Dynamic Data Stream
#'
#' Generate a new synthetic multidimensional dynamic data stream having the
#' desired properties.
#'
#' @param n A vector containing \code{x} values, where the values corresponds to
#'        the number of points for each step and \code{x} to the number of steps.
#' @param prop Proportion of outliers in the hidden space.
#' @param proptype Type of the proportion of outliers. Value "proportional":
#'        depend on the size of the empty space. Value "absolute": same absolute
#'        proportion per subspace.  
#' @param stream.config A stream configuration object. Should have been
#'        generated with \code{nstep > 1}.
#' @param verbose If TRUE, then the state of the stream will be printed as
#'        output for every 100 points.
#' @param coldstart If TRUE (default) all subspaces will start with a margin
#'        value of 0.
#' @param transition A string indication what kind of transition should occur.
#'        Can be "Linear" (default) or "Abrupt".
#'
#' @return A an object of class \code{stream}, which is a \code{List} of 5
#'         elements.
#' - \code{data} contains the stream generated
#' - \code{labels} contains the description of each point (\code{0} if the point
#'     is not an outlier, or the subspace in which it is outlying as a string)
#' - \code{n} the number of points at each step
#' - \code{prop} the proportion of outliers in the hidden space
#' - \code{stream.config} the associated stream configuration object (which is
#'     valid only for dynamic streams)
#'
#' @details
#' The data is generated uniformly, except in certain subspaces where the data
#' is concentrated in particular dependencies (i.e. in the "Wall" dependency,
#' data concentrates on the axes, in a L-like shape). This should create spaces
#' with high dependency and space to observe hidden outliers. Note that the
#' proportion of outlier \code{prop} does not relate directly to the percentage
#' of outliers in the output stream. Since it corresponds to the probability of
#' a point, being ALREADY in the hidden space to stay where it is, the overall
#' proportion of outliers depends on the hidden space volume, which depends
#' on the number of subspaces and their margins. The greater the margin, the
#' bigger the hidden space. 
#'
#' @examples
#' # Generate a stream with default parameters 
#' stream <- generate.dynamic.stream()
#' # Generate a stream with custom configuration
#' stream.config <- generate.stream.config(dim=50, nstep=10, volatility=0.5)
#' stream <- generate.dynamic.stream(n=100, prop=0.05,
#'                                   stream.config=stream.config)
#' # Output stream results (to uncomment)
#' # output.stream(stream, "example")
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @seealso
#' * \code{\link{generate.stream.config}}  : generate a stream.config file for a
#'   dynamic or static stream 
#'
#' @md
#' @export
generate.dynamic.stream <- function(n=100, prop=0.01, proptype="proportional",
                                    stream.config=NULL, verbose=FALSE,
                                    coldstart=TRUE, transition="Linear") {
  sanitycheck.generate(n=n, prop=prop, stream.config=stream.config,
                       verbose=verbose)

  if(is.null(stream.config)) {
    stream.config <- generate.stream.config()
  } else {
    if(stream.config$nstep <= 1) {
      stop("The stream.config file in not compatible with dynamic streams:" +
           " nstep should be > 1")
    }
  }

  if(length(n) == 1) {
    n <- rep(n, stream.config$nstep)
  } # else assume that n has the good size, was checked by the sanity check 

  dim <- stream.config$dim
  subspaceslist <- stream.config$subspaceslist
  marginslist <- stream.config$marginslist
  dependency <- stream.config$dependency
  discretize <- stream.config$discretize
  allowOverlap <- stream.config$allowOverlap

  data <- data.frame()
  labels <- c()
  # Generate some data for each time step description 
  for(seq in 1:length(n)) {
    if(verbose) print(paste("Step", seq, "of", length(n), ". Size", n[[seq]],
                            "elements."))
    # Determine for the current state step the start and end margins values for
    # each subspaces 
    subspaces_state <- list()  # Indicates if the subspace has a dependency.
    currentmargins <- list()  # The start margin-value of a step.
    nextmargins <- list()  # The end margin-value of a step.
    
    if(seq == 1) {
      # If we want a coldstart, the starting margins values will be 0 for all
      # subspaces. Otherwise, the provided value is used.
      # TODO @apoth: Check, if this influences whether a drift is possible in
      #              from the first to the second step.
      subspaces_state <- subspaceslist[[seq]]
      if(coldstart) {
        currentmargins <- c(rep(0, length(subspaceslist[[seq]])))
      } else {
        currentmargins <- marginslist[[seq]]
      }
      nextmargins <- marginslist[[seq]]
    } else {
      # We shall consider subspace from the previous and the next state 
      subspaces_state <- unique(c(subspaceslist[[seq - 1]],
                                  subspaceslist[[seq]]))
      
      for(sub in 1:length(subspaces_state)) {
        # In the case a subspace is contained in the next step, its intended
        # value should be equal to its margins in the next step.
        # Otherwise, it should be 0.
        if(any(sapply(subspaceslist[[seq]],
                      function(x) setequal(x, subspaces_state[[sub]])))) {
          nextmargins <- c(nextmargins,
                           marginslist[[seq]][sapply(subspaceslist[[seq]],
                              function(x) setequal(x, subspaces_state[[sub]]))])
        } else {
          nextmargins <- c(nextmargins, 0)
        }
        
        # In the case a subspace is contained in the current step, its start
        # value should be equal to its margins in the current step.
        # Otherwise, it should be 0.
        if(any(sapply(subspaceslist[[seq - 1]],
                      function(x) setequal(x,subspaces_state[[sub]])))) {
          currentmargins <- c(currentmargins,
                         marginslist[[seq - 1]][sapply(subspaceslist[[seq - 1]],
                               function(x) setequal(x,subspaces_state[[sub]]))])
        } else {
          currentmargins <- c(currentmargins, 0)
        }
      }
    }
    currentmargins <- as.list(currentmargins)
    nextmargins <- as.list(nextmargins)
    
    i <- 0
    for(x in 1:n[[seq]]) {
      # TODO @apoth: Add new transition types here!
      #
      # Update the current margins (transitioning uniformly between
      # currentmargins and nextmargins)
      if(transition == "Linear") {
      margins_state <- as.list(unlist(currentmargins) -
                               (unlist(currentmargins) - 
                                unlist(nextmargins)) * (x - 1) / n[[seq]])
      } else if(transition == "Abrupt") {  # Leave the margins as they are
        margins_state <- currentmargins
      } else {
        stop("Unknown transition type specified.")
      }
      if(i %% 100 == 0 & verbose) {
        print(c("subspaces_state:", paste(subspaces_state)), collapse=" ")
        #print(c("currentmargins:", paste(currentmargins)), collapse=" ")
        print(c("margins_state:", paste(margins_state)), collapse=" ")
        #print(c("nextmargins:", paste(nextmargins)), collapse=" ")
      }
      i <- i + 1
      
      # Generate a row 
      res <- generate.row(dim=dim, subspaces=subspaces_state,
                          margins=margins_state, prop=prop, proptype=proptype,
                          dependency=dependency, discretize=discretize)
      data <- rbind(data, t(res$data))
      labels <- c(labels, res$label)
    }
  }
  # Put adequate names on the columns 
  attributes(data)$names <- c(c(1:dim),"class")
  res <- list("data"=data,"labels"=labels, "n"=n, "prop"=prop,
              "proptype"=proptype, "allowOverlap" = allowOverlap,
              "stream.config"=stream.config)
  
  attr(res, "class") <- "stream"
  return(res)
}
