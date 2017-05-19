# A bunch of sanity checks for the functions generating data streams
sanitycheck.generate <- function(n=100, prop=0.01, stream.config=NULL, verbose=FALSE) {
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(prop))
  stopifnot(is.logical(verbose))

  if(!is.null(stream.config)) {
    stopifnot(class(stream.config) == "stream.config")

    if(stream.config$nstep > 1) {
      if(length(stream.config$subspaceslist) != length(stream.config$marginslist)) {
        stop("Length of 'subspaceslist' and 'marginslist' need to be the same")
      }
      if(length(stream.config$nstep) != length(n) & length(n) != 1) {
        stop("Length of 'n' is not coherent with stream.config. Should be either 1 for a regular stream or a vector and same size as nstep")
      }
      if(length(stream.config$nstep) != length(n) & length(n) != 1) {
        stop("Length of 'n' is not coherent with stream.config. Should be either 1 for a regular stream or a vector and same size as nstep")
      }
      for(subspaces in stream.config$subspaceslist) {
        if(any(sapply(1:length(subspaces), function(x) any(sapply(subspaces[-x], function(y) setequal(intersect(y, subspaces[[x]]),y)))))) {
          stop("Subspaces may overlap but it is not allowed a subspace to be included in another")
        }
        if(any(sapply(subspaces, function(x) any(x > stream.config$dim)))) {
          stop("This is not possible to have subspaces where dimension numbers are bigger than the total number of dimensions")
        }
      }
    } else {
      if(length(stream.config$subspaces) != length(stream.config$margins)) {
        stop("Length of 'subspaces' and 'margins' need to be the same")
      }
      if(length(n) != 1) {
        stop("Length of 'n' should be 1 in the case of a static stream")
      }
      if(any(stream.config$margins < 0 | stream.config$margin > 1)) {
        stop("All margin in 'margins' should be > 0 and <= 1")
      }
      if(any(sapply(1:length(stream.config$subspaces), function(x) any(sapply(stream.config$subspaces[-x], function(y) setequal(intersect(y, stream.config$subspaces[[x]]),y)))))) {
        stop("Subspaces may overlap but it is not allowed a subspace to be included in another")
      }
      if(any(sapply(stream.config$subspaces, function(x) any(x > stream.config$dim)))) {
        stop("This is not possible to have subspaces where dimension numbers are bigger than the total number of dimensions")
      }
    }
  }

  if(any(n<=0)) {
    stop("The number of point per step(s) 'n' shall not contain values <= 0")
  }
  if(prop < 0 | prop > 1) {
    stop("The proportion of outliers in hidden space 'prop' should be >= 0 and <= 1")
  }

}

# A bunch of sanity checks for the functions generating configurations
sanitycheck.config <- function(dim=20, mindim=2, maxdim=4, subspaces=NULL, nstep=10, cycle=0, 
                               volatility=0.1, values=c(0.1,0.2,0.3,0.4,0.5,0.8,0.7,0.9)) {
  stopifnot(is.numeric(dim))
  stopifnot(is.numeric(mindim))
  stopifnot(is.numeric(maxdim))
  stopifnot(is.numeric(nstep))
  stopifnot(is.numeric(cycle))
  stopifnot(is.numeric(volatility))
  stopifnot(is.numeric(values))

  if(!is.null(subspaces)) {
    if(any(sapply(subspaces, function(x) any(x > dim)))) {
        stop("This is not possible to have subspaces where dimension numbers are bigger than the total number of dimensions")
    }
  }
  if(nstep <= 0) {
    stop("'nstep' should be > 0. Note: 1 means a static stream")
  }
  if(nstep==0 & cycle != 0) {
    stop("Defining a cycle does not make sense in a non-dynamic stream")
  }
  if(cycle < 0 | cycle > nstep/2) {
    stop("The number of elements to create a cycle 'cycle' should be between 0 (no cycle) and half of the number of steps 'nstep'")
  }
  if(dim < 10) {
    stop("The number of dimensions 'dim' should be at least >= 10")
  }
  if(mindim < 2) {
    stop("The minimum number of dimensions 'mindim' for a subspace shall not be lower than 2")
  }
  if(maxdim < mindim) {
    stop("The minimum number of dimensions 'mindim' for a subspace shall not be bigger than the maximum number of dimensions")
  }
  if(maxdim > dim/2) {
    stop("A maximum number of dimensions bigger than half of the total number of dimensions is not allowed")
  }
  if(volatility <= 0 | volatility > 1) {
    stop("The relative proportion at with the system change at each step should be > 0 and <= 1. Note that for a system where volatility = 0, we would use the method 'generate.static.data'")
  }
  if(any(values > 1) | any(values < 0)) {
    stop("Possibles margin values 'values' should be strictly between 0 and 1")
  }
  if(nstep < 0) {
    stop("The number of steps 'nstep' should be strictly positive")
  }
}