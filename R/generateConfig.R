#' Random generation of partly overlapping consecutive subspaces with a minimum number of dimensions \code{mindim} 
#' and a maximum number of dimensions \code{maxdim} from a total number of dimensions \code{dim}. 
#'
#' @param dim Total number of dimensions from which to generate the subspaces.
#' @param mindim Minimum number of dimensions for a subspace.
#' @param maxdim Maximum number of dimensions for a subspace.
#'
#' @return A list of partly overlapping subspaces spanning over the \code{dim} dimensions 
#'
#' @details
#' Generate at most \code{round((dim*0.8)/length(mindim:maxdim)/x)} subspaces of each size \code{x}
#' For example, for \code{generate.subspaces(dim=100, maxdim=5, mindim=2)}: 
#' * at most 4 5-dim subspaces 
#' * at most 5 4-dim subspaces
#' * at most 6 3-dim subspaces
#' * at most 10 2-dim subspaces
#' This is done so to make sure that roughly a least 0.8 of the dimensions are unaffected  
#' This also make sure the dimensions are not dominated by high-dimensional subspaces
#' TODO: Can this be improved?
#'
#' @examples
#' subspaces <- generate.subspaces(dim=100, maxdim=5, mindim=2)
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @md
#' @export
generate.subspaces <- function(dim=20, mindim=2, maxdim=4) {
  sanitycheck.config(dim=dim, mindim=mindim, maxdim=maxdim)

  for(x in maxdim:mindim) {
    n <- (dim*0.8)/length(mindim:maxdim)/x
    if(n < 1) {
      maxdim <- maxdim - 1
    }
  }
  if(maxdim < mindim) {
    stop("The range of dimensions is too big for the number of dimensions")
  }
  #print(maxdim)

  subspaces <- list()
  for(x in maxdim:mindim) {
    n <- round((dim*0.8)/length(mindim:maxdim)/x)
    if(n != 0) {
      for(y in 1:n) {
        possiblestart <- 1:(dim-x)
        for(i in 1:length(possiblestart)) {
          start <- sample(possiblestart,1)
          candidate <- start:(start+x-1)
          if(!any(sapply(subspaces, function(x) setequal(intersect(x, candidate),candidate)|setequal(intersect(x, candidate),x)))) {
            #print(paste("candidate:", paste(candidate, collapse= ',')))
            subspaces <- c(subspaces, list(candidate))
            break
          }
          possiblestart <- possiblestart[possiblestart != start]
        }
      }
    }
  }
  subspaces
}

#' Resample compatible subspaces to replace a set of subspaces.
#' The new subspaces should not include or be included in another subspace from the set. 
#' @param dim Total number of dimensions from which to generate the subspaces.
#' @param subspaces A list of generated subspaces.
#' @param indexes Positions of the subspaces to replace.
#'
#' @return The original set of subspaces, where the places at \code{indexes} where modified
#'
#' @examples 
#' subspaces <- list(c(1,2,3), c(3,4,5), c(7,8), c(11,12), c(15,16))
#' subspaces <- replace.subspaces(20, subspaces, indexes=c(2,4))
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @md
#' @export
replace.subspaces <- function(dim, subspaces, indexes) {
  # no sanity check, assumed to be done already 

  for(index in indexes) {
    x <- length(subspaces[[index]])
    possiblestart <- 1:(dim-x)
    for(i in 1:length(possiblestart)) {
      start <- sample(possiblestart,1)
      candidate <- start:(start+x-1)
      #print(paste("candidate:", paste(candidate, collapse= ',')))
      if(!any(sapply(subspaces, function(x) setequal(intersect(x, candidate),candidate)|setequal(intersect(x, candidate),x)))) {
        subspaces[[index]] <- candidate
        break
      }
      possiblestart <- possiblestart[possiblestart != start]
    }
  }
  subspaces
}

#' Sample valid margins for a list of subspaces (not used currently)
#' @param subspaces A list of generated subspaces.
#' @param values A vector of valid values for subspace margins.
#'
#' @return A vector containing margins values for each subspaces
#'
#' @examples
#' generate.margins(list(c(1,2,3), c(4,5), c(5,6)), values=c(0.1,0.2,0.3,0.4,0.5,0.8,0.7,0.9))
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @md
#' @export
generate.margins <- function(subspaces, values=c(0.1,0.2,0.3,0.4,0.5,0.8,0.7,0.9)) {
  sanitycheck.config(values=values) # assume subspaces are already OK. 
  # Generate  margins for a list of subspaces. 
  sample(values, length(subspaces), replace=TRUE)
}

#' Generate iteratively a list of margins for the same set of subspaces with a particular volatility (not used currently)
#' @param subspaces A list of generated subspaces.
#' @param nstep Number of steps for which we generate new margin values.
#' @param volatility Percentage of subspaces to replace at each step.
#' @param values A vector of valid values for subspace margins.
#'
#' @return A list containing lists of margins for each step 
#'
#' @examples
#' generate.marginslist(list(c(1,2,3), c(4,5), c(5,6), c(7,8)), nstep=3, volatility=0.5)
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @md
#' @export
generate.marginslist <- function(subspaces, nstep=10, volatility=0.1, values=c(0.1,0.2,0.3,0.4,0.5,0.8,0.7,0.9)) {
  # Generate a marginslist for the same subspace list. Replace length(subspaces) * volatility margins at each step 
  sanitycheck.config(subspaces=subspaces, nstep=nstep, volatility=volatility, values=values)
  marginslist = list()
  for(n in 1:nstep) {
    ifelse(n==1, margins <- rep(1, length(subspaces)),margins <- marginslist[[n-1]])
    tochange <- sample(1:length(subspaces),max(1, as.integer(length(subspaces)*volatility)))
    newval <- sapply(margins[tochange], function(x) sample(values[values != x], 1))
    margins[tochange] <- newval 
    marginslist[[n]] <- margins
  }
  marginslist
}

#' Generate iteratively a list of subspaces and margins based on the initial set of subspaces with a particular volatility
#'
#' @param dim Total number of dimensions from which to generate the subspaces.
#' @param subspaces A list of generated subspaces.
#' @param nstep Number of steps for which we generate new subspaces and margin values.
#' @param volatility Percentage of subspaces to replace at each step.
#' @param values A vector of valid values for subspace margins.
#' @param cycle If > 0 the dynamic contains a cycle of this size. 
#'
#' @return A list with two elements:
#  - \code{marginlist} A list containing lists of margins for each steps
#' - \code{subspacelist} A list containing lists of subspaces for each steps
#' 
#' @details
#' For each step produce a new set of (possibly overlapping) subspaces where length(subspaces)*volatility subspaces were changed
#' and assigned a margin taken from values 
#' 
#' @examples
#' subspaces <- list(c(1,2,3), c(3,4), c(5,6,7,8), c(7,8,9))
#' meta <- generate.dynamic(dim=20, subspaces=subspaces, nstep=3, volatility=0.5)
#' meta_cycle <- generate.dynamic(dim=20, subspaces=subspaces, nstep=10, volatility=0.5, cycle=3)
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @md
#' @export
generate.dynamic <- function(dim, subspaces, nstep=10, volatility=0.1, values=c(0.1,0.2,0.3,0.4,0.5,0.8,0.7,0.9), cycle=0) {
  sanitycheck.config(dim=dim, subspaces=subspaces, nstep=nstep, volatility=volatility, values=values, cycle=cycle)

  if(cycle==0) {
    marginslist <- list(as.list(sample(values, length(subspaces), replace=TRUE)))
    subspaceslist <- list(subspaces)
    for(n in 1:(nstep-1)) {
      indexes <- sample(1:length(subspaceslist[[n]]), max(c(floor(length(subspaceslist[[n]]))*volatility,1)))
      nextsubspaces <- replace.subspaces(dim, subspaceslist[[n]], indexes)
      nextmargins <- marginslist[[n]]
      nextmargins[indexes] <- sample(values, length(indexes), replace=TRUE)
      subspaceslist[[n+1]] <- nextsubspaces 
      marginslist[[n+1]] <- nextmargins
    }
    res <- list("marginslist" = marginslist, "subspaceslist" = subspaceslist)
  } else {
    cycle <- generate.dynamic(dim=dim, subspaces=subspaces, nstep=cycle, volatility=volatility, values=values)
    marginslist_cycle <- c(cycle$marginslist, rev(cycle$marginslist[2:(length(cycle$marginslist)-1)])) # close the marginslist_cycle
    subspaceslist_cycle <- c(cycle$subspaceslist, rev(cycle$subspaceslist[2:(length(cycle$subspaceslist)-1)])) # close the subspaceslist_cycle
  
    res <- list("marginslist" = rep(marginslist_cycle, ceiling(nstep/length(marginslist_cycle)))[1:nstep], 
                "subspaceslist" = rep(subspaceslist_cycle, ceiling(nstep/length(marginslist_cycle)))[1:nstep])
  }
  res
}

#' Generate a valid stream configuration.
#'
#' @param dim Number of dimensions in the stream.
#' @param mindim Minimum number of dimensions for each subspaces. Should be >= 2.
#' @param maxdim Maximum number of dimensions for each subspaces. Should be >= mindim and <= dim/2.
#' @param values A vector of valid values for subspace margins.
#' @param nstep Number of steps for which we generate new subspaces and margin values.
#' @param cycle Number of iterations to use to create a cyclic streams. If 0, then the stream has no cycle.
#' @param volatility Proportion of subspaces to change at any step. Should be > 0 and <= 1. 1 means that all subspaces and margins at changed a each step
#'
#' @return A stream.config object to be passed to a generate.dynamic.stream or generate.static.stream method 
#'
#' @examples
#' stream.config <- generate.stream.config(dim=50, nstep=10, volatility=0.5)
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @seealso
#' * \code{\link{generate.dynamic.stream}} : generate a dynamic stream
#' * \code{\link{generate.static.stream}}  : generate a static stream 
#'
#' @md
#' @export
generate.stream.config <- function(dim=20, mindim=2, maxdim=4, values=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), nstep=10, cycle=0, volatility=0.1) {
  sanitycheck.config(dim=dim, mindim=mindim, maxdim=maxdim, values=values, nstep=nstep, cycle=cycle, volatility=volatility)

  res <- list("dim"=dim, "mindim"=mindim, "maxdim"=maxdim, "values"=values, "nstep"=nstep, "cycle"=cycle, "volatility"=volatility)
  if(nstep > 1) { # nstep == 1 means we generate a static stream 
    subspaces <- generate.subspaces(dim,mindim,maxdim)
    meta <- generate.dynamic(dim=dim, subspaces=subspaces, nstep=nstep, volatility=volatility, values=values, cycle=cycle)
    subspaceslist <- meta$subspaceslist
    marginslist <- meta$marginslist
    res <- c(res, list("subspaceslist"=subspaceslist, "marginslist"=marginslist))
  } else {
    subspaces <- generate.subspaces(dim,mindim,maxdim)
    margins <- generate.margins(subspaces, values=values)
    res <- c(res, list("subspaces"=subspaces, "margins"=margins))
  }
  attr(res, "class") <- "stream.config"
  return(res)
}