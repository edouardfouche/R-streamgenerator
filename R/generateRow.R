#' Generate a row with statistical properties
#'
#' @param dim Number of dimensions of the generated vector
#' @param subspaces List of subspaces that contain a dependency
#' @param margins List of margins that correspond to each subspace
#' @param dependency Type of dependency for the subspaces (We don't support 'mixed' currently)
#' @param prop Probability of a point belonging to the hidden space of a subspace to become an outlier
#' @param proptype Type of the proportion of outliers. Value "proportional": depends on the size of the empty space. Value "absolute": same absolute proportion per subspace. 
#'
#' @return A list with 2 elements where \code{data} contains the generated vector and \code{labels} the corresponding label.
#
#' @details
#' The row is at first drawn from the uniform distribution between 0 and 1 over each dimension.
#' For each subspace in \code{subspaces}, if the point belongs to the hidden space of the specified dependency 
#' (i.e. for the "Wall", the value for each dimension of the subspace if bigger than the 1-margin), 
#' it then becomes an outlier with probability \code{prop}. If it is an outlier, it would stay in the hidden space 
#' and we make sure it is not too close to the border by rescaling the value so that it is at least 10% of the hidden space away from it. 
#' If it is not an outlier, we map uniformly the point to the dependencies (i.e. for "Wall", one of the axis or center (for a 3-D dimensions)).
#' The probability to be mapped to one of such element is determined by their volume.
#' The bigger the area, the more likely the point will be mapped to the area. 
#' In the case a point is already an outlier in a subspace, it cannot become an outlier in an overlapping subspace
#' but it still has the same probability to become an outlier in another space. This appends occasionally (pigeonhole principle)
#'
#' If the point is not an outlier, it will have \code{0} as label, otherwise it has a string representing the different subspaces.
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @seealso
#' * \code{\link{generate.dynamic.stream}} : generate a dynamic stream
#' * \code{\link{generate.static.stream}}  : generate a static stream 
#'
#' @md
#' @importFrom stats runif
generate.row <- function(dim=10, subspaces=list(c(3,4), c(7,8)), margins=list(0.9,0.9), dependency = "Wall", prop=0.01, proptype="proportional", discretize=0) {
  # no sanity check, assumed to be done already 

  ########## some helper functions ############
  # Calculate eukklidian length of a vector
  getEuklidLen <- function(vec) {
    sqrt(sum(vec**2))
  }
  # Calculate vector projection for determing the distance of a point to a line
  getVectorProjection <- function(start, end, point) {
    diagonal <- end - start
    result <- list(va1=numeric(0), va2=numeric(0))
    result$va1 <- sum((point-start) * (diagonal/getEuklidLen(diagonal))) * (diagonal/getEuklidLen(diagonal))
    result$va2 <- (point-start) - result$va1
    result
  } 
  # Opposite point basically means mirrored aorund (0.5, 0.5)
  createOppositePoint <- function(start) {
    (start - 0.5) * (-1) + 0.5
  }
  # Create object containing the corners of a n-dimensional hypercube.
  # If makePretty is FALSE, the value of expand.grid is returned.
  # If makePretty is TRUE, a list of vectors representing the corners is returned.
  getCorners <- function(n=2, makePretty=FALSE) {
    # create all corners via expand.grid
    arguments <- vector("list", n)  # arguments for expand.grid
    for (i in 1:n) {
      arguments[[i]] <- c(0,1)
    }
    corners <- expand.grid(arguments)
    if (makePretty) {
      result <- vector("list", nrow(corners))
      for (i in 1:nrow(corners)) {
        result[[i]] <- as.vector(corners[i,], mode="integer")
      }
      result
    } else {
      corners
    }
  }

  #############################################
  isInHiddenSpace.Wall <- function(row, subspace) {all(row[subspaces[[subspace]]] > 1-margins[[subspace]])}
  isInHiddenSpace.Square <- function(row, subspace) {all(abs(row[subspaces[[subspace]]]-0.5) < margins[[subspace]]/2)}
  isInHiddenSpace.Donut <- function(row, subspace) {sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) < margins[[subspace]]/2 | sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) > 0.5}
  isInHiddenSpace.Linear <-function(row, subspace) {
    p <- row[subspaces[[subspace]]]
    b <- rep(1, length(p))
    # https://en.wikipedia.org/wiki/Vector_projection
    a1 <- sum(p*(b/sqrt(length(b))))
    va1 <- a1 * (b/sqrt(length(b)))
    vd <- p - va1 
    d <- sqrt(sum(vd**2))
    d > 0.5 - margins[[subspace]]/2
  }
  isInHiddenSpace.Cross <- function(row, subspace, marginFactor = 1) {
    createDiagList <- function(n=2) {
      result <- c(starts=vector("list", (n+1)), ends=vector("list", (n+1)))
      result$starts[[1]] <- rep(0, n)
      result$ends[[1]] <- rep(1, n)
      for (i in 2:(n+1)) {
        result$starts[[i]] <- rep(0, n)
        result$starts[[i]][[i-1]] <- 1
        result$ends[[i]] <- createOppositePoint(result$starts[[i]])
      }
      result
    }
    point <- row[subspaces[[subspace]]]
    if (marginFactor != 1) {point <- row}
    n <- length(subspaces[[subspace]])
    diagonals.list <- createDiagList(n)
    # T.B.D.
    diagonals.projections <- vector("list", (n+1))
    diffs <- vector("list", (n+1))
    for (i in 1:(n+1)) {
      diagonals.projections[[i]] <- getVectorProjectionOnDiagonal(diagonals.list$starts[[i]],
                                                                diagonals.list$ends[[i]],
                                                                point);
      diffs[i] <- getEuklidLen(diagonals.projections[[i]]$va2)
    }
    all(diffs > 0.5 - (margins[[subspace]]*marginFactor)/2)
  }
  isInHiddenSpace.Hourglass <- function(row, subspace, marginFactor = 1) {
    getHOPs <- function(start, n, oppDim) {
      corners <- getCorners(n=n, makePretty=FALSE)
      # create the "oppValue" for the desired dimension (oppDim)
      oppValue <- createOppositePoint(start[[oppDim]])
      # replace the value of the oppDim of all corners
      for (i in 1:nrow(corners)) {
        corners[i,][oppDim] <- oppValue
      }
      # remove duplicates
      corners <- unique(corners)
      # rename rows and transform to a list of vectors
      rownames(corners) <- 1:nrow(corners)
      result <- vector("list", nrow(corners))
      for (i in 1:nrow(corners)) {
        result[[i]] <- as.vector(corners[i,], mode="integer")
      }
      result
    }
    createHOPLines <- function(n=2) {
      # The hourglass lines are the vectors between a corner of the
      # hypercube and its ``HOPs'' (Hourglass Opposing Points). A HOP can be
      # any other corner of the hypercube other than the starting point as long
      # as the value of a pre-specified dimension is different (by only using
      # the corners, the values in question can only be either 0 or 1).
      #
      # Example: Assume we have a 3D Hourglass. 
      # We thus have vectors (x,y,z). For finding the HOPs of a corner, we
      # want x to be the opposite. Thus, the point (0,0,0) would have the following HOPs:
      # (1,0,0), (1,1,0), (1,0,1) and (1,1,1)
      # 
      # In general we have 2^(n-1) HOPs per corner in a n-dimensional hypercube.
      corners <- getCorners(n=n, makePretty=TRUE)
      result = vector("list", length(corners))
      for (i in 1:length(corners)) {
        result[[i]][["starts"]] <- corners[[i]]
        result[[i]][["endList"]] <- getHOPs(result[[i]][["starts"]], n, 1)
      }
      result
    }

    if (marginFactor != 1) {
        point <- row  # assume call from ensureOutlyingBehavior
    } else {
        point <- row[subspaces[[subspace]]]  # only look at the relevant subspace
    }
    n <- length(subspaces[[subspace]])
    line.list <- createHOPLines(n)
    numberOfCorners <- length(line.list)
    numberOfHOPs <- length(line.list[[1]][["endList"]])
    line.projections <- vector("list", length(line.list))
    diffs <- vector("list", (numberOfCorners * numberOfHOPs))
    k <- 1
    for (i in 1:numberOfCorners) {
        line.projections[[i]] <- vector("list", numberOfHOPs)
      for (j in 1:numberOfHOPs) {
        line.projections[[i]][[j]] <- getVectorProjection(line.list[[i]][["starts"]],
                                                          line.list[[i]][["endList"]][[j]],
                                                          point)
        diffs[k] <- getEuklidLen(line.projections[[i]][[j]]$va2)
        k <- k + 1
      }
    }
    all(diffs > 0.5 - ((margins[[subspace]]*marginFactor)/2))
  }


  ensureOutlyingBehavior.Wall <- function(row, subspace) row[subspaces[[subspace]]] + (1-row[subspaces[[subspace]]])*0.2
  ensureOutlyingBehavior.Square <- function(row, subspace) {
    transformed_r <- row[subspaces[[subspace]]]-0.5
    transformed_r <- transformed_r * ((margins[[subspace]]/2)-(margins[[subspace]]/2)*0.2) / (margins[[subspace]]/2)
    transformed_r + 0.5
  }
  ensureOutlyingBehavior.Donut <- function(row, subspace) { 
    # Distinguish between "the point is inside the sphere" and "the point is in the corner"
    if(sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) < margins[[subspace]]/2) { # If it is INSIDE the donut, push it towards the center
      absr <- abs(row[subspaces[[subspace]]]-0.5)
      res <- ((row[subspaces[[subspace]]]-0.5) * absr/(absr + absr*0.2)) + 0.5
    } else { # If it is OUTSIDE the Donut, push it towards the corners 
      absr <- abs(row[subspaces[[subspace]]]-0.5)
      res <- ((row[subspaces[[subspace]]]-0.5) * (absr + (0.5-absr)*0.3)/absr) + 0.5  # I've put 0.3 here because otherwise I find it too close really 
    }
    res
  }
  ensureOutlyingBehavior.Linear <- function(row, subspace){
    d <- 0 # Note: This way is a bit suboptimal but it avoids points at the really border at least. 
    while(!(d > 0.5 - (margins[[subspace]]*0.8)/2)) {
      p <- runif(length(row[subspaces[[subspace]]]))
      b <- rep(1, length(p))
      # https://en.wikipedia.org/wiki/Vector_projection
      a1 <- sum(p*(b/sqrt(length(b))))
      va1 <- a1 * (b/sqrt(length(b)))
      vd <- p-va1
      d <- sqrt(sum(vd**2))
    }
    p
  }
  ensureOutlyingBehavior.Cross <- function(row, subspace){
    d <- FALSE 
    while(!d) {
      p <- runif(length(row[subspaces[[subspace]]]))
      d <- isInHiddenSpace(p, subspace, marginFactor = 0.8)
    }
    p
  }
  ensureOutlyingBehavior.Hourglass <- function(row, subspace){
    d <- FALSE 
    while(!d) {
      p <- runif(length(row[subspaces[[subspace]]]))
      d <- isInHiddenSpace(p, subspace, marginFactor = 0.8)
    }
    p
  }
  ensureOutlyingBehavior.HG <- function(row, subspace) {
      d <- FALSE
      while(!d) {
          p <- runif(length(row[subspaces[[subspace]]]))
          d <- isInHiddenSpace(p, subspace, marginFactor=0.8)
      }
      p
  }
  
  
  outlierFlags <- rep(FALSE, length(subspaces))
  r <- runif(dim)
  
  for(s in 1:length(subspaces)) {
    if(dependency == "Wall") {
      isInHiddenSpace <- isInHiddenSpace.Wall
      ensureOutlyingBehavior <- ensureOutlyingBehavior.Wall 
    } else if(dependency == "Square") {
      isInHiddenSpace <- isInHiddenSpace.Square
      ensureOutlyingBehavior <- ensureOutlyingBehavior.Square
    } else if(dependency == "Donut") {
      isInHiddenSpace <- isInHiddenSpace.Donut
      ensureOutlyingBehavior <- ensureOutlyingBehavior.Donut
    } else if (dependency == "Linear") {
      isInHiddenSpace <- isInHiddenSpace.Linear
      ensureOutlyingBehavior <- ensureOutlyingBehavior.Linear 
    } else if (dependency == "Cross") {
      isInHiddenSpace <- isInHiddenSpace.Cross
      ensureOutlyingBehavior <- ensureOutlyingBehavior.Cross 
    } else if (dependency == "Hourglass") {
      isInHiddenSpace <- isInHiddenSpace.Hourglass
      ensureOutlyingBehavior <- ensureOutlyingBehavior.Hourglass
    } else if (dependency == "HG") {
      isInHiddenSpace <- isInHiddenSpace.HG
      ensureOutlyingBehavior <- ensureOutlyingBehavior.HG
    } else {
      stop("Currently unsupported dependency type")
    }
    
    # Do something only if the point is already in the hidden space OR the proportion of outliers is absolute. 
    if(isInHiddenSpace(r, s) || (proptype=="absolute")) { 
      # Do something only if the point is not already an outlier in any other subspace that has non-empty intersection 
      # If this is the case, we might destroy his outlying behavior in the other subspace, which we want to avoid. 
      if(!any(lapply(subspaces[outlierFlags], function(y) length(intersect(subspaces[[s]],y))) > 0)) {
        outlierFlags[s] <- sample(c(TRUE,FALSE),1,prob=c(prop,1-prop)) # Choose if it is an outlier in this subspace, with probability prob
        if(!outlierFlags[s]) { # If we decide not to make an outlier out of it, regenerate it outside of the hidden region 
          while(isInHiddenSpace(r, s)) {
            r[subspaces[[s]]] <- runif(length(subspaces[[s]])) 
          }
        } else {
          # Just to make sure we don't go in an infinite loop in case the margin is too small. 
          if(margins[[s]] >= 0.1) { 
            while(!isInHiddenSpace(r, s)) {
              r[subspaces[[s]]] <- runif(length(subspaces[[s]])) 
            }
            # If we decide to make an outlier out of it:
            # Add a little margin (20% of the hidden space) to avoid possible ambiguities about the nature of the outlier 
            r[subspaces[[s]]] <- ensureOutlyingBehavior(r,s)
          }
        }
      }
    }
  }
  
  
  
  ###################### Discretize
  if(discretize) {
    #r <- r - (r%%(1/discretize))
    discretizeIt <- function(x) {
      if(abs(x %% (1/(discretize-1))) > abs(x %% -(1/(discretize-1)))) {
        x <- x - (x %%-(1/(discretize-1)))
      } else {
        x <- x - (x %% (1/(discretize-1)))
      }
      x
    }
    for(x in 1:length(r)) {
      r[x] <- discretizeIt(r[x])
    }

    # Create the true labels for each point
    # Do the process afterwards to minimize bug risks
    # i.e. make the labeling process independent from the generation 
    # Here we need to take into account that the margin was modified by discretization. 
    for(x in 1:length(subspaces)) {
      if(dependency == "Wall") {
        outlierFlags[x] <- all(r[subspaces[[x]]] > (1-margins[[x]])+(1/(discretize-1)))
      } else if(dependency == "Square") {
        outlierFlags[x] <- all(abs(r[subspaces[[x]]]-0.5) < (margins[[x]]-(1/(discretize-1)))/2)
        #if(all(abs(r[subspaces[[x]]]-0.5) < (margins[[x]]-(margins[[x]] %% (1/(discretize-1))))/2)) {
        #  print(cat("vec:", r[subspaces[[x]]], "-->", abs(r[subspaces[[x]]]-0.5), "smaller than", (margins[[x]]-(margins[[x]] %% (1/(discretize-1))))/2 , "with margin", margins[[x]], "discretize:", discretize))
        #}
      } else if(dependency == "Donut") {
        outlierFlags[x] <- (sqrt(sum((r[subspaces[[x]]]-0.5)**2)) < margins[[x]]/2 - sqrt(((1/(discretize-1))**2)*2)/2) | (sqrt(sum((r[subspaces[[x]]]-0.5)**2)) > 0.5+sqrt(((1/(discretize-1))**2)*2)/2)
      } else if (dependency == "Linear"){
        outlierFlags[x] <- isInHiddenSpace.Linear(r,x)
      } else if (dependency == "Cross"){
        outlierFlags[x] <- isInHiddenSpace.Cross(r,x)
      } else if (dependency == "Hourglass") {
        outlierFlags[x] <- isInHiddenSpace.Hourglass(r,x)
      } else if (dependency == "HG") {
        outlierFlags[x] <- isInHiddenSpace.HG(r,x)
      } else {
        stop("Currently unsupported dependency type")
      }
    }
    
  } else {
    # Create the true labels for each point
    # Do the process afterwards to minimize bug risks
    # i.e. make the labeling process independent from the generation 
    for(x in 1:length(subspaces)) {
      if(dependency == "Wall") {
        outlierFlags[x] <- all(r[subspaces[[x]]] > (1-margins[[x]]))
      } else if(dependency == "Square") {
        outlierFlags[x] <- all(abs(r[subspaces[[x]]]-0.5) < margins[[x]]/2)
      } else if(dependency == "Donut") {
        outlierFlags[x] <- sqrt(sum((r[subspaces[[x]]]-0.5)**2)) < margins[[x]]/2 | sqrt(sum((r[subspaces[[x]]]-0.5)**2)) > 0.5
      } else if (dependency == "Linear") {
        outlierFlags[x] <- isInHiddenSpace.Linear(r,x)
      } else if (dependency == "Cross") {
        outlierFlags[x] <- isInHiddenSpace.Cross(r,x)
      } else if (dependency == "Hourglass") {
        outlierFlags[x] <- isInHiddenSpace.Hourglass(r,x)
      } else if (dependency == "HG") {
        outlierFlags[x] <- isInHiddenSpace.HG(r,x)
      } else {
        stop("Currently unsupported dependency type")
      }
    }
  }
  
  #################################
  

  
  # Generate a description for the outlier as an integer composed of the index of each subspaces separated by ;
  # In case the point is not an outlier in any space then just put a 0
  ifelse(any(outlierFlags), description <- paste(sapply(subspaces[outlierFlags],function(x) paste(x,collapse=",")),collapse=";"), description <- "0") 
  class <- ifelse(description == "0", 0, 1) # derive the class label 
  r <- c(r,class)
  # Row output 
  list("data"=r, "label"=description)
}





#'  Helper function that generates multiple rows.
#' 
#' Helper function that generates multiple rows with the same characteristics.
#' Useful to generate a static stream.
#'
#' @param n Number of rows to generate.
#' @param dim Number of dimension of the vector generate.
#' @param subspaces List of subspaces that contain a hidden space (i.e they show some dependency).
#' @param margins List of margins that correspond to each subspace.
#' @param prop Probability of a point belonging to the hidden space of a subspace to become an outlier.
#' @param proptype Type of the proportion of outliers. Value "proportional": depend on the size of the empty space. Value "absolute": same absolute proportion per subspace. 
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @md
#' @return A list with 2 elements where \code{data} is a data.frame object containing the \code{n} vectors and \code{labels} containing \code{n} corresponding labels. 
generate.multiple.rows <- function(n, dim, subspaces, margins, dependency, prop, proptype, discretize) {
  # no sanity check, assumed to be done already 
  data <- data.frame()
  labels <- c()
  for(x in 1:n) {
    res <- generate.row(dim=dim, subspaces=subspaces, margins=margins, dependency=dependency, prop=prop, proptype=proptype,  discretize=discretize)
    data <- rbind(data, t(res$data))
    labels <- c(labels, res$label)
  }
  attributes(data)$names <- c(c(1:dim),"class")
  list("data"=data, "labels"=labels)
}
