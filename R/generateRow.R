#' Generate a raw with statistical properties
#'
#' @param dim Number of dimension of the generated vector
#' @param subspaces List of subspaces that contain a dependency.
#' @param margins List of margins that correspond to each subspace.
#' @param dependency Type of dependency for the subspaces (We don't support 'mixed') currently 
#' @param prop Probability of a point belonging to the hidden space of a subspace to become an outlier.
#' @param proptype Type of the proportion of outliers. Value "proportional": depend on the size of the empty space. Value "absolute": same absolute proportion per subspace. 
#'
#' @return A list with 2 elements where \code{data} contains the generated vector and \code{labels} the corresponding label.
#
#' @details
#' The row is at first drawn from the uniform distribution between 0 and 1 over each dimension
#' For each subspace in \code{subspaces}, if the point belongs to the hidden space of the specified dependency 
#' (i.e. for the "Wall", the value for each dimension of the subspace if bigger than the 1-margin), 
#' then it becomes an outlier with probability \code{prop}. If it is an outlier, it would stay in the hidden space 
#' and we make sure it is not too close from the border by rescaling the value so that it is at least 10% of the hidden space away from it. 
#' If it is not an outlier, we map uniformly the point to the dependencies (i.e. for "Wall", one of the axis or center (for a 3-D dimensions))
#' The probability to be mapped to one of such element is determined by their volume
#' The bigger the area, the more likely the point will be mapped to the area. 
#' In the case a point is already an outlier in a subspace, it cannot become and outlier in an overlapping subspace
#' But it still has the same probability to become an outlier in another space. This appends occasionally (pigeonhole principle)
#'
#' If the point if not an outlier, it will have \code{0} as label, otherwise, it has a string representing the different subspaces
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
  
  generate.row.subspace.Wall <- function(row, subspace) { 
    isInHiddenSpace.Wall <- function(row, subspace) {all(row[subspaces[[subspace]]] > 1-margins[[subspace]])}
    # Do something only if the point is already in the hidden region
    if(isInHiddenSpace.Wall(row, subspace) || (proptype=="absolute")) { 
      # Do something only if the point is not already an outlier in any other subspace that has non-empty intersection 
      # If this is the case, we might destroy his outlying behavior in the other subspace, which we want to avoid. 
      if(!any(lapply(subspaces[outlierFlags], function(y) length(intersect(subspaces[[subspace]],y))) > 0)) {
        outlierFlags[subspace] <- sample(c(TRUE,FALSE),1,prob=c(prop,1-prop)) # Choose if it is an outlier in this subspace, with probability prob
        if(!outlierFlags[subspace]) { # If we decide not to make an outlier out of it, move it to the L-shaped cluster 
          # THIS IS A PREVIOUS WAY TO DO IT. IT MIGHT BE MORE EFFICIENT BUT IT IS MORE COMPLICATED. 
          # Function generating the volumes of hypercubes of for subspace x of dimension length(subspaces[[x]])
          #volume_function <- function(y) (1-margins[[x]])^y*margins[[x]]^(length(subspaces[[x]])-y)*choose(length(subspaces[[x]]),y)
          # Choose how many dimension we exclude (i.e., put it in an arm of the L or at the intersection, uniformly)
          # The number of dimensions to exclude is chosen with a probability such that it is uniform with the space of the margin
          # in the respective projections. 
          #nb_exclude <- sample(0:(length(subspaces[[x]])-1), 1, prob=sapply(length(subspaces[[x]]):1, volume_function))
          #exclude <- sample(1:length(subspaces[[x]]),nb_exclude)
          #ifelse(nb_exclude != 0, s <- subspaces[[x]][-exclude], s <- subspaces[[x]])
          # Rescale uniformly the values of the attributes we did not exclude to fit in the range of the margin 
          #r[s] <- (r[s]-(1-margins[[x]]))*(1-margins[[x]])/margins[[x]]
          while(isInHiddenSpace.Wall(row, subspace)) {
            row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
          }
        } else {
          if(margins[[subspace]] >= 0.1) {
            while(!isInHiddenSpace.Wall(row, subspace)) {
              row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
            }
            # If we decide to make an outlier out of it:
            # Add a little margin (20% of the hidden space) to avoid possible ambiguities about the nature of the outlier 
            row[subspaces[[subspace]]] <- row[subspaces[[subspace]]] + (1-row[subspaces[[subspace]]])*0.2
          }
        }
      }
    }
    row
  }
  
  generate.row.subspace.Square <- function(row, subspace) { 
    isInHiddenSpace.Square <- function(row, subspace) {all(abs(row[subspaces[[subspace]]]-0.5) < margins[[subspace]]/2)}
    # Do something only if the point is already in the hidden region
    if(isInHiddenSpace.Square(row, subspace) || (proptype=="absolute")) { 
      # Do something only if the point is not already an outlier in any other subspace that has non-empty intersection 
      # If this is the case, we might destroy his outlying behavior in the other subspace, which we want to avoid. 
      if(!any(lapply(subspaces[outlierFlags], function(y) length(intersect(subspaces[[x]],y))) > 0)) {
        outlierFlags[subspace] <- sample(c(TRUE,FALSE),1,prob=c(prop,1-prop)) # Choose if it is an outlier in this subspace, with probability prob
        if(!outlierFlags[subspace]) { # If we decide not to make an outlier out of it, regenerate it outside from the hidden region 
          while(isInHiddenSpace.Square(row, subspace)) {
            row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
          }
        } else {
          if(margins[[subspace]] >= 0.1) {
            while(!isInHiddenSpace.Square(row, subspace)) {
              row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
            }
            # If we decide to make an outlier out of it:
            # Add a little margin (20% of the hidden space) to avoid possible ambiguities about the nature of the outlier 
            transformed_r <- row[subspaces[[subspace]]]-0.5
            transformed_r <- transformed_r * ((margins[[subspace]]/2)-(margins[[subspace]]/2)*0.2) / (margins[[subspace]]/2)
            row[subspaces[[subspace]]] <- transformed_r + 0.5
          }
        }
      }
    }
    row
  }
  
  # DEPRECATED
  # I wrote than when I tried to generate Donuts, but turns out it produces some kind of parallel triangle. Interesting.
  generate.row.subspace.Triangle <- function(row, subspace) { 
    # Do something only if the point is already in the hidden region
    if(sum(row[subspaces[[subspace]]]-0.5)**2 < margins[[subspace]]/2 | sum(row[subspaces[[subspace]]]-0.5)**length(subspaces[[subspace]]) > 1) { 
      # Do something only if the point is not already an outlier in any other subspace that has non-empty intersection 
      # If this is the case, we might destroy his outlying behavior in the other subspace, which we want to avoid. 
      if(!any(lapply(subspaces[outlierFlags], function(y) length(intersect(subspaces[[subspace]],y))) > 0)) {
        outlierFlags[subspace] <- sample(c(TRUE,FALSE),1,prob=c(prop,1-prop)) # Choose if it is an outlier in this subspace, with probability prob
        if(!outlierFlags[subspace]) { # If we decide not to make an outlier out of it, regenerate it outside from the hidden region 
          while(sum(row[subspaces[[subspace]]]-0.5)**2 < margins[[subspace]]/2 | sum(row[subspaces[[subspace]]]-0.5)**length(subspaces[[subspace]]) > 1) {
            row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
          }
        } else {
          # If we decide to make an outlier out of it:
          # Add a little margin (20% of the hidden space) to avoid possible ambiguities about the nature of the outlier 
          # Distinguish between "the point is inside the sphere" and "the point is in the corner"
          if(sum(row[subspaces[[subspace]]]-0.5)**2 < margins[[subspace]]/2) {
            current_radius = sum(row[subspaces[[subspace]]]-0.5)**2
            new_radius = current_radius - current_radius*0.2
            row[subspaces[[subspace]]] = row[subspaces[[subspace]]] * new_radius / current_radius
          } else {
            # Push it (probably not what I want to do but ok)
            current_radius = sum(row[subspaces[[subspace]]]-0.5)**2
            new_radius = current_radius + current_radius*0.2
            row[subspaces[[subspace]]] = row[subspaces[[subspace]]] * new_radius / current_radius
          }
        }
      }
    }
    r
  }
  
  generate.row.subspace.Donut <- function(row, subspace) { 
    # Do something only if the point is already in the hidden region
    isInHiddenSpace.Donut <- function(row, subspace) {sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) < margins[[subspace]]/2 | sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) > 0.5}
    if(isInHiddenSpace.Donut(row, subspace) || (proptype=="absolute")) { 
      # Do something only if the point is not already an outlier in any other subspace that has non-empty intersection 
      # If this is the case, we might destroy his outlying behavior in the other subspace, which we want to avoid. 
      if(!any(lapply(subspaces[outlierFlags], function(y) length(intersect(subspaces[[subspace]],y))) > 0)) {
        outlierFlags[subspace] <- sample(c(TRUE,FALSE),1,prob=c(prop,1-prop)) # Choose if it is an outlier in this subspace, with probability prob
        if(!outlierFlags[subspace]) { # If we decide not to make an outlier out of it, regenerate it outside from the hidden region 
          while(isInHiddenSpace.Donut(row, subspace)) {
            row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
          }
        } else {
          if(margins[[subspace]] >= 0.1) { 
            while(!isInHiddenSpace.Donut(row, subspace)) {
              row[subspaces[[subspace]]] <- runif(length(subspaces[[subspace]])) 
            }
            # If we decide to make an outlier out of it:
            # Add a little margin (30% of the hidden space) to avoid possible ambiguities about the nature of the outlier 
            # Distinguish between "the point is inside the sphere" and "the point is in the corner"
            if(sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) < margins[[subspace]]/2) { # If it is INSIDE the donut, push it towards the center
              absr <- abs(row[subspaces[[subspace]]]-0.5)
              row[subspaces[[subspace]]] = ((row[subspaces[[subspace]]]-0.5) * absr/(absr + (0.5-absr)*0.3)) + 0.5
            } else { # If it is OUTSIDE the Donut, push it towards the corners 
              absr <- abs(row[subspaces[[subspace]]]-0.5)
              row[subspaces[[subspace]]] = ((row[subspaces[[subspace]]]-0.5) * (absr + (0.5-absr)*0.3)/absr) + 0.5
            }
          }
        }
      }
    }
    row
  }
  
  r <- runif(dim)
  outlierFlags <- rep(FALSE, length(subspaces))
  for(s in 1:length(subspaces)) {
    if(dependency == "Wall") {
      r <- generate.row.subspace.Wall(r, s)
    } else if(dependency == "Square") {
      r <- generate.row.subspace.Square(r, s)
    } else if(dependency == "Donut") {
      r <- generate.row.subspace.Donut(r, s)
    } else {
      stop("Currently unsupported dependency type")
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
    # Here we need to take into account that the margin where modified by discretization. 
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





#' Helper function that generate multiple rows with the same characteristics
#' Useful to generate a static stream
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