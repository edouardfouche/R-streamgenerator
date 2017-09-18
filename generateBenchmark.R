setwd("~/git/R-streamgenerator")

library(devtools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load_all(dirname(rstudioapi::getActiveDocumentContext()$path))

library(stringr)

################################################ Data for external validation

generate.static.benchmark <- function(ndim, dependency, proptype, discretize=0, allowOverlap=FALSE, num=0) {
  stream.config <- generate.stream.config(dim=ndim, nstep=1, mindim=2, maxdim=5, values=c(0.5,0.6,0.7,0.8,0.9), dependency=dependency, discretize=discretize, allowOverlap=allowOverlap)
  stream <- generate.static.stream(n=1000, prop=0.01, proptype= proptype, stream.config=stream.config)
  
  if(allowOverlap) o <- "AOV"
  else o <- "NOV"
  
  if(discretize == 0) {
    category <- paste(dependency, proptype, o, sep="-")
  } else {
    category <- paste(dependency, proptype, o, "disc", str_pad(discretize,3,pad="0"), sep="-")
  }
  name <- paste("synth", category, str_pad(ndim,3,pad="0"), str_pad(num,3,pad="0"), sep="_")
  output.stream(stream,name)
}

generate.dependency.dim <- function(dependency, proptype, allowOverlap=FALSE) {
  for(dim in c(10,40,70,100,250)) {
    for(x in c(0,1,2)) {
      generate.static.benchmark(dim, dependency, proptype=proptype, allowOverlap=allowOverlap, num=x)
    }
  }
}

generate.dependency.discrete <- function(dependency, proptype, allowOverlap=FALSE) {
  for(disc in c(10,25,50,75,100)) {
    for(x in c(0,1,2)) {
      generate.static.benchmark(100, dependency, proptype=proptype, discretize= disc, allowOverlap=allowOverlap, num=x)
    }
  }
}


setwd("~/data/New-synth-proportional")

################### Statics streams 

########## Real-valued data sets propotional

##### Wall dependency 
generate.dependency.dim("Wall", "proportional")
##### Square dependency 
generate.dependency.dim("Square", "proportional")
##### Donut
generate.dependency.dim("Donut", "proportional")

########## Discrete data sets propotional

##### Wall dependency 
generate.dependency.discrete("Wall", "proportional")
##### Square dependency 
generate.dependency.discrete("Square", "proportional")
##### Donut
generate.dependency.discrete("Donut", "proportional")



setwd("~/data/New-synth-absolute")

################### Statics streams 

########## Real-valued data sets absolute proportion

##### Wall dependency 
generate.dependency.dim("Wall", "absolute")
##### Square dependency 
generate.dependency.dim("Square", "absolute")
##### Donut
generate.dependency.dim("Donut", "absolute")

########## Discrete data sets absolute proportion

##### Wall dependency 
generate.dependency.discrete("Wall", "absolute")
##### Square dependency 
generate.dependency.discrete("Square", "absolute")
##### Donut
generate.dependency.discrete("Donut", "absolute")

setwd("~/data/New-synth-absolute-overlap")

########## Real-valued data sets with overlap

##### Wall dependency 
generate.dependency.dim("Wall", "absolute", allowOverlap=TRUE)
##### Square dependency 
generate.dependency.dim("Square", "absolute", allowOverlap=TRUE)
##### Donut
generate.dependency.dim("Donut", "absolute", allowOverlap=TRUE)

########## Discrete data sets with overlap 

##### Wall dependency 
generate.dependency.discrete("Wall", "absolute", allowOverlap=TRUE)
##### Square dependency 
generate.dependency.discrete("Square", "absolute", allowOverlap=TRUE)
##### Donut
generate.dependency.discrete("Donut", "absolute", allowOverlap=TRUE)

############################################################################## Data for internal validation 

generate.internal.benchmark <- function(dependency, nDim, discrete) {
  if(!discrete) {
    for(x in 0:9) {
      stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency=dependency, discretize=0, allowOverlap=FALSE)
      stream.config$subspaces <- list(c(1:nDim))
      stream.config$margins <- list(c(x/10))
      stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
      #if(nDim==2) plot(stream$data[,1:2])
      write.csv(stream$data[,1:nDim], paste(nDim,"D",dependency, x, ".csv", sep=""), row.names = FALSE)
    }
  } else {
    for(disc in c(10,25,50,75,100)) {
      x <- 5
      stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency=dependency, discretize=disc, allowOverlap=FALSE)
      stream.config$subspaces <- list(c(1:nDim))
      stream.config$margins <- list(c(x/10))
      stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
      #if(nDim==2) plot(stream$data[,1:2])
      write.csv(stream$data[,1:nDim], paste(nDim,"D",dependency, "disc", disc, "-", x, ".csv", sep=""), row.names = FALSE)
    }
  }
}

setwd("~/data/dep")

generate.internal.benchmark("Wall", 2, discrete=FALSE)
generate.internal.benchmark("Wall", 3, discrete=FALSE)
generate.internal.benchmark("Wall", 4, discrete=FALSE)
generate.internal.benchmark("Wall", 5, discrete=FALSE)

generate.internal.benchmark("Square", 2, discrete=FALSE)
generate.internal.benchmark("Square", 3, discrete=FALSE)
generate.internal.benchmark("Square", 4, discrete=FALSE)
generate.internal.benchmark("Square", 5, discrete=FALSE)

generate.internal.benchmark("Donut", 2, discrete=FALSE)
generate.internal.benchmark("Donut", 3, discrete=FALSE)
generate.internal.benchmark("Donut", 4, discrete=FALSE)
generate.internal.benchmark("Donut", 5, discrete=FALSE)

generate.internal.benchmark("Wall", 2, discrete=TRUE)
generate.internal.benchmark("Wall", 3, discrete=TRUE)
generate.internal.benchmark("Wall", 4, discrete=TRUE)
generate.internal.benchmark("Wall", 5, discrete=TRUE)

generate.internal.benchmark("Square", 2, discrete=TRUE)
generate.internal.benchmark("Square", 3, discrete=TRUE)
generate.internal.benchmark("Square", 4, discrete=TRUE)
generate.internal.benchmark("Square", 5, discrete=TRUE)

generate.internal.benchmark("Donut", 2, discrete=TRUE)
generate.internal.benchmark("Donut", 3, discrete=TRUE)
generate.internal.benchmark("Donut", 4, discrete=TRUE)
generate.internal.benchmark("Donut", 5, discrete=TRUE)


###### Generate many depSquare with different dependencies
setwd("~/data/depSquare")

for(x in 0:99) {
  stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency="Square", discretize=0, allowOverlap=FALSE)
  stream.config$subspaces <- list(c(1:2))
  stream.config$margins <- list(c(x/100))
  stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
  #if(nDim==2) plot(stream$data[,1:2])
  write.csv(stream$data[,1:2], paste("2DSquare", str_pad(x,2,pad="0"), ".csv", sep=""), row.names = FALSE)
}

setwd("~/data/depDonut")

for(x in 0:99) {
  stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency="Donut", discretize=0, allowOverlap=FALSE)
  stream.config$subspaces <- list(c(1:2))
  stream.config$margins <- list(c(x/100))
  stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
  #if(nDim==2) plot(stream$data[,1:2])
  write.csv(stream$data[,1:2], paste("2DDonut", str_pad(x,2,pad="0"), ".csv", sep=""), row.names = FALSE)
}

setwd("~/data/depWall")

for(x in 0:99) {
  stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency="Wall", discretize=0, allowOverlap=FALSE)
  stream.config$subspaces <- list(c(1:2))
  stream.config$margins <- list(c(x/100))
  stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
  #if(nDim==2) plot(stream$data[,1:2])
  write.csv(stream$data[,1:2], paste("2DWall", str_pad(x,2,pad="0"), ".csv", sep=""), row.names = FALSE)
}


###### Just generate Square dependencies, non-overlapping 

generate.static.benchmark <- function(ndim, dependency, proptype, discretize=0, allowOverlap=FALSE, num=0) {
  stream.config <- generate.stream.config(dim=ndim, nstep=1, mindim=2, maxdim=5, values=c(0.5,0.6,0.7,0.8,0.9), dependency=dependency, discretize=0, allowOverlap=FALSE)
  stream <- generate.static.stream(n=1000, prop=0.005, proptype= proptype, stream.config=stream.config)
  name <- paste("synth", dependency, str_pad(ndim,3,pad="0"), str_pad(num,3,pad="0"), sep="_")
  output.stream(stream,name)
}

generate.dependency.dim <- function(dependency) {
  for(dim in c(10,20,30,40,50,75,100,250)) {
    for(x in c(0,1,2)) {
      generate.static.benchmark(dim, dependency, proptype="absolute", allowOverlap=FALSE, num=x)
    }
  }
}

setwd("~/data/Square-synth")
generate.dependency.dim("Square")

x <- runif(100)
y <- x
plot(x,y)
