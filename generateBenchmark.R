setwd("~/git/R-streamgenerator")

library(devtools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load_all(dirname(rstudioapi::getActiveDocumentContext()$path))

library(stringr)

generate.static.benchmark <- function(ndim, dependency, proptype, discretize=0, allowOverlap=FALSE, num=0) {
  stream.config <- generate.stream.config(dim=ndim, nstep=1, mindim=2, maxdim=5, values=c(0.5,0.6,0.7,0.8,0.9), dependency="Wall", discretize=discretize, allowOverlap=allowOverlap)
  stream <- generate.static.stream(n=1000, prop=0.01, proptype= proptype, stream.config=stream.config)
  if(discretize == 0) {
    name <- paste("synth", dependency, str_pad(ndim,3,pad="0"), str_pad(num,3,pad="0"), proptype, sep="_")
    if(allowOverlap) name <- paste(name,"overlap", sep="_")
  } else {
    name <- paste("synth", dependency, "disc", str_pad(discretize,3,pad="0"), str_pad(ndim,3,pad="0"), str_pad(num,3,pad="0"), proptype, sep="_")
    if(allowOverlap) name <- paste(name,"overlap",sep="_")
  }
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
      generate.static.benchmark(250, dependency, proptype=proptype, discretize= disc, allowOverlap=allowOverlap, num=x)
    }
  }
}


setwd("~/data/New-synth-proportional")

################### Statics streams 

########## Real-valued data sets 

##### Wall dependency 
generate.dependency.dim("Wall", "proportional")
##### Square dependency 
generate.dependency.dim("Square", "proportional")
##### Donut
generate.dependency.dim("Donut", "proportional")

########## Discrete data sets

##### Wall dependency 
generate.dependency.discrete("Wall", "proportional")
##### Square dependency 
generate.dependency.discrete("Square", "proportional")
##### Donut
generate.dependency.discrete("Donut", "proportional")



setwd("~/data/New-synth-absolute")

################### Statics streams 

########## Real-valued data sets 

##### Wall dependency 
generate.dependency.dim("Wall", "absolute")
##### Square dependency 
generate.dependency.dim("Square", "absolute")
##### Donut
generate.dependency.dim("Donut", "absolute")

########## Discrete data sets

##### Wall dependency 
generate.dependency.discrete("Wall", "absolute")
##### Square dependency 
generate.dependency.discrete("Square", "absolute")
##### Donut
generate.dependency.discrete("Donut", "absolute")

setwd("~/data/New-synth-absolute-overlap")

########## Real-valued data sets 

##### Wall dependency 
generate.dependency.dim("Wall", "absolute", allowOverlap=TRUE)
##### Square dependency 
generate.dependency.dim("Square", "absolute", allowOverlap=TRUE)
##### Donut
generate.dependency.dim("Donut", "absolute", allowOverlap=TRUE)

########## Discrete data sets

##### Wall dependency 
generate.dependency.discrete("Wall", "absolute", allowOverlap=TRUE)
##### Square dependency 
generate.dependency.discrete("Square", "absolute", allowOverlap=TRUE)
##### Donut
generate.dependency.discrete("Donut", "absolute", allowOverlap=TRUE)

