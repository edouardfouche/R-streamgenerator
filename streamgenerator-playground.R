setwd("~/git/R-streamgenerator")

library(devtools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load_all(dirname(rstudioapi::getActiveDocumentContext()$path))

subspaces <- list(c(1,2,3), c(3,4,5), c(7,8), c(11,12), c(15,16))
replace.subspaces(20, subspaces, indexes=1:length(subspaces), allowOverlap=TRUE)


subspaces <- list(c(1,2,3), c(3,4), c(5,6,7,8), c(7,8,9))
meta <- generate.dynamic(dim=20, subspaces=subspaces, nstep=3, volatility=0.5)
meta_cycle <- generate.dynamic(dim=20, subspaces=subspaces, nstep=10, volatility=0.5, cycle=3)

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Wall", discretize=30, allowOverlap=FALSE)
stream.config$subspaces
## Override the subspaces and margin
subspaces <- list(c(1,2))#,c(3,4), c(5,6,7),c(8,9))
stream.config$subspaces <- subspaces
stream.config$margins <- c(0.9)#, 0.5, 0.7, 0.1)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

library(scales)

### visualize it 
plot(stream$data[,1], stream$data[,2], col=alpha(stream$data$class+1,0.5))
plot(stream$data[,3], stream$data[,4], col=as.numeric((stream$labels != 0))+1)
plot(stream$data[,8], stream$data[,9], col=as.numeric((stream$labels != 0))+1)

stream$labels
library(rgl)
plot3d(stream$data[,7], stream$data[,5], stream$data[,6], col=as.numeric((stream$labels != 0))+1)

##### generate visualization for the docu. 
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Wall", discretize=0, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
wall <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Square", discretize=0, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
square <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Donut", discretize=0, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
donut <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

svg("img/dependencies_real.svg", width=15, height=5)
par(mfrow=c(1,3))
plot(wall$data[,1], wall$data[,2], col=alpha(wall$data$class+1,0.5), main="Wall (real)")
plot(square$data[,1], square$data[,2], col=alpha(square$data$class+1,0.5), main="Square (real)")
plot(donut$data[,1], donut$data[,2], col=alpha(donut$data$class+1,0.5), main="Donut (real)")
dev.off()

stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Wall", discretize=20, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
wall <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Square", discretize=20, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
square <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Donut", discretize=20, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
donut <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)

svg("img/dependencies_discrete.svg", width=15, height=5)
par(mfrow=c(1,3))
plot(wall$data[,1], wall$data[,2], col=alpha(wall$data$class+1,0.5), main="Wall (discrete)")
plot(square$data[,1], square$data[,2], col=alpha(square$data$class+1,0.5), main="Square (discrete)")
plot(donut$data[,1], donut$data[,2], col=alpha(donut$data$class+1,0.5), main="Donut (discrete)")
dev.off()


stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4, dependency="Wall", discretize=0, allowOverlap=FALSE)
stream.config$subspaces <- list(c(1,2)) # Override the subspaces and margin
stream.config$margins <- c(0.8)
stream <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)
plot(stream$data[,1], stream$data[,2], col=alpha(stream$data$class+1,0.5))


# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.005, proptype="absolute", stream.config=stream.config)



setwd("~/test/")

# TODO: There are some trouble when overlapping is not allowed. 
config <- generate.stream.config(overlapAllowed = FALSE, volatility = 0.5)
stream <- generate.dynamic.stream(n=1000, prop=0.005, proptype="proportional", stream.config = config, verbose=TRUE)




### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency="Square")
## Override the subspaces and margins
subspaces <- list(c(1,2),c(4,5,6),c(8,9))
stream.config$subspaces <- subspaces
stream.config$margins <- c(0.9, 0.9, 0.5)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.01, stream.config=stream.config)

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2, dependency="Donut")
## Override the subspaces and margins
subspaces <- list(c(1,2),c(4,5,6),c(8,9))
stream.config$subspaces <- subspaces
stream.config$margins <- c(0.9, 0.9, 0.2)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0, stream.config=stream.config)



generateDonut <- function(margin=0.5){
  t <- 2*pi*runif(1)
  u <- (runif(1) + runif(1))
  ifelse(u>1, r <- 2-u, r <- u)
  if(r < margin){r <- (r*(1-margin)/margin) + margin}
  c(r*cos(t), r*sin(t))
}



output.stream(stream, paste(lapply(subspaces, FUN=function(x) paste(x-1,collapse="-")), collapse="_"))

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=3, maxdim=3)
## Override the subspaces and margins
subspaces <- list(c(2,3,4),c(7,8,9))
stream.config$subspaces  <- subspaces
stream.config$margins  <-c(0.2, 0.2)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=100, prop=0.00, stream.config=stream.config)

output.stream(stream, paste(lapply(subspaces, FUN=function(x) paste(x-1,collapse="-")), collapse="_"))

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=4)
## Override the subspaces and margins
subspaces <- list(c(1,2),c(4,5,6), c(3,7,8,9))
stream.config$subspaces  <- subspaces
stream.config$margins <- c(0.3,0.2, 0.1)


### Generate a static stream with extreme dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=20, nstep=1, mindim=2, maxdim=4)
## Override the subspaces and margins
subspaces <- list(c(1,2),c(3,4,5), c(6,7,8,9), c(15,16,17,18,19))
stream.config$subspaces  <- subspaces
stream.config$margins <- c(0.05,0.05,0.05,0.05)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=100, prop=0.00, stream.config=stream.config)

output.stream(stream, paste(c(sapply(subspaces, FUN=function(x) paste(x-1,collapse="-")),"extreme"), collapse="_"))

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=100, nstep=1, mindim=2, maxdim=5)
## Override the subspaces and margins
subspaces <- list(c(12,13),c(26,27),c(38,39,40),c(56,57,58),c(41,42,43,44),c(47,48,49,50),c(31,32,33,34,35),c(74,75,76,77,78))
stream.config$subspaces <- subspaces
stream.config$margins <- c(0.2, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)

output.stream(stream, paste(lapply(subspaces, FUN=function(x) paste(x-1,collapse="-")), collapse="_"))

### Generate a static stream with no dependencies, 100 dimensions :) 
stream.config <- generate.stream.config(dim=100, nstep=1, mindim=2, maxdim=2)
stream.config$margins <- rep(1,length(stream.config$margins)) # Just fill the margins to 1, so there will be no dependencies
# There cannot be any outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
output.stream(stream, "nodep100")

### Generate a static stream with no dependencies, 50 dimensions :) 
stream.config <- generate.stream.config(dim=50, nstep=1, mindim=2, maxdim=2)
stream.config$margins <- rep(1,length(stream.config$margins)) # Just fill the margins to 1, so there will be no dependencies
# There cannot be any outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
output.stream(stream, "nodep50")

### Generate a static stream with no dependencies, 20 dimensions :) 
stream.config <- generate.stream.config(dim=20, nstep=1, mindim=2, maxdim=2)
stream.config$margins <- rep(1,length(stream.config$margins)) # Just fill the margins to 1, so there will be no dependencies
# There cannot be any outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
output.stream(stream, "nodep20")


### Generate a static stream with no dependencies, 10 dimensions :) 
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2)
stream.config$margins <- rep(1,length(stream.config$margins)) # Just fill the margins to 1, so there will be no dependencies
# There cannot be any outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)
output.stream(stream, "nodep10")

data <- read.csv("/home/fouchee/git/SubspaceSearch/SubspaceSearch/src/test/resources/11-12_25-26_37-38-39_55-56-57_40-41-42-43_46-47-48-49_30-31-32-33-34_73-74-75-76-77_data.txt")


data <- read.csv("/home/fouchee/git/R-streamgenerator/nodep_data.txt")


library(subcon)
library(data.table)
indexMatrix <- sortedIndexMatrix(data.table(data))

HiCSMatrixC(indexMatrix, 0.1, 100)

HiCSSearch(indexMatrix,0.1,1000,700,30)
HiCSContrastC(indexMatrix,c(38,39,40),0.1,1000)
HiCSContrastC(indexMatrix,c(27,26,74),0.1,1000)
HiCSContrastC(indexMatrix,c(27,26),0.1,1000)

HiCSContrastC(indexMatrix,c(27,74),0.1,1000)

HiCSContrastC(indexMatrix,c(27,26,71),0.1,1000)

averageDeviationC(indexMatrix, c(27,26), 0.1, 26, 1000)
averageDeviationC(indexMatrix, c(27,26,74), 0.1, 74, 1000) # problem here
averageDeviationC(indexMatrix, c(38,39,40), 0.1, 40, 1000)


library(subcon)
library(data.table)
indexMatrix <- sortedIndexMatrix(data.table(stream$data))


HiCSContrastC(indexMatrix,c(3,7,8,9),0.1,1000)
