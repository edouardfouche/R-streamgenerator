
library(devtools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load_all(dirname(rstudioapi::getActiveDocumentContext()$path))

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=10, nstep=1, mindim=2, maxdim=2)
## Override the subspaces and margins
subspaces <- list(c(3,4),c(5,6),c(8,9))
stream.config$subspaces <- subspaces
stream.config$margins <- c(0.2, 0.2, 0.2)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=100, prop=0.00, stream.config=stream.config)

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

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=100, prop=0.00, stream.config=stream.config)

output.stream(stream, paste(lapply(subspaces, FUN=function(x) paste(x-1,collapse="-")), collapse="_"))

### Generate a static stream with some dependencies on choosen subspaces
stream.config <- generate.stream.config(dim=100, nstep=1, mindim=2, maxdim=5)
## Override the subspaces and margins
subspaces <- list(c(12,13),c(26,27),c(38,39,40),c(56,57,58),c(41,42,43,44),c(47,48,49,50),c(31,32,33,34,35),c(74,75,76,77,78))
stream.config$subspaces <- subspaces
stream.config$margins <- c(0.2, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

# Let's say we are not interested in outliers, so we set the prop to 0
stream <- generate.static.stream(n=1000, prop=0.00, stream.config=stream.config)

output.stream(stream, paste(lapply(subspaces, FUN=function(x) paste(x-1,collapse="-")), collapse="_"))

data <- read.csv("/home/fouchee/git/SubspaceSearch/SubspaceSearch/src/test/resources/11-12_25-26_37-38-39_55-56-57_40-41-42-43_46-47-48-49_30-31-32-33-34_73-74-75-76-77_data.txt")

library(subcon)
library(data.table)
indexMatrix <- sortedIndexMatrix(data.table(data))

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
