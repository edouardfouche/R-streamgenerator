# R-streamgenerator
A statistical muldi-dimensional stream generator for benchmarking stream mining algorithms.

## Install

1. Clone this repository
2. Install dev-tools: 
```R
install.packages("devtools")
```
3a. Install the package:
```R
library(devtools)
devtools::install("~/path/to/cloned/R-streamgenerator")
```

3b. Or load the package in the environment (development mode):
```R
library(devtools)
load_all("~/path/to/cloned/R-streamgenerator")
```

## Get started

* Generate a static stream 

```R
stream <- generate.static.stream()
# Generate a stream with custom configuration
stream.config <- generate.stream.config(dim=50, nsteps=1) # nsteps should be = 1
stream <- generate.static.stream(n=1000, prop=0.05, stream.config=stream.config)
```

* Generate a dynamic stream 

```R
stream <- generate.dynamic.stream()
# Generate a stream with custom configuration
stream.config <- generate.stream.config(dim=50, nsteps=10, volatility=0.5)
stream <- generate.dynamic.stream(n=100, prop=0.05, stream.config=stream.config)
```

* Output stream

```R
output.stream(stream, "example")
```