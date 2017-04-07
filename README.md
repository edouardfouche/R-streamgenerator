# R-streamgenerator
A statistical muldi-dimensional stream generator for benchmarking stream mining algorithms.

This R package provide functions to generate multidimensional data streams where the correlation structure is susceptible to change through time. 

## How it works

- We define (or generate randomly) a list of subspaces *subspaces* from a number of dimensions *dim*, where each subspaces are composed of at least *mindim* dimensions and at most *maxdim* dimensions. The subspaces shall overlap, but no subspace shall contain another. 
- We determine *margins*, a list of number between 0 and 1 (parameter *values*) having the same size as *subspaces*. So the subspace at position *x* is assigned to the margin at position *x*. 
- Each data point is represented by a vector of length *dim*, whose values are drown from the uniform distribution between 0 and 1, at the exception the subspaces specified in *subspaces*. For each subspace, an hypercube of side *margin*-1 is defined, such that all points falling into this hypercube are uniformly moved away to the rest of the space. 
- Each data point falling into the hypercube has a probability *prop* to stay in it, which makes this point a *non-trivial* outlier. We make sure that the point is not too close to the border of the hypercube by tolerance of about 10% of *margin*-1. 
- The list *subspaces* and *margins* are changed over a number *nsteps* of time steps by a proportion *volatility*. So if *volatility* = 0.5, half of the subspace/margin pairs will be changed at each step. Each step is composed of a number of *n* points, which can also vary through time. Between each time step, the state of the generator change uniformly to the state described by the next *subspaces*/*margins* list. 

Note that the proportion of outlier \code{prop} does not relate directly to the percentage of outliers in the output stream. Since it corresponds to the probability of a point, being already in the hidden space to stay where it is, the overall proportion of outliers depends on the hidden space volume, which depends on the number of subspaces and their. margins.

TL;DR: the data is generated uniformly, except in certain subspaces where the data is concentrated on the axes, in a L-like shape. The should create spaces with various dependency and space to place hidden outliers. The dependencies are susceptible to change in amplitude and subspaces through time. The following picture shows a snapshot of 100 points of  attributes n°7 and n°8 in data stream generated this way between at different point in time: 

[[https://github.com/edouardfouche/R-streamgenerator/blob/master/img/streamgenerator_1.png|alt=illustration]]

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

## TODO(s)

* Write more tests
* Write about the generation approach
* Use it 