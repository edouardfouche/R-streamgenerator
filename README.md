# R-streamgenerator
A statistical muldi-dimensional stream generator for benchmarking stream mining algorithms.

This R package provides functions to generate multidimensional data streams where the correlation structure can change through time. 

More information about the motivation of this project is available in this [article][article]

## How it works

- We define (or generate randomly) a list of subspaces *subspaces* from a number of dimensions *dim*, where each subspace is composed of at least *mindim* dimensions and at most *maxdim* dimensions. The subspaces shall overlap, but no subspace shall contain and or be contained in another. 
- We determine *margins*, a list of numbers between 0 and 1 having the same size as *subspaces*. The subspace at position *x* is assigned to the margin at position *x*. 
- Each data point is represented by a vector of length *dim*, whose values are drown from the uniform distribution between 0 and 1, at the exception of the subspaces specified in *subspaces*. For each subspace, an hypercube of side *margin*-1 is defined, such that all points falling into this hypercube are uniformly moved away to the rest of the space. 
- Each data point falling into the hypercube has a probability *prop* to stay in it, which makes this point a *non-trivial* outlier. We make sure that the point is not too close to the border of the hypercube by a tolerance of about 10% of *margin*-1. 
- The list *subspaces* and *margins* are changed over a number *nstep* of time steps by a proportion *volatility*. So if *volatility* is equal to 0.5, half of the subspace/margin pairs will be changed at each step. Each step is composed of a number of *n* points, which can also vary for each step. Between each time step, the state of the generator change uniformly from the current to the next *subspaces*/*margins* list. 

Note that the proportion of outlier in the output stream does not relate directly to *prop*. Since *prop* corresponds to the probability of a point in the hidden space to stay where it is, the overall proportion of outliers depends on the hidden space volume, which depends on the number of subspaces and their margins.

**TL;DR** the data is generated uniformly, except in some subspaces where the data is concentrated on the axes at a certain degree, in a L-like shape (in the 2-D case). This type of dependency leaves space to place *hidden* outliers. The dependencies are susceptible to change in amplitude and subspaces through time. The following picture shows a snapshot of 100 points of  attributes n°7 and n°8 in data stream generated this way at different points in time: 

![streamgenerator_1](/img/streamgenerator_1.png)

As you can see, the relationship between attribute n°7 and n°8 has evolved through time. Also, there is an outlier in picture 5 (in red). Obviously, by looking at the whole time window, this point would probably not have been detected as such. 

## Install

1. Install dev-tools: 
```R
install.packages("devtools")
```
2. Install the package from github

```R
library(devtools)
devtools::install_github("edouardfouche/R-streamgenerator")
```

### Development mode

1. Clone this repository 
2. Load the package in your environment:
```R
library(devtools)
load_all("~/path/to/cloned/R-streamgenerator/")
```

Note that the package is not published to CRAN (yet). 

## Get started

* Generate a static stream 

```R
stream <- generate.static.stream() # default parameters
# Generate a stream with custom configuration
stream.config <- generate.stream.config(dim=50, nstep=1) # nstep should be = 1
stream <- generate.static.stream(n=1000, prop=0.05, stream.config=stream.config)
```

* Generate a dynamic stream 

```R
stream <- generate.dynamic.stream() # default parameters
# Generate a stream with custom configuration
stream.config <- generate.stream.config(dim=50, nstep=10, volatility=0.5)
stream <- generate.dynamic.stream(n=100, prop=0.05, stream.config=stream.config)
```

* Output the stream

```R
# This will create 3 files in your working directory. 
# "example_data.txt" contains the stream
# "example_labels.txt" contains the labels, i.e if each point is an outlier and in which subspace(s)
# "example_description.txt" contains a human-readable description of the stream 
output.stream(stream, "example")
```

## TODO(s)

* Write more tests
* Write about the generation approach
* Develop other dependencies and transitions (drifts)
* Develop for discrete data too 
* Use it 


[article]: https://edouardfouche.com/Data-Stream-Generation-with-Concept-Drift/
