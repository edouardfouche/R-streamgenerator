# R-streamgenerator
A statistical muldi-dimensional stream generator for benchmarking stream mining algorithms.

This R package provides functions to generate multidimensional data streams where the correlation structure can change through time. 

More information about the motivation of this project is available in this [article][article]

## How it works

- We define (or generate randomly) a list of subspaces *subspaces* from a number of dimensions *dim*, where each subspace is composed of at least *mindim* dimensions and at most *maxdim* dimensions. The subspaces shall overlap or not, depending on user's parameter *allowOverlap*, but no subspace shall contain and or be contained in another. 
- We determine *margins*, a list of numbers between 0 and 1 having the same size as *subspaces*. The subspace at position *x* is assigned to the margin at position *x*. 
- Each data point is represented by a vector of length *dim*, whose values are drown from the uniform distribution between 0 and 1, at the exception of the subspaces specified in *subspaces*. For each of those subspaces, a dependency is created, this dependency may have different shapes (Wall, Square, Donut). Then strength of the dependency in a subspace depends on its *margin* value. 
- Each subspace have a proportion *prop* of outlier. If the *proptype* is set to *proportional*, then the expected proportion of outlier in a subspace is proportional to its *margin*, i.e., the volume of hidden space. If the *proptype* is set to *absolute*, then the expected proportion of outlier depends on the number of points only. 
- There is the possibility to generate *static* streams and *dynamic* streams. For dynamic *streams*, the lists *subspaces* and *margins* are changed over a number *nstep* of time steps by a proportion *volatility*. For example, if *volatility* is equal to 0.5, half of the subspace/margin pairs will be changed at each step. Each step is composed of a number of *n* points, which can also vary for each step. 
- Between each time step, the state of the generator change uniformly from the current to the next *subspaces*/*margins* list. 
- The generated streams can be composed of real values (default) or can be discretized into a number of *discrete* values. 

Note that the overall proportion of outlier in the output stream does not relate directly to *prop*. Since *prop* corresponds either to the absolute expected proportion of outlier per subspace, or the expected proportion of outlier conditioned on the size of the hidden space. In both cases, it depends on the number of subspaces with dependencies. 


**TL;DR** the data is generated uniformly, except in some subspaces where the data is concentrated in the shape of particular dependencies. The choosen dependencies include regions to place hidden outliers. The dependencies are susceptible to change in amplitude and subspaces through time. The following picture shows a snapshot of 100 points in a subspace with a dependency of type "Wall" in a generated data stream at different points in time: 

![streamgenerator_1](/img/streamgenerator_1.png)

As you can see, the relationship between attribute n°7 and n°8 has evolved through time. Also, there is an outlier in picture 5 (in red). Obviously, by looking at the whole time window, this point would probably not have been detected as such. 

Currently, 3 kinds of dependencies are available, "Wall", "Square" and "Donut". Here is what they look like in 2-D spaces, with *n = 1000*, *margin=0.8*, *prop=0.005*, *proptype="absolute"*. Outliers are showed in red. 

With *discrete=0*:

![dependencies_real](/img/dependencies_real.svg)

With *discrete=20*: 

![dependencies_discrete](/img/dependencies_discrete.svg)

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
* Allow mixed dependencies in multivariate streams
* Allow mixed real/discrete data sets
* Write visualization functions 
* Use it 


[article]: https://edouardfouche.com/Data-Stream-Generation-with-Concept-Drift/
