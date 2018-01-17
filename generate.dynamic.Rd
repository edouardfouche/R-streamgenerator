\name{generate.dynamic}
\alias{generate.dynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate.dynamic(dim, subspaces, nstep = 10, volatility = 0.1, values = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.8, 0.7, 0.9), cycle = 0, allowOverlap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dim}{
%%     ~~Describe \code{dim} here~~
}
  \item{subspaces}{
%%     ~~Describe \code{subspaces} here~~
}
  \item{nstep}{
%%     ~~Describe \code{nstep} here~~
}
  \item{volatility}{
%%     ~~Describe \code{volatility} here~~
}
  \item{values}{
%%     ~~Describe \code{values} here~~
}
  \item{cycle}{
%%     ~~Describe \code{cycle} here~~
}
  \item{allowOverlap}{
%%     ~~Describe \code{allowOverlap} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dim, subspaces, nstep = 10, volatility = 0.1, values = c(0.1, 
    0.2, 0.3, 0.4, 0.5, 0.8, 0.7, 0.9), cycle = 0, allowOverlap = FALSE) 
{
    sanitycheck.config(dim = dim, subspaces = subspaces, nstep = nstep, 
        volatility = volatility, values = values, cycle = cycle)
    if (cycle == 0) {
        marginslist <- list(as.list(sample(values, length(subspaces), 
            replace = TRUE)))
        subspaceslist <- list(subspaces)
        for (n in 1:(nstep - 1)) {
            indexes <- sample(1:length(subspaceslist[[n]]), max(c(floor(length(subspaceslist[[n]])) * 
                volatility, 1)))
            nextsubspaces <- replace.subspaces(dim, subspaceslist[[n]], 
                indexes, allowOverlap)
            nextmargins <- marginslist[[n]]
            nextmargins[indexes] <- sample(values, length(indexes), 
                replace = TRUE)
            subspaceslist[[n + 1]] <- nextsubspaces
            marginslist[[n + 1]] <- nextmargins
        }
        res <- list(marginslist = marginslist, subspaceslist = subspaceslist)
    }
    else {
        cycle <- generate.dynamic(dim = dim, subspaces = subspaces, 
            nstep = cycle, volatility = volatility, values = values)
        marginslist_cycle <- c(cycle$marginslist, rev(cycle$marginslist[2:(length(cycle$marginslist) - 
            1)]))
        subspaceslist_cycle <- c(cycle$subspaceslist, rev(cycle$subspaceslist[2:(length(cycle$subspaceslist) - 
            1)]))
        res <- list(marginslist = rep(marginslist_cycle, ceiling(nstep/length(marginslist_cycle)))[1:nstep], 
            subspaceslist = rep(subspaceslist_cycle, ceiling(nstep/length(marginslist_cycle)))[1:nstep])
    }
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
