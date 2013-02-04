\name{unique}

\docType{methods}

\alias{unique}
\alias{unique,RasterLayer,missing-method}
\alias{unique,RasterStackBrick,missing-method}

\title{Unique values}

\description{
This function returns the unique values in a RasterLayer, or the unique combinations of values in a multi-layer raster object.
}

\usage{
\S4method{unique}{RasterLayer,missing}(x, incomparables=FALSE, ...) 

\S4method{unique}{RasterStackBrick,missing}(x, incomparables=FALSE, ...) 
}

\arguments{
  \item{x}{Raster object}
  \item{incomparables}{ignored. Must be missing}
  \item{...}{additional arguments}  
}

\seealso{ \code{\link[base]{unique}} }

\value{
vector or matrix
}



\examples{
}

\keyword{spatial}
