\name{FigQAMAE}
\alias{FigQAMAE}

\title{
Generate a SpatialPoints object corresponding to the best result obtained in an optimized network 
}
\description{
Generate a 
}
\usage{
FigQAMAE(x, y, stratum, fact=1, nullValue=0)
}

\arguments{
  \item{x}{
object of class rbga resulting from 
}
  \item{y}{
object of class rbga resulting from 
}
  \item{stratum}{
object of class rbga resulting from 
}
  \item{fact}{
object of class rbga resulting from 
}
  \item{nullValue}{
object of class rbga resulting from 
}
}
\value{
a SpatialPoints object
}
\seealso{
See function 
}
\examples{
library(raster)
example_x1 <- raster(system.file("external/example_x1.rst", package="rAA")) 
example_y1 <- raster(system.file("external/example_y1.rst", package="rAA")) 
example_mask1 <- raster(system.file("external/example_mask1.rst", package="rAA")) 
FigQAMAE(example_x1, example_y1, example_mask1, fact=1)
}

\keyword{ spatial }
