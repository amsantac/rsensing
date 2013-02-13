\name{FigQAmultiRes}
\alias{FigQAmultiRes}

\title{
Quantity Allocation at multi-resolution 
}
\description{
This function determines the Quantity Allocation error at a multi-resolution.  The user must indicate the reference layer (x), the predicted layer (y), a raster map defining the strata (stratum), and the sequence of contraction factors (seqn).  The output figure shows the agreement in quantity between the reference and predicted layers.
}
\usage{
FigQAmultiRes(x, y, stratum, seqn="geom", nullValue=0)
}

\arguments{
  \item{x}{
a RasterLayer object corresponding to the reference map.  The reference layer is taken as the 'truth' image.
}
  \item{y}{
a RasterLayer object corresponding to the predicted map
}
  \item{stratum}{
a RasterLayer object defining the strata 
}
  \item{seqn}{
numeric vector indicating the sequence of contraction factors to adjust spatial resolution (pixel size) a los cuales se calculan los diferentes valores de Quantity Allocation
}
  \item{nullValue}{
numeric vector indicating the sequence of contraction factors to adjust spatial resolution (pixel size) a los cuales se calculan los diferentes valores de Quantity Allocation
}
}
\value{
Las funciones que generan graficas, como esta, probablemente no van a tener esta seccion de Value
}
\seealso{
See function 
}
\examples{
library(raster)
example_x1 <- raster(system.file("external/example_x1.rst", package="rAA")) 
example_y1 <- raster(system.file("external/example_y1.rst", package="rAA")) 
example_mask1 <- raster(system.file("external/example_mask1.rst", package="rAA")) 
FigQAmultiRes(example_x1, example_y1, example_mask1, seqn="geom") 
}

\keyword{ spatial }
