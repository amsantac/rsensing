\name{FigQAmultiRes}
\alias{Figure for Quantity Allocation at multi Resolution}

\title{
Quantity Allocation at multi-resolution 
}
\description{
This function determines the Quantity Allocation error at a multi-resolution.  The user must indicate the reference layer (x), the predicted layer (y), a mask defining the strata (mask), and establish the contraction factor (fact).  The output image shows the agreement in quantity between the reference and predicted layers.
}
\usage{
FigQAmultiRes(x, y, mask, fact=2)
}

\arguments{
  \item{x}{
indicate reference raster layer.  The reference layer is taken as the 'truth' image.
}
  \item{y}{
 indicate predicted raster layer
}
  \item{mask}{
indicate raster layer to be used as a mask; this layer defines the strata 
}
  \item{fact}{
indicate the contraction factor to adjust spatial resolution (pixel size) in output image
}
}
\value{
output image
}
\seealso{
See function 
}
\examples{
#data(example_x1)
#data(example_y1)
#data(example_mask1)
#FigQAmultiRes(example_x1, example_y1, example_mask1, fact=2) 
}

\keyword{ spatial, quantity allocation }
