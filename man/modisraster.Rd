\name{modisraster}
\alias{modisraster}
\docType{data}
\title{
A raster brick of 16-day satellite image NDVI time series for a small subset in south eastern Somalia.
}
\description{
A raster brick containing 16-day NDVI satellite images (MOD13C1 product). See for description of the data set itself \url{https://lpdaac.usgs.gov/products/modis_products_table}.
}

\source{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2012). 
Near real-time disturbance detection using satellite image time series. 
\emph{Remote Sensing of Environment}. \url{http://eeecon.uibk.ac.at/wopec2/repec/inn/wpaper/2011-18.pdf}.
}

\examples{

## example of spatial and temporal raster plotting functionality
#plot(modisraster,1) # plot the first image layer
#plot(modisraster[1]) # plot for the first pixel the full time series

## select modis data for one pixel out of the raster brick
## and link it with the dates information.
#ndvi <- bfastts(modisraster[1],dates)
#plot(ndvi)

# References
citation("bfast")
}
\keyword{datasets}
\keyword{ts}
