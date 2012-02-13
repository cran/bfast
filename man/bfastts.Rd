\name{bfastts}
\alias{bfastts}
\title{Create a regular time series object by combining data and date information}
\description{
  Create a regular time series object by combining measurements (data) and time (dates) information.
}

\usage{
bfastts(data, dates, 
		type = c("irregular", "16-day"))
}

\arguments{
  \item{data}{A data vector }
  \item{dates}{Optional input of dates for each measurement in the 'data' variable. In 
    case the data is a irregular time series, a vector with 'dates' for 
  	each measurement can be supplied using this 'dates' variable. The irregular data will be linked
  	with the dates vector to create daily regular time series with a frequency = 365. Extra days
  	in leap years might cause problems. Please be carefull using this option as it is 
  	experimental. Feedback is welcome.}
  \item{type}{ (\code{"irregular"}) indicates that the data is collected at irregular dates and as such will be converted to a daily time series. (\code{"16-day"})indicates that data is collected a a regular time interval (every 16-days e.g. like the MOD13Q1 data products)}
}

\details{
  \code{bfastts} create a regular time series
}

\value{
  \code{bfastts} returns an object of class \code{"ts"}, i.e., a list with components as follows.
  \item{zz}{ a regular \code{"ts"} time series with a frequency equal to 365 or 23 i.e. 16-day time series.}
}

\references{
Verbesselt J, Zeileis A, Herold M (2011).
  Near Real-Time Disturbance Detection in Terrestrial Ecosystems Using Satellite
  Image Time Series: Drought Detection in Somalia.
  Working Paper 2011-18. Working Papers in Economics and Statistics,
  Research Platform Empirical and Experimental Economics, Universitaet Innsbruck.
  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-18}.
  Submitted to Remote Sensing and Environment.
}

\author{Achim Zeileis, Jan Verbesselt}

\seealso{\code{\link[strucchange]{monitor}}, \code{\link[strucchange]{mefp}}, \code{\link[strucchange]{breakpoints}}}

\examples{

#if(require("raster")) {
#	data <- modisraster[1]
#	ndvi <- bfastts(data, dates, type = c("16-day"))
#	plot(ndvi) 
#}

}

\keyword{ts}

