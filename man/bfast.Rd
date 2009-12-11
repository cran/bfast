\name{bfast}
\Rdversion{1.1}
\alias{bfast}
\title{
Break detection in the seasonal and trend component of a univariate time series
}
\description{
Iterative break detection in seasonal and trend component of a time series. 
Seasonal breaks is a function that combines the iterative decomposition of 
time series into trend, seasonal and remainder components with significant 
break detection in the decomposed components of the time series.
}
\usage{
bfast(Yt, h = 0.15, max.iter = NULL, breaks = NULL)
}
\arguments{
  \item{Yt}{univariate time series to be analyzed. This should be an object of class "ts" with a frequency greater than one without NA's.
}
  \item{h}{minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment.
}
  \item{max.iter}{maximum amount of iterations allowed for estimation of breakpoints in seasonal and trend component.
}
  \item{breaks}{integer specifying the maximal number of breaks to be calculated. By default the maximal number allowed by h is used.
}
}
\details{To be completed.
}
\value{
    An object of the class "bfast" is a list with the following elements:
    \item{Yt}{ equals the Yt used as input.}
    \item{output}{ is a list with the following elements (for each iteration):
    \tabular{ll}{
        \code{Tt} \tab the fitted trend component\cr
        \code{St} \tab the fitted seasonal component\cr
        \code{Nt} \tab the noise or remainder component\cr
        \code{Vt} \tab equals the deseasonalized data \code{Yt - St} for each iteration\cr
        \code{bp.Vt} \tab output of the \code{\link[strucchange]{breakpoints}} function for the trend model\cr
        \code{ci.Vt} \tab output of the  \code{\link[strucchange]{breakpoints}} confint function for the trend model\cr
        \code{Wt} \tab equals the detrended data \code{Yt - Tt} for each iteration\cr
        \code{bp.Vt} \tab output of the \code{\link[strucchange]{breakpoints}} function for the seasonal model\cr
        \code{ci.Vt} \tab output of the  \code{\link[strucchange]{breakpoints}} confint function for the seasonal model
    }}
    \item{nobp}{ is a list with the following elements:
    \tabular{ll}{
        \code{nobp.Vt} \tab logical, TRUE if there are breakpoints detected\cr
        \code{nobp.Wt} \tab logical, TRUE if there are breakpoints detected
    }}        
    \item{magnitude}{ magnitude of the biggest change detected in the trend component}
    \item{Time}{ timing of the biggest change detected in the trend component}
}
\references{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (In Press). 
Detecting trend and seasonal changes in satellite image time series. 
\emph{Remote Sensing of Environment}. \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}. 
Or see \url{http://robjhyndman.com/papers/bfast1}.
}

\author{
Jan Verbesselt
}

\seealso{
\code{\link[bfast]{plot.bfast}} for plotting of bfast() results. \cr
\code{\link[strucchange]{breakpoints}} for more examples and background information about estimation of breakpoints in time series.
}
\examples{
# Simulated Data
plot(simts) # stl object containing simulated NDVI time series

datats <- ts(rowSums(simts$time.series)) # sum of all the components (season,abrupt,remainder)
tsp(datats) <- tsp(simts$time.series) # assign correct time series attributes
plot(datats)

fit <- bfast(datats,h=0.15,max.iter=1)
plot(fit,sim=simts)

fit # prints out whether breakpoints are detected in the seasonal and trend component

# Real data
# The data should be a regular ts() object without NA's
# See Fig. 8 b in reference
plot(harvest, ylab='NDVI') # MODIS 16-day cleaned and interpolated NDVI time series 

fit <- bfast(harvest,h=0.15,max.iter=1,breaks=2)
plot(fit)
plot(fit,type="trend",largest=TRUE)
plot(fit,type="all") 

}
\keyword{ts}

