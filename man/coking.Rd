\name{coking}
\alias{coking}
\title{Coking data}
\description{
  The \code{coking} data frame has 18 rows and 3 columns.
  It contains the time to coking in an experiment where the oven width
  and temperature were varied.
}
\usage{coking}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{width}}{a factor with levels \code{4}, \code{8}, and
      \code{12}, giving the oven width in inches.}
    \item{\code{temp}}{a factor with levels \code{1600} and \code{1900},
      giving the temperature in Fahrenheit.}
    \item{\code{time}}{a numeric vector, time to coking.}
  }
}
\source{
  R.A. Johnson (1994), \emph{Miller and Freund's Probability and Statistics for
  Engineers}, 5th ed., Prentice-Hall. 
}
\examples{
attach(coking)
matplot(tapply(time,list(width,temp),mean))
detach(coking)
}
\keyword{datasets}
