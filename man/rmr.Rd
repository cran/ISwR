\name{rmr}
\alias{rmr}
\title{Resting metabolic rate}
\description{
The \code{rmr} data frame has 44 rows and 2 columns.
It contains the resting metabolic rate and body weight data for 44 women.
}
\usage{rmr}
\format{
This data frame contains the following columns:
\describe{
\item{\code{body.weight}}{
a numeric vector, body weight (kg).
}
\item{\code{metabolic.rate}}{
a numeric vector, metabolic rate (kcal/24hr).
}
}
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research},
  Exercise 11.2, Chapman & Hall. 
}
\examples{
plot(metabolic.rate~body.weight,data=rmr)
}
\keyword{datasets}
