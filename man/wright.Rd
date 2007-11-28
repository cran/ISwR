\name{wright}
\alias{wright}
\title{Comparison of Wright peak-flow meters}
\description{
The \code{wright} data frame has 17 rows and 2 columns.
It contains data on peak expiratory flow rate with two different flow
meters on each of 17 subjects. 
}
\usage{wright}
\format{
This data frame contains the following columns:
\describe{
\item{\code{std.wright}}{
a numeric vector, data from large flow meter (l/min).
}
\item{\code{mini.wright}}{
a numeric vector, data from mini flow meter (l/min).
}
}
}
\source{
J.M. Bland and D.G. Altman (1986), Statistical methods for assessing
agreement between two methods of clinical measurement, \emph{Lancet}, 1:307--310.
}
\examples{
plot(wright)
abline(0,1)
}
\keyword{datasets}
