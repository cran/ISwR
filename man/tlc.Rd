\name{tlc}
\alias{tlc}
\title{Total lung capacity}
\description{
The \code{tlc} data frame has 32 rows and 4 columns. It contains data on
pretransplant total lung capacity (TLC) for recipients of heart-lung
transplants by whole-body plethysmography.
}
\usage{tlc}
\format{
This data frame contains the following columns:
\describe{
\item{\code{age}}{
a numeric vector, age of recipient (years).
}
\item{\code{sex}}{
a numeric vector code, female: 1, male: 2.
}
\item{\code{height}}{
a numeric vector, height of recipient (cm).
}
\item{\code{tlc}}{
a numeric vector, total lung capacity (l).
}
}
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research},
  Exercise 12.5,  10.1, Chapman & Hall.
}
\examples{
plot(tlc~height,data=tlc)
}
\keyword{datasets}
