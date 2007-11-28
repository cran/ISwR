\name{tlc}
\alias{tlc}
\title{Total lung capacity}
\description{
The \code{tlc} data frame has 32 rows and 4 columns. It contains data on
pretransplant total lung capacity (TLC) for recipients of heart-lung
transplants, by whole-body plethysmography.
}
\usage{tlc}
\format{
This data frame contains the following columns:
\describe{
\item{age}{
a numeric vector. Age of recipient (years).
}
\item{sex}{
a numeric vector code. Female:~1, male:~2.
}
\item{height}{
a numeric vector. Height of recipient (cm).
}
\item{tlc}{
a numeric vector. Total lung capacity (l).
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
