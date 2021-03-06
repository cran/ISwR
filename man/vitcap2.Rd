\name{vitcap2}
\alias{vitcap2}
\title{Vital capacity, full data set}
\description{
The \code{vitcap2} data frame has 84 rows and 3 columns.
Age and vital capacity for workers in the cadmium industry.
}
\usage{vitcap2}
\format{
This data frame contains the following columns:
\describe{
\item{\code{group}}{
  a numeric vector; group codes are 1: exposed > 10 years,
  2: exposed < 10 years, 3: not exposed.
}
\item{\code{age}}{
a numeric vector, age in years.
}
\item{\code{vital.capacity}}{
a numeric vector, vital capacity (a measure of lung volume) (l).
}
}
}
\source{
  P. Armitage and G. Berry (1987), \emph{Statistical Methods in Medical Research}, 2nd ed., Blackwell, p.286.
}
\examples{
plot(vital.capacity~age, pch=group, data=vitcap2)
}
\keyword{datasets}
