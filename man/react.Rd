\name{react}
\alias{react}
\title{Tuberculin reactions}
\description{
  The numeric vector \code{react} contains differences between two
  nurses' determinations of 334 tuberculin reaction sizes.
}
\usage{react}
\format{
  A single vector, differences between reaction sizes in mm.
}
\source{
 Anon. (1977), \emph{Exercises in Applied Statistics}, Exercise 2.9, Dept.\ of
 Theoretical Statistics,  
 Aarhus University.
}
\examples{
hist(react) # not good because of discretization effects...
plot(density(react))
}
\keyword{datasets}
