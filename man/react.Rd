\name{react}
\alias{react}
\title{Tuberculin reactions}
\description{
  The numeric vector \code{react} contains differences between two
  nurses' determination of 334 tuberculin reaction sizes.
}
\usage{react}
\format{
  A single vector. Reaction sizes in mm.
}
\source{
 \emph{Exercises in Applied statistics} (1977), Exercise~2.9, Dept.\ of
 Theoretical Statistics,  
 Aarhus University.
}
\examples{
hist(react) # not good because of discretization effects...
plot(density(react))
}
\keyword{datasets}
