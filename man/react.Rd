\name{react}
\alias{react}
\non_function{}
\title{Tuberculin reactions}
\format{
  A single vector. Reaction sizes in mm.
}
\description{
  The numeric vector \code{react} contains differences between two
  nurses' determination of 334 tuberculin reaction sizes.
}
\source{
 \emph{Exercises in Applied statistics} (1977), Exercise~2.9, Dept.\ of
 Theoretical Statistics,  
 Aarhus University.
}
\examples{
data(react)
hist(react) # not good because of discretization effects...
plot(density(react))
}
\keyword{datasets}
