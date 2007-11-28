\name{intake}
\alias{intake}
\title{Energy intake}
\description{
The \code{intake} data frame has 11 rows and 2 columns.
It contains paired values of energy intake for 11 women.
}
\usage{intake}
\format{
  This data frame contains the following columns:
  \describe{
    \item{pre}{a numeric vector. Premenstrual intake (kJ).}
    \item{post}{a numeric vector. Postmenstrual intake (kJ).}
  }
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  9.3, Chapman & Hall.
}
\examples{
plot(intake$pre, intake$post)
}
\keyword{datasets}
