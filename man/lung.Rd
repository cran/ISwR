\name{lung}
\alias{lung}
\non_function{}
\title{Methods for determining lung volume}
\usage{data(lung)}
\description{
The \code{lung} data frame has 18 rows and 3 columns. It contains data
on three different  methods  of determining human
  lung volume.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{volume}{a numeric vector. Measured lung volume.}
    \item{method}{a factor with levels \code{A}, \code{B}, and \code{C}. }
    \item{subject}{a factor with levels \code{1}--\code{6}.}
  }
}
\source{
 \emph{Exercises in Applied statistics} (1977), Exercise~4.15, Dept.\ of
 Theoretical Statistics,  
 Aarhus University.
}
\examples{
data(lung)
}
\keyword{datasets}
