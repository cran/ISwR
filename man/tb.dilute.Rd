\name{tb.dilute}
\alias{tb.dilute}
\title{Tuberculin dilution assay}
\description{
The \code{tb.dilute} data frame has 18 rows and 3 columns. It contains
data from a drug test involving dilutions of tuberculin.
}
\usage{tb.dilute}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{reaction}}{a numeric vector, reaction sizes (average of diameters) for tuberculin skin pricks.}
    \item{\code{animal}}{a factor with levels \code{1}--\code{6}.}
    \item{\code{logdose}}{a factor with levels \code{0.5}, \code{0}, and \code{-0.5}.}
  }
}
\details{The actual dilutions were 1:100, \eqn{1:100\sqrt{10}}, 1:1000. 
  Setting the middle one to 1 and using base-10 logarithms gives 
  the \code{logdose} values.}
\source{
 Anon. (1977), \emph{Exercises in Applied Statistics}, part of Exercise 4.15,
 Dept.\ of 
 Theoretical Statistics,  
 Aarhus University.
}
\keyword{datasets}
