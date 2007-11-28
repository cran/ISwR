\name{eba1977}
\alias{eba1977}
\encoding{UTF-8}
\docType{data}
\title{Lung cancer incidence in four Danish cities 1968--1971}
\description{
  This data set contains counts of incident lung cancer cases and
  population size in four neighbouring Danish cities by age group.
}
\usage{eba1977}
\format{
  A data frame with 24 observations on the following 4 variables:
  \describe{
    \item{\code{city}}{a factor with levels \code{Fredericia},
      \code{Horsens}, \code{Kolding}, and \code{Vejle}.}
    \item{\code{age}}{a factor with levels \code{40-54}, \code{55-59},
      \code{60-64}, \code{65-69}, \code{70-74}, and \code{75+}.}
    \item{\code{pop}}{a numeric vector, number of inhabitants.}
    \item{\code{cases}}{a numeric vector, number of lung cancer cases.}
  }
}
\details{
  These data were \dQuote{at the center of public interest in Denmark in
  1974}, according to Erling Andersen's paper. The city of Fredericia
  has a substantial petrochemical industry in the harbour area. 
}
\source{
  E.B. Andersen (1977), Multiplicative Poisson models with unequal
  cell rates,  \emph{Scandinavian Journal of Statistics}, 4:153--158.
}
\references{
  J. Clemmensen et al. (1974), \emph{Ugeskrift for \enc{Læger}{Laeger}},
  pp. 2260--2268. 
}
\keyword{datasets}
