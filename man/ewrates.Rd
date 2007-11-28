\name{ewrates}
\alias{ewrates}
\docType{data}
\title{Rates of lung and nasal cancer mortality, and total mortality.}
\description{
England and Wales mortality rates from lung cancer, nasal cancer,
and all causes, 1936--1980. The 1936 rates are repeated as 1931 rates in
order to accommodate follow-up for the \code{\link{nickel}} study.
}
\usage{ewrates}
\format{
  A data frame with 150 observations on the following 5 variables:
  \describe{
    \item{\code{year}}{calendar period, 1931: 1931--35, 1936:
      1936--40, \ldots. }
    \item{\code{age}}{age class, 10: 10--14, 15:15--19, \ldots.}
    \item{\code{lung}}{lung cancer mortality rate per 1 million person-years}
    \item{\code{nasal}}{nasal cancer mortality rate per 1 million person-years}
    \item{\code{other}}{all cause mortality rate per 1 million person-years}
  }
}
\details{
  Taken from the \dQuote{Epi} package by Bendix Carstensen et al.
}
\source{
  N.E. Breslow, and N. Day  (1987). \emph{Statistical Methods in Cancer
    Research. Volume II: The Design and Analysis of Cohort Studies},
  Appendix IX.
  IARC Scientific Publications, Lyon. 
}
\keyword{datasets}
