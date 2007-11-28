\name{nickel.expand}
\alias{nickel.expand}
\docType{data}
\title{Nickel smelters in South Wales, expanded}
\description{
  The data concern a cohort of nickel smelting workers in South Wales,
  with information on exposure, follow-up period, and cause of death, as
  in the \code{\link{nickel}} data.
  This version has follow-up times split according to age groups and is
  merged with the mortality rates in \code{\link{ewrates}}.
}
\usage{nickel.expand}
\format{
  A data frame with 3724 observations on the following 12 variables:
  \describe{
    \item{\code{agr}}{age class: 10: 10--14, 15: 15--19, \ldots.}
    \item{\code{ygr}}{calendar period, 1931: 1931--35, 1936:
      1936--40, \ldots .}
    \item{\code{id}}{subject identifier (numeric).}
    \item{\code{icd}}{ICD cause of death if dead, 0 otherwise (numeric).}
    \item{\code{exposure}}{exposure index for workplace (numeric). }
    \item{\code{dob}}{date of birth (numeric).} 
    \item{\code{age1st}}{age at first exposure (numeric).} 
    \item{\code{agein}}{age at start of follow-up (numeric).} 
    \item{\code{ageout}}{age at end of follow-up (numeric).} 
    \item{\code{lung}}{lung cancer mortality rate per 1 million person-years.}
    \item{\code{nasal}}{nasal cancer mortality rate per 1 million person-years.}
    \item{\code{other}}{all cause mortality rate per 1 million person-years.}
  }
}
\source{
  Computed from \code{\link{nickel}} and  \code{\link{ewrates}} data sets.
}

\keyword{datasets}
