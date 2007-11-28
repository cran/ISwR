\name{nickel}
\alias{nickel}
\docType{data}
\title{Nickel smelters in South Wales}
\description{
  The data concern a cohort of nickel smelting workers in South Wales,
  with information on exposure, follow-up period, and cause of death.
}
\usage{nickel}
\format{
  A data frame containing 679 observations of the following 7 variables:
  \describe{
    \item{\code{id}}{subject identifier (numeric).}
    \item{\code{icd}}{ICD cause of death if dead, 0 otherwise (numeric).}
    \item{\code{exposure}}{exposure index for workplace (numeric) }
    \item{\code{dob}}{date of birth (numeric).} 
    \item{\code{age1st}}{age at first exposure (numeric).} 
    \item{\code{agein}}{age at start of follow-up (numeric).} 
    \item{\code{ageout}}{age at end of follow-up (numeric).} 
  }
}
\details{
  Taken from the \dQuote{Epi} package by Bendix Carstensen et al.
  For comparison purposes,
  England and Wales mortality rates (per 1,000,000 per annum)
  from lung cancer (ICDs 162 and 163),
  nasal cancer (ICD 160), and all causes, by age group and calendar period, are
  supplied in the data set \code{\link{ewrates}}.
}
\source{
  N.E. Breslow and N. Day (1987). \emph{Statistical Methods in Cancer
    Research. Volume II: The Design and Analysis of Cohort Studies},
  IARC Scientific Publications, Lyon. 
}
\keyword{datasets}
