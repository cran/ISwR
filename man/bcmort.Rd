\name{bcmort}
\alias{bcmort}
\docType{data}
\title{Breast cancer mortality}
\description{
  Danish study on the effect of screening for breast cancer.
}
\usage{
bcmort
}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{age}}{a factor with levels \code{50-54}, \code{55-59},
      \code{60-64}, \code{65-69}, \code{70-74}, and \code{75-79}}.
    \item{\code{cohort}}{a factor with levels \code{Study gr.},
      \code{Nat.ctr.}, \code{Hist.ctr.}, and \code{Hist.nat.ctr.}.}
    \item{\code{bc.deaths}}{a numeric vector, number of breast cancer deaths.}
    \item{\code{p.yr}}{a numeric vector, person-years under study.}
  }
}
\details{
  Four cohorts were collected. The \dQuote{study group} consists of the
  population of women in the appropriate age range in Copenhagen and
  Frederiksberg after the introduction of routine mammography screening.
  The \dQuote{national control
  group} consisted of the population in the parts of Denmark in which
  routine mammography screening was not available. These two groups were
  both collected in the years 1991--2001.  The \dQuote{historical control
  group} and the
  \dQuote{historical national control group} are similar cohorts from 10 years
  earlier (1981--1991), before the introduction of screening in Copenhagen and
  Frederiksberg. The study group comprises the entire population, not
  just those accepting the invitation to be screened.    
}
\source{
  A.H. Olsen et al. (2005), Breast cancer mortality in {C}openhagen after
  introduction of
  mammography screening. {\em British Medical Journal}, 330: 220--222.

}
\keyword{datasets}
