\name{malaria}
\alias{malaria}
\title{Malaria antibody data}
\description{
The \code{malaria} data frame has 100 rows and 4 columns.
}
\usage{malaria}
\format{
  This data frame contains the following columns:
  \describe{
    \item{subject}{subject code.}
    \item{age}{age in years.}
    \item{ab}{antibody level.}
    \item{mal}{a numeric vector code: Malaria, 0/1 is no/yes, respectively. }
  }
}
\details{
  A random sample of 100
  children from a village in Ghana, aged 3--15 years. The children were
  followed for a period of 8 months. At the beginning of the study,
  values of a particular antibody were assessed. Based on observations
  during the study
  period, the children were categorized into two groups:
  individuals with and without symptoms of malaria.
}
\source{
 Unpublished data.
}
\examples{
summary(malaria)
}
\keyword{datasets}
