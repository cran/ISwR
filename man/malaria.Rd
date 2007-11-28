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
    \item{\code{subject}}{subject code.}
    \item{\code{age}}{age in years.}
    \item{\code{ab}}{antibody level.}
    \item{\code{mal}}{a numeric vector code, Malaria: 0: no, 1: yes.}
  }
}
\details{
  A random sample of 100
  children aged 3--15 years from a village in Ghana. The children were
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
