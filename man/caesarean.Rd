\name{caesarean}
\non_function{}
\alias{caesarean}
\alias{caesar.shoe}
\title{Caesarean section and maternal shoe size}
\description{
  The table \code{caesar.shoe} contains the relation between caesarean
  section and maternal shoe sizes (UK sizes!)
}
\format{
  A matrix with two rows and six columns
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  10.1, Chapman & Hall.
}
\examples{
data(caesarean)
require(ctest)
prop.trend.test(caesar.shoe["Yes",],margin.table(caesar.shoe,2))
}
\keyword{datasets}
