\name{bp.obese}
\alias{bp.obese}
\non_function{}
\title{Obesity and blood pressure}
\description{
  The \code{bp.obese} data frame has 102 rows and 3 columns.
  It contains data from a random sample of Mexican-American adults in a
  small California town.
}
\format{
This data frame contains the following columns:
\describe{
\item{sex}{
a numeric vector code. 0: male, 1: female.
}
\item{obese}{
  a numeric vector. Ratio of actual weight to ideal weight from New York
  Metropolitan Life Tables.
}
\item{bp}{
  a numeric vector. Systolic blood pressure (mm Hg).
}
}
}
\source{
  B.W. Brown \& M. Hollander (1977), Statistics. A Biomedical Introduction,
  Wiley. 
}
\examples{
data(bp.obese)
plot(bp~obese,pch = ifelse(sex==1, "F", "M"), data = bp.obese)
}
\keyword{datasets}
