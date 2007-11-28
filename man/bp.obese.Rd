\name{bp.obese}
\alias{bp.obese}
\title{Obesity and blood pressure}
\description{
  The \code{bp.obese} data frame has 102 rows and 3 columns.
  It contains data from a random sample of Mexican-American adults in a
  small California town.
}
\usage{bp.obese}
\format{
This data frame contains the following columns:
\describe{
\item{\code{sex}}{
a numeric vector code, 0: male, 1: female.
}
\item{\code{obese}}{
  a numeric vector, ratio of actual weight to ideal weight from New York
  Metropolitan Life Tables.
}
\item{\code{bp}}{
  a numeric vector,systolic blood pressure (mm Hg).
}
}
}
\source{
  B.W. Brown and M. Hollander (1977), {\em Statistics: A Biomedical
  Introduction,} 
  Wiley. 
}
\examples{
plot(bp~obese,pch = ifelse(sex==1, "F", "M"), data = bp.obese)
}
\keyword{datasets}
