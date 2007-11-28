\name{energy}
\alias{energy}
\title{Energy expenditure}
\description{
  The \code{energy} data frame has 22 rows and 2 columns.
  It contains data on the energy expenditure in groups of lean and obese women.
}
\usage{energy}
\format{
  This data frame contains the following columns:
  \describe{
    \item{expend}{
      a numeric vector. 24 hour energy expenditure (MJ).
    }
    \item{stature}{
      a factor with levels
      \code{lean} and 
      \code{obese}.
    }
  }
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  9.4, Chapman & Hall.
}
\examples{
plot(expend~stature,data=energy)
}
\keyword{datasets}
