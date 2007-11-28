\name{kfm}
\alias{kfm}
\title{Breast-feeding data}
\description{
  The \code{kfm} data frame has 50 rows and 7 columns.
  It was collected by Kim Fleischer Michaelsen and contains data for 50
  infants of age approximately 2 months. They were weighed immediately
  before and
  after each breast feeding. and the measured intake of breast milk was
  registered along with various other data.
}
\usage{kfm}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{no}}{
      a numeric vector, identification number.
    }
    \item{\code{dl.milk}}{
      a numeric vector, breast-milk intake (dl/24h).
    }
    \item{\code{sex}}{
      a factor with levels
      \code{boy} and
      \code{girl}. 
    }
    \item{\code{weight}}{
      a numeric vector, weight of child (kg).
    }
    \item{\code{ml.suppl}}{
      a numeric vector, supplementary milk substitute (ml/24h).
    }
    \item{\code{mat.weight}}{
      a numeric vector, weight of mother (kg).
    }
    \item{\code{mat.height}}{
      a numeric vector, height of mother (cm).
    }
  }
}
\note{
  The amount of supplementary milk substitute refers to a period before
  the data collection.
}
\source{
 Original data.
}
\examples{
plot(dl.milk~mat.height,pch=c(1,2)[sex],data=kfm)
}
\keyword{datasets}
