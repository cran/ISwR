\name{kfm}
\alias{kfm}
\non_function{}
\title{Breast-feeding data}
\description{
  The \code{kfm} data frame has 50 rows and 7 columns.
  It was collected by Kim Fleischer Michaelsen and contains data for 50
  infants of age approximately 2~months. They were weighed immediately
  before and
  after each breast feeding and the measured intake of breast milk was
  registered along with various other data.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{no}{
      a numeric vector. Identification number.
    }
    \item{dl.milk}{
      a numeric vector. Breast-milk intake (dl/24h).
    }
    \item{sex}{
      a factor with levels
      \code{boy} and
      \code{girl} 
    }
    \item{weight}{
      a numeric vector. Weight of child (kg).
    }
    \item{ml.suppl}{
      a numeric vector. Supplementary milk substitute (ml/24h).
    }
    \item{mat.weight}{
      a numeric vector. Weight of mother (kg).
    }
    \item{mat.height}{
      a numeric vector. Height of mother (cm).
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
data(kfm)
plot(dl.milk~mat.height,pch=c(1,2)[sex],data=kfm)
}
\keyword{datasets}
