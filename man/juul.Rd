\name{juul}
\alias{juul}
\title{Juul's IGF data}
\description{
  The \code{juul} data frame has 1339 rows and 6 columns.
  It contains a reference sample of the distribution of insulin-like
  growth factor (IGF-I), one observation per subject in various ages, with the
  bulk of the data collected in connection with school physical
  examinations. 
}
\usage{juul}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{age}}{
      a numeric vector (years).
    }
    \item{\code{menarche}}{
      a numeric vector. Has menarche occurred (code 1: no, 2: yes)?
    }
    \item{\code{sex}}{
      a numeric vector (1: boy, 2: girl).
    }
    \item{\code{igf1}}{
      a numeric vector, insulin-like growth factor (\eqn{\mu}g/l).
    }
    \item{\code{tanner}}{
      a numeric vector, codes 1--5: Stages of puberty ad modum Tanner.
    }
    \item{\code{testvol}}{
      a numeric vector, testicular volume (ml).
    }
  }
}
\source{
 Original data.
}
\examples{
plot(igf1~age, data=juul)
}
\keyword{datasets}
