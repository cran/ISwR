\name{juul2}
\alias{juul2}
\title{Juul's IGF data, extended version}
\description{
  The \code{juul2} data frame has 1339 rows and 8 columns;
  extended version of |juul|.
}
\usage{juul2}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{age}}{
      a numeric vector (years).
    }
    \item{\code{height}}{
      a numeric vector (cm).
    }
    \item{\code{menarche}}{
      a numeric vector. Has menarche occurred (code 1: no, 2: yes)?
    }
    \item{\code{sex}}{
      a numeric vector (1: boy, 2: girl).
    }
    \item{\code{igf1}}{
      a numeric vector, insulin-like growth factor
      (\eqn{\mu\mathrm{g}/\mathrm{l}}{microgram per liter}).
    }
    \item{\code{tanner}}{
      a numeric vector, codes 1--5: Stages of puberty ad modum Tanner.
    }
    \item{\code{testvol}}{
      a numeric vector, testicular volume (ml).
    }
    \item{\code{weight}}{
      a numeric vector, weight (kg).
    }
  }
}
\source{
 Original data.
}
\examples{
plot(igf1~age, data=juul2)
}
\keyword{datasets}
