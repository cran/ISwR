\name{juul2}
\alias{juul2}
\title{Juul's IGF data, extended version}
\description{
  The \code{juul2} data frame has 1339 rows and 8 columns.
  Extended version of |juul|.
}
\usage{juul2}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age}{
      a numeric vector (years).
    }
    \item{height}{
      a numeric vector (cm).
    }
    \item{menarche}{
      a numeric vector. Has menarche occurred (code 1: no, 2: yes)?
    }
    \item{sex}{
      a numeric vector (1: boy, 2: girl).
    }
    \item{igf1}{
      a numeric vector. Insulin-like growth factor ($\mu$g/l).
    }
    \item{tanner}{
      a numeric vector. Codes 1--5: Stages of puberty a.m. Tanner.
    }
    \item{testvol}{
      a numeric vector. Testicular volume (ml).
    }
    \item{weight}{
      a numeric vector. Weight (kg).
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
