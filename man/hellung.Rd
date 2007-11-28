\name{hellung}
\alias{hellung}
\title{Growth of Tetrahymena cells}
\description{
  The \code{hellung} data frame has 51 rows and 3 columns.
  diameter and concentration of \emph{Tetrahymena} cells with and without
  glucose added to growth medium.
}
\usage{hellung}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{glucose}}{
      a numeric vector code, 1: yes, 2: no. 
    }
    \item{\code{conc}}{
      a numeric vector, cell concentration (counts/ml).
    }
    \item{\code{diameter}}{
      a numeric vector, cell diameter (\eqn{\mu}m).
    }
  }
}
\source{
  D. Kronborg and L.T. Skovgaard (1990), \emph{Regressionsanalyse},
  Table 1.1, FADLs Forlag (in Danish).
}
\examples{
plot(diameter~conc,pch=glucose,log="xy",data=hellung)
}
\keyword{datasets}
