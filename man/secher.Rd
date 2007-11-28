\name{secher}
\alias{secher}
\title{Birth weight and ultrasonography}
\description{
The \code{secher} data frame has 107 rows and 4 columns. It contains
ultrasonographic measurements of fetuses immediately before birth and
their subsequent 
birth weight.
}
\usage{secher}
\format{
This data frame contains the following columns:
\describe{
\item{\code{bwt}}{
a numeric vector, birth weight (g).
}
\item{\code{bpd}}{
a numeric vector, biparietal diameter (mm).
}
\item{\code{ad}}{
a numeric vector, abdominal diameter (mm).
}
\item{\code{no}}{
a numeric vector, observation number.
}
}
}
\source{
  D. Kronborg and L.T. Skovgaard (1990), \emph{Regressionsanalyse},
  Table 3.1, FADLs Forlag (in Danish).
  
  Secher et al. (1987), European Journal of Obstetrics, Gynecology, and
  Reproductive Biology, 24: 1--11.
}
\examples{
plot(bwt~ad, data=secher, log="xy")
}
\keyword{datasets}
