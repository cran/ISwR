\name{secher}
\alias{secher}
\non_function{}
\title{Birth weight and ultrasonography}
\description{
The \code{secher} data frame has 107 rows and 4 columns. It contains
ultrasonographic measurements of fetuses immediately before birth and
subsequent 
birth weight.
}
\format{
This data frame contains the following columns:
\describe{
\item{bwt}{
a numeric vector. Birth weight (g).
}
\item{bpd}{
a numeric vector. Biparietal diameter (mm).
}
\item{ad}{
a numeric vector. Abdominal diameter (mm).
}
\item{no}{
a numeric vector. Observation number.
}
}
}
\source{
  D. Kronborg and L.T. Skovgaard (1990), \emph{Regressionsanalyse},
  Table~3.1, FADLs Forlag (in Danish).

  Secher et al. (1987), Eur.j.obs.gyn.repr.biol., 24, 1--11.
}
\examples{
data(secher)
plot(bwt~ad, data=secher, log="xy")
}
\keyword{datasets}
