\name{wright}
\alias{wright}
\non_function{}
\title{Comparison of Wright peak-flow meters}
\description{
The \code{wright} data frame has 17 rows and 2 columns.
It contains data on peak expiratory flow rate with two different flow
meters on each of 17 subjects. 
}
\format{
This data frame contains the following columns:
\describe{
\item{std.wright}{
a numeric vector. Data from large flow meter (l/min).
}
\item{mini.wright}{
a numeric vector. Data from mini flow meter (l/min).
}
}
}
\source{
J.M. Bland and D.G. Altman (1986), \emph{Lancet}, pp. 307--310.
}
\examples{
data(wright)
plot(wright)
abline(0,1)
}
\keyword{datasets}
