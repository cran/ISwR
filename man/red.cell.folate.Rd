\name{red.cell.folate}
\alias{red.cell.folate}
\non_function{}
\title{Red cell folate data}
\description{
  The \code{folate} data frame has 22 rows and 2 columns.
  It contains data on red cell folate levels in patients receiving three
  different methods of ventilation during  anesthesia.
}
\format{
This data frame contains the following columns:
\describe{
  \item{folate}{
    a numeric vector. Folate concentration ($\mu$g/l).
  }
  \item{ventilation}{
    a factor with levels
    \code{N2O+O2,24h}: 50\% nitrous oxide and 50\% oxygen, continuously for
    24~hours; 
    \code{N2O+O2,op}:  50\% nitrous oxide and 50\% oxygen, only during operation;
    \code{O2,24h}: no nitrous oxide, but 35--50\% oxygen for 24~hours.
}
}
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  9.10, Chapman & Hall.
}
\examples{
data(red.cell.folate)
plot(folate~ventilation,data=red.cell.folate)
}
\keyword{datasets}
