\name{cystfibr}
\alias{cystfibr}
\non_function{}
\title{Cystic fibrosis lung function data}
\description{
  The \code{cystfibr} data frame has 25 rows and 10 columns.
  It contains lung function data for cystic fibrosis patients (7--23 years
  old)
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age}{a numeric vector. Age in years.}
    \item{sex}{a numeric vector code. 0: male, 1:female.}
    \item{height}{a numeric vector. Height (cm).}
    \item{weight}{a numeric vector. Weight (kg).}
    \item{bmp}{a numeric vector. Body mass (\% of normal).}
    \item{fev1}{a numeric vector. Forced expiratory volume.}
    \item{rv}{a numeric vector. Residual volume.}
    \item{frc}{a numeric vector. Functional residual capacity.}
    \item{tlc}{a numeric vector. Total lung capacity.}
    \item{pemax}{a numeric vector. Maximum expiratory pressure.}
  }
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  12.11, Chapman & Hall.
}
\references{
  O'Neill et al. (1983) (full reference in Altman).
}
\keyword{datasets}
