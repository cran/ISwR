\name{fake.trypsin}
\alias{fake.trypsin}
\title{ Trypsin by age groups }
\description{
  The \code{trypsin} data frame has 271 rows and 3 columns.
  Serum levels of immunoreactive trypsin in healthy volunteers (faked!).
}
\usage{fake.trypsin}
\format{
  This data frame contains the following columns:
  \describe{
    \item{trypsin}{
      a numeric vector. Serum-trypsin in ng/ml.
    }
    \item{grp}{
      a numeric vector. Age coding. See below.
    }
    \item{grpf}{
      a factor with levels 
      \code{1}: age 10--19, 
      \code{2}: age 20--29,
      \code{3}: age 30--39,
      \code{4}: age 40--49,
      \code{5}: age 50--59, and
      \code{6}: age 60--69.
    }
  }
}
\details{
  Data have been simulated to match given group means and SD.
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  9.12, Chapman & Hall.
}
\examples{
plot(trypsin~grp, data=fake.trypsin)
}
\keyword{datasets}
