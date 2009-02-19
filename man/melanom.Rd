\name{melanom}
\alias{melanom}
\encoding{UTF-8}
\title{Survival after malignant melanoma}
\description{
  The \code{melanom} data frame has 205 rows and 7 columns.
  It contains data relating to the survival of patients after an operation for
  malignant melanoma, collected at Odense University Hospital by K.T.
  Drzewiecki. 
}
\usage{melanom}
\format{
This data frame contains the following columns:
\describe{
\item{\code{no}}{
a numeric vector, patient code.
}
\item{\code{status}}{
a numeric vector code, survival status; 1: dead from melanoma, 2: alive,
3: dead from other cause.
}
\item{\code{days}}{
a numeric vector, observation time.
}
\item{\code{ulc}}{
a numeric vector code, ulceration; 1: present, 2: absent.
}
\item{\code{thick}}{
a numeric vector, tumor thickness (1/100 mm).
}
\item{\code{sex}}{
a numeric vector code; 1: female, 2: male.
}
}
}
\source{
  P.K. Andersen, \enc{Ø}{O}. Borgan, R.D. Gill, and N. Keiding (1991),
  \emph{Statistical Models Based on 
  Counting Processes}, Appendix 1, Springer-Verlag. 
}
\examples{
require(survival)
plot(survfit(Surv(days,status==1),data=melanom))
}
\keyword{datasets}
