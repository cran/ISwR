\name{melanom}
\alias{melanom}
\non_function{}
\title{Survival after malignant melanoma}
\description{
  The \code{melanom} data frame has 205 rows and 7 columns.
  It contains data relating to survival of patients after operation for
  malignant melanoma collected at Odense University Hospital by K.T.
  Drzewiecki. 
}
\format{
This data frame contains the following columns:
\describe{
\item{no}{
a numeric vector. Patient code.
}
\item{status}{
a numeric vector code. Survival status. 1: dead from melanoma, 2: alive,
3: dead from other cause.
}
\item{days}{
a numeric vector. Observation time.
}
\item{ulc}{
a numeric vector code. Ulceration, 1: present, 2: absent.
}
\item{thick}{
a numeric vector. Tumor thickness (1/100 mm).
}
\item{sex}{
a numeric vector code. 1: female, 2: male.
}
}
}
\source{
  P.K. Andersen, {\O}. Borgan, R.D. Gill, N. Keiding (1991),
  \emph{Statistical Models Based on 
  Counting Processes}, Appendix 1, Springer-Verlag. 
}
\examples{
data(melanom)
require(survival)
plot(survfit(Surv(days,status==1),data=melanom))
}
\keyword{datasets}
