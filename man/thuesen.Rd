\name{thuesen}
\alias{thuesen}
\title{Ventricular shortening velocity}
\description{
The \code{thuesen} data frame has 24 rows and 2 columns.
It contains ventricular shortening velocity and blood glucose for type 1
diabetic patients.  
}
\usage{thuesen}
\format{
This data frame contains the following columns:
\describe{
\item{blood.glucose}{
a numeric vector. Fasting blood glucose (mmol/l).
}
\item{short.velocity}{ 
a numeric vector. Mean circumferential shortening velocity (\%/s).
}
}
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  11.6, Chapman & Hall.
}
\examples{

plot(short.velocity~blood.glucose, data=thuesen)
}
\keyword{datasets}
