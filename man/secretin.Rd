\name{secretin}
\alias{secretin}
\non_function{}
\title{Secretin-induced blood glucose changes}
\usage{data(secretin)}
\description{
The \code{secretin} data frame has 50 rows and 6 columns. It contains
data from a glucose response experiment.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{gluc}{a numeric vector. Blood glucose level.}
    \item{person}{a factor with levels \code{A}--\code{E}.}
    \item{time}{a factor with levels \code{20}, \code{30}, \code{60}, \code{90} 
      (minutes since injection), and \code{pre} (before injection).}
    \item{repl}{a factor with levels
      {\code{a}: 1st sample} and
      {\code{b}: 2nd sample}.}
    \item{time20plus}{a factor with levels
      {\code{20+}: 20 minutes or longer since injection} and
      {\code{pre}: before injection.}}
    \item{time.comb}{a factor with levels
      {\code{20}:  20 minutes since injection},
      {\code{30+}: 30 minutes or longer since injection}, and
      {\code{pre}: before injection.  }}
  }
}
\details{
 Secretin is a hormone of the duodenal mucous membrane. An extract was
 administered to five patients with arterial hypertension. Primary
 registrations (double determination) of blood glucose were on graph
 paper, later quantified with the smallest of the two measurements
 recorded first.}
 
\source{
 \emph{Exercises in Applied statistics} (1977), Exercise~5.8, Dept.\ of
 Theoretical Statistics,  
 Aarhus University.
}
\examples{
data(secretin)
}
\keyword{datasets}
