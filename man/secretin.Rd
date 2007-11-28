\name{secretin}
\alias{secretin}
\title{Secretin-induced blood glucose changes}
\description{
The \code{secretin} data frame has 50 rows and 6 columns. It contains
data from a glucose response experiment.
}
\usage{secretin}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{gluc}}{a numeric vector, blood glucose level.}
    \item{\code{person}}{a factor with levels \code{A}--\code{E}.}
    \item{\code{time}}{a factor with levels \code{20}, \code{30}, \code{60}, \code{90} 
      (minutes since injection), and \code{pre} (before injection).}
    \item{\code{repl}}{a factor with levels
      {\code{a}: 1st sample};
      {\code{b}: 2nd sample}.}
    \item{\code{time20plus}}{a factor with levels
      {\code{20+}: 20 minutes or longer since injection};
      {\code{pre}: before injection.}}
    \item{\code{time.comb}}{a factor with levels
      {\code{20}:  20 minutes since injection};
      {\code{30+}: 30 minutes or longer since injection};
      {\code{pre}: before injection.  }}
  }
}
\details{
 Secretin is a hormone of the duodenal mucous membrane. An extract was
 administered to five patients with arterial hypertension. Primary
 registrations (double determination) of blood glucose were on graph
 paper and later quantified with the smallest of the two measurements
 recorded first.}
 
\source{
 Anon. (1977), \emph{Exercises in Applied Statistics}, Exercise 5.8, Dept.\ of
 Theoretical Statistics,  
 Aarhus University.
}
\keyword{datasets}
