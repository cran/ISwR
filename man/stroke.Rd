\name{stroke}
\alias{stroke}
\docType{data}
\title{Estonian stroke data}
\description{
  All cases of stroke in Tartu, Estonia, during the period 1991--1993,
  with follow-up until January 1, 1996.
}
\usage{stroke}
\format{
  A data frame with 829 observations on the following 10 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{Female} and \code{Male}.}
    \item{\code{died}}{a Date, date of death.}
    \item{\code{dstr}}{a Date, date of stroke.}
    \item{\code{age}}{a numeric vector, age at stroke.}
    \item{\code{dgn}}{a factor, diagnosis, with levels \code{ICH}
      (intracranial haemorrhage), \code{ID} (unidentified). \code{INF}
      (infarction, ischaemic),  \code{SAH} (subarchnoid haemorrhage).}
    \item{\code{coma}}{a factor with levels \code{No} and \code{Yes},
      indicating whether patient was in coma after the stroke.}
    \item{\code{diab}}{a factor with levels \code{No} and \code{Yes},
      history of diabetes.}
    \item{\code{minf}}{a factor with levels \code{No} and \code{Yes},
      history of myocardial infarction.}
    \item{\code{han}}{a factor with levels \code{No} and \code{Yes}, history
    of hypertension.}
    \item{\code{obsmonths}}{a numeric vector, observation times in
    months (set to 0.1 for patients dying on the same day as the
    stroke).}
    \item{\code{dead}}{a logical vector, whether patient died during the
      study.}
  }
}
\source{
  Original data.
}
\references{
  J. Korv, M. Roose, and A.E. Kaasik (1997). Stroke Registry of Tartu,
    Estonia, from 1991 through 1993. Cerebrovascular Disorders 7:154--162.
}
\keyword{datasets}
