\name{zelazo}
\alias{zelazo}
\non_function{}
\title{Age at walking}
\usage{data(zelazo)}
\description{The \code{zelazo} object is a list with four components.} 
\format{
  This is a list containing data on age at
  walking (in months) for four groups of infants: 
\describe{
\item{active}{test group receiving active training. These children had
  their walking and placing reflexes
  trained during four three-minute sessions that took place every day
  from their second to their eighth week of life.}
\item{passive}{passive training group. Received the same types of
  social and gross motor stimulation, 
  but did not have their specific walking and placing reflexes
  trained. }
\item{none}{no training.  Had no special training, but
  were tested along with the children who underwent active or passive
  training.}
\item{ctr.8w}{eigth-week controls. Had no training and were only
  tested at the age of 8 weeks. }
}
}
\source{P.R.~Zelazo, N.A.~Zelazo, and S.~Kolb (1972),
  ``Walking'' in the newborn, \emph{Science}, 176, 314--315.
}
\note{When asked to enter these data from a text source, many
students will use one vector per group and will need to reformat data
into a data frame for some uses. The rather unusual format of this data set
mimics that situation. 
}
\examples{
data(zelazo)
}
\keyword{datasets}



