\name{ashina}
\alias{ashina}
\title{Ashina's crossover trial}
\description{
  The \code{ashina} data frame has 16 rows and 3 columns.
  It contains data from a crossover trial for the effect of an NO
  synthase inhibitor on
  headaches. Visual analog scale recordings of pain level were made at
  baseline and at five time points after infusion of the drug or
  placebo. A score
  was calculated as the sum of the differences from baseline. All
  patients received both treatment and placebo in randomized order.
}
\usage{ashina}
\format{
  This data frame contains the following columns:
  \describe{
    \item{vas.active}{ 
      a numeric vector. Summary score when given active substance.
    }
    \item{vas.plac}{
      a numeric vector. Summary score when given placebo treatment.
    }
    \item{grp}{
      a numeric vector code. 1: placebo first, 2: active first.
    }
  }
}
\source{
  Original data
}
\references{
  M.Ashina et al. (1999), \emph{Lancet} 353, pp. 287--289 
}
\examples{
plot(vas.active~vas.plac,pch=grp,data=ashina)
abline(0,1)
}
\keyword{datasets}
