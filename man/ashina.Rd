\name{ashina}
\alias{ashina}
\title{Ashina's crossover trial}

\description{ 

  The \code{ashina} data frame has 16 rows and 3 columns. It contains
  data from a crossover trial for the effect of an NO synthase inhibitor
  on headaches. Visual analog scale recordings of pain levels were made
  at baseline and at five time points after infusion of the drug or
  placebo. A score was calculated as the sum of the differences from
  baseline. Data were recorded during two sessions for each patient. Six
  patients were given treatment on the first occasion and the placebo on
  the second. Ten patients had placebo first and then treatment. The
  order of treatment and the placebo was randomized.

}
\usage{ashina}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{vas.active}}{ 
      a numeric vector, summary score when given active substance.
    }
    \item{\code{vas.plac}}{
      a numeric vector, summary score when given placebo treatment.
    }
    \item{\code{grp}}{
      a numeric vector code, 1: placebo first, 2: active first.
    }
  }
}
\source{
  Original data.
}
\references{
  M.Ashina et al. (1999), Effect of inhibition of nitric oxide
  synthase on chronic tension-type headache: a randomised crossover
  trial. \emph{Lancet} 353, 287--289
}
\examples{
plot(vas.active~vas.plac,pch=grp,data=ashina)
abline(0,1)
}
\keyword{datasets}
