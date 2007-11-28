\name{heart.rate}
\alias{heart.rate}
\title{Heart rates after enalaprilat}
\description{
 The \code{heart.rate} data frame has 36 rows and 3 columns.
 It contains data for nine patients with congestive heart failure before
 and shortly after administration of enalaprilat, in a balanced two-way
 layout.
}
\usage{heart.rate}
\format{
  This data frame contains the following columns:
  \describe{
    \item{hr}{
      a numeric vector. Heart rate in beats per minute.
    }
    \item{subj}{
      a factor with levels
      \code{1} to \code{9}. 
    }
    \item{time}{
      a factor with levels
      \code{0} (before),
      \code{30}, 
      \code{60}, and 
      \code{120} (minutes after administration).
    }
  }
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research}, Table
  12.2, Chapman & Hall.
}
\examples{

evalq(interaction.plot(time,subj,hr), heart.rate)
}
\keyword{datasets}

