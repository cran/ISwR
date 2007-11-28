\name{graft.vs.host}
\alias{graft.vs.host}
\title{Graft versus host disease}
\description{
  The \code{gvhd} data frame has 37 rows and 7 columns.
  It contains data from patients receiving a nondepleted allogenic bone
  marrow transplant with the purpose of finding variables associated with
  the development of acute graft-versus-host disease.
}
\usage{graft.vs.host}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{pnr}}{
      a numeric vector patient number.
    }
    \item{\code{rcpage}}{
      a numeric vector, age of recipient (years).
    }
    \item{\code{donage}}{
      a numeric vector, age of donor (years).
    }
    \item{\code{type}}{
      a numeric vector, type of leukaemia coded 1: AML, 2: ALL, 3: CML
      for acute myeloid, acute lymphatic, and chronic myeloid leukaemia. 
    }
    \item{\code{preg}}{
      a numeric vector code indicating whether donor has been pregnant. 0: no, 1: yes.
    }
    \item{\code{index}}{
      a numeric vector  giving an index of mixed epidermal cell-lymphocyte
      reactions. 
    }
    \item{\code{gvhd}}{
      a numeric vector code, graft-versus-host disease, 0: no, 1: yes.
    }
    \item{\code{time}}{a numeric vector, follow-up time}
    \item{\code{dead}}{a numeric vector code, 0: no (censored), 1: yes}
  }
}
\source{
  D.G. Altman (1991), \emph{Practical Statistics for Medical Research},
  Exercise 12.3, Chapman & Hall.
}
\examples{
plot(jitter(gvhd,0.2)~index,data=graft.vs.host)
}
\keyword{datasets}
