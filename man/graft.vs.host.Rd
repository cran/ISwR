\name{graft.vs.host}
\alias{graft.vs.host}
\title{Graft versus host disease}
\description{
  The \code{gvhd} data frame has 37 rows and 7 columns.
  It contains data from patients receiving a nondepleted allogenic bone
  marrow transplant, with the purpose of finding variables associated with
  the development of acute graft-versus-host disease.
}
\usage{graft.vs.host}
\format{
  This data frame contains the following columns:
  \describe{
    \item{pnr}{
      a numeric vector. Patient number.
    }
    \item{rcpage}{
      a numeric vector. Age of recipient (years).
    }
    \item{donage}{
      a numeric vector. Age of donor (years).
    }
    \item{type}{
      a numeric vector, type of leukaemia coded 1: AML, 2: ALL, 3: CML
      for acute myeloid, acute lymphatic, and chronic myeloid leukaemia. 
    }
    \item{preg}{
      a numeric vector code, indicating whether donor has been pregnant. 0: no, 1: yes.
    }
    \item{index}{
      a numeric vector  giving an index of mixed epidermal cell-lymphocyte
      reactions. 
    }
    \item{gvhd}{
      a numeric vector code, graft versus host disease. 0: no, 1: yes.
    }
    \item{time}{a numeric vector. Follow-up time}
    \item{dead}{a numeric vector code 0: no (censored), 1: yes}
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
