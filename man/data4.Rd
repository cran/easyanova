\name{data4}
\alias{data4}
\docType{data}
\title{
data4: Kaps and Lamberson (2009): page 349
}
\description{
Two latin squares design for test four different treatments on hay intake of fattening steers
}
\usage{data(data4)}
\format{
  A data frame with 32 observations on the following 5 variables.
  \describe{
    \item{\code{diet}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{square}}{a numeric vector}
    \item{\code{steer}}{a numeric vector}
    \item{\code{period}}{a numeric vector}
    \item{\code{response}}{a numeric vector}
  }
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\examples{
data(data4)
summary(data4)
}

