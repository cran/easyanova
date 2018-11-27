\name{data3}
\alias{data3}
\docType{data}
\title{
data3: Kaps and Lamberson (2009): page 347
}
\description{
Latin square design for test four different treatments on hay intake of fattening steers
}
\usage{data(data3)}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{treatment}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{period}}{a factor with levels \code{p1} \code{p2} \code{p3} \code{p4}}
    \item{\code{steer}}{a factor with levels \code{a1} \code{a2} \code{a3} \code{a4}}
    \item{\code{response}}{a numeric vector}
  }
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\examples{
data(data3)
summary(data3)
}

