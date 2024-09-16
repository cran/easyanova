\name{data13}
\alias{data13}
\docType{data}
\title{
data13: Cruz and Carneiro (2006): page 575
}
\description{
Incomplete block design
}
\usage{data(data13)}
\format{
  A data frame with 23 observations on the following 3 variables.
  \describe{
    \item{\code{genotypes}}{a factor with levels \code{f1} \code{f10} \code{f11} \code{f12} \code{f13} \code{f14} \code{f2} \code{f3} \code{f4} \code{f5} \code{f6} \code{f7} \code{f8} \code{f9} \code{test1} \code{test2} \code{test3}}
    \item{\code{blocks}}{a factor with levels \code{b1} \code{b2} \code{b3}}
    \item{\code{yield}}{a numeric vector}
  }
}
\references{
CRUZ, C.D. and CARNEIRO, P.C.S. Modelos biometricos aplicados ao melhoramento genetico. 2nd Edition. Vicosa, UFV, v.2, 2006. 585p.
}
\examples{
data(data13)
summary(data13)
}

