\name{data2}
\alias{data2}
\docType{data}
\title{
data2: Kaps and Lamberson (2009): page 313: randomizad block design
}
\description{
Complete randomized block design to determine the average daily gain of steers
}
\usage{data(data2)}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{Treatments}}{a factor with levels \code{t1} \code{t2} \code{t3}}
    \item{\code{Blocks}}{a factor with levels \code{b1} \code{b2} \code{b3} \code{b4}}
    \item{\code{Gain}}{a numeric vector}
  }
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\examples{
data(data2)
summary(data2)
}

