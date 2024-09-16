\name{data10}
\alias{data10}
\docType{data}
\title{
data10: Kaps and Lamberson (2009): page 395
}
\description{
Completely randomized design with a covariate.The effect of three diets on daily gain of steers was investigated. The design was
a completely randomized design. Weight at the beginning of the experiment (initial weight)
was recorded, but not used in the assignment of animals to diet. 
}
\usage{data(data10)}
\format{
  A data frame with 15 observations on the following 4 variables.
  \describe{
    \item{\code{Diets}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{Initial_weight}}{a numeric vector}
    \item{\code{Repetitions}}{a numeric vector}
    \item{\code{Gain}}{a numeric vector}
  }
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\examples{
data(data10)
summary(data10)
}

