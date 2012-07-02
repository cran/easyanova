\name{data8}
\alias{data8}
\docType{data}
\title{
data8: Kaps and Lamberson (2009): page 386
}
\description{
Split-plot Design. Main Plots in Randomized Blocks. An experiment was conducted in order to investigate four different treatments of
pasture and two mineral supplements on milk yield. The total number of cows available was
24. The experiment was designed as a split-plot, with pasture treatments (factor A) assigned
to the main plots and mineral supplements (factor B) assigned to split-plots. The experiment
was replicated in three blocks. 
}
\usage{data(data8)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{pasture}}{a factor with levels \code{p1} \code{p2} \code{p3} \code{p4}}
    \item{\code{block}}{a numeric vector}
    \item{\code{mineral}}{a factor with levels \code{m1} \code{m2}}
    \item{\code{milk}}{a numeric vector}
  }
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\examples{
data(data8)
summary(data8)
}

