\name{data7}
\alias{data7}
\docType{data}
\title{
data7: Kaps and Lamberson (2009): page 409
}
\description{
The aim of this experiment was to
test the difference between two treatments on gain of kids. A sample of 18 kids was chosen,
nine  for each treatment. One kid in treatment 1 was removed from the experiment
due to illness. The experiment began at the age of 8 weeks. Weekly gain was measured at
ages 9, 10, 11 and 12 weeks. 
}
\usage{data(data7)}
\format{
  A data frame with 68 observations on the following 4 variables.
  \describe{
    \item{\code{treatment}}{a character vector}
    \item{\code{rep}}{a numeric vector}
    \item{\code{week}}{a character vector}
    \item{\code{gain}}{a numeric vector}
  }
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\examples{
data(data7)
summary(data7)
}

